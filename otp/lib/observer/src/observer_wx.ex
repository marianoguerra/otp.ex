defmodule :m_observer_wx do
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
    frame: :undefined,
    menubar: :undefined,
    menus: [],
    status_bar: :undefined,
    notebook: :undefined,
    main_panel: :undefined,
    panels: :undefined,
    active_tab: :undefined,
    node: :undefined,
    nodes: :undefined,
    prev_node: '',
    log: false,
    reply_to: false,
    config: :undefined
  )

  def start() do
    case :wx_object.start(:observer_wx, [], []) do
      err = {:error, _} ->
        err

      _Obj ->
        :ok
    end
  end

  def stop() do
    :wx_object.call(:observer, :stop)
  end

  def create_menus(object, menus) when is_list(menus) do
    :wx_object.call(object, {:create_menus, menus})
  end

  def get_attrib(what) do
    :wx_object.call(:observer, {:get_attrib, what})
  end

  def set_status(what) do
    :wx_object.cast(:observer, {:status_bar, what})
  end

  def get_tracer() do
    :wx_object.call(:observer, :get_tracer)
  end

  def get_active_node() do
    :wx_object.call(:observer, :get_active_node)
  end

  def get_menubar() do
    :wx_object.call(:observer, :get_menubar)
  end

  def get_scale() do
    scaleStr = :os.getenv('OBSERVER_SCALE', '1')

    try do
      :erlang.list_to_integer(scaleStr)
    catch
      _, _ ->
        1
    else
      scale when scale < 1 ->
        1

      scale ->
        scale
    end
  end

  def init(_Args) do
    :erlang.register(:observer, self())
    :wx.new()

    try do
      :wxSystemOptions.setOption('mac.listctrl.always_use_generic', 1)
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end

    scale = get_scale()

    frame =
      :wxFrame.new(:wx.null(), -1, 'Observer', [
        {:size, {scale * 850, scale * 600}},
        {:style, 2048 ||| 64 ||| 1024 ||| 512 ||| 4096 ||| 536_870_912 ||| 4_194_304}
      ])

    iconFile = :filename.join(:code.priv_dir(:observer), 'erlang_observer.png')
    icon = :wxIcon.new(iconFile, [{:type, 2 + 13}])
    :wxFrame.setIcon(frame, icon)
    :wxIcon.destroy(icon)
    state = r_state(frame: frame)
    updState = setup(state)
    :net_kernel.monitor_nodes(true)
    :erlang.process_flag(:trap_exit, true)
    {frame, updState}
  end

  defp setup(r_state(frame: frame) = state) do
    config = load_config()

    cnf = fn who ->
      :proplists.get_value(who, config, %{})
    end

    menuBar = :wxMenuBar.new()
    {nodes, nodeMenus} = get_nodes()
    defMenus = default_menus(nodeMenus)
    :observer_lib.create_menus(defMenus, menuBar, :default)
    :wxFrame.setMenuBar(frame, menuBar)
    panel = :wxPanel.new(frame, [])
    notebook = :wxNotebook.new(panel, 3, [{:style, 0}])
    sysPanel = :observer_sys_wx.start_link(notebook, self(), cnf.(:sys_panel))
    :wxNotebook.addPage(notebook, sysPanel, 'System', [])
    mainSizer = :wxBoxSizer.new(8)
    :wxSizer.add(mainSizer, notebook, [{:proportion, 1}, {:flag, 8192}])
    :wxPanel.setSizer(panel, mainSizer)
    statusBar = :wxStatusBar.new(frame)
    :wxFrame.setStatusBar(frame, statusBar)
    :wxFrame.setTitle(frame, :erlang.atom_to_list(node()))

    :wxStatusBar.setStatusText(
      statusBar,
      :erlang.atom_to_list(node())
    )

    :wxNotebook.connect(
      notebook,
      :command_notebook_page_changed,
      [{:skip, true}, {:id, 3}]
    )

    :wxFrame.connect(frame, :close_window, [])
    :wxMenu.connect(frame, :command_menu_selected)
    :wxFrame.show(frame)

    doFreeze =
      [:wxe_util.get_const(:wxMAJOR_VERSION), :wxe_util.get_const(:wxMINOR_VERSION)] < [2, 9] or
        :erlang.element(
          1,
          :os.type()
        ) === :win32

    doFreeze and :wxWindow.freeze(panel)
    perfPanel = :observer_perf_wx.start_link(notebook, self(), cnf.(:perf_panel))
    :wxNotebook.addPage(notebook, perfPanel, 'Load Charts', [])
    allcPanel = :observer_alloc_wx.start_link(notebook, self(), cnf.(:allc_panel))
    :wxNotebook.addPage(notebook, allcPanel, 'Memory Allocators', [])
    appPanel = :observer_app_wx.start_link(notebook, self(), cnf.(:app_panel))
    :wxNotebook.addPage(notebook, appPanel, 'Applications', [])
    proPanel = :observer_pro_wx.start_link(notebook, self(), cnf.(:pro_panel))
    :wxNotebook.addPage(notebook, proPanel, 'Processes', [])
    portPanel = :observer_port_wx.start_link(notebook, self(), cnf.(:port_panel))
    :wxNotebook.addPage(notebook, portPanel, 'Ports', [])
    tVPanel = :observer_tv_wx.start_link(notebook, self(), cnf.(:tv_panel))
    :wxNotebook.addPage(notebook, tVPanel, 'Table Viewer', [])
    tracePanel = :observer_trace_wx.start_link(notebook, self(), cnf.(:trace_panel))
    :wxNotebook.addPage(notebook, tracePanel, 'Trace Overview', [])
    :wxWindow.refresh(panel)
    doFreeze and :wxWindow.thaw(panel)
    :wxFrame.raise(frame)
    :wxFrame.setFocus(frame)
    sysPid = :wx_object.get_pid(sysPanel)
    send(sysPid, {:active, node()})

    panels = [
      {:sys_panel, sysPanel, 'System'},
      {:perf_panel, perfPanel, 'Load Charts'},
      {:allc_panel, allcPanel, 'Memory Allocators'},
      {:app_panel, appPanel, 'Applications'},
      {:pro_panel, proPanel, 'Processes'},
      {:port_panel, portPanel, 'Ports'},
      {:tv_panel, tVPanel, 'Table Viewer'},
      {:trace_panel, tracePanel, 'Trace Overview'}
    ]

    updState =
      r_state(state,
        main_panel: panel,
        notebook: notebook,
        menubar: menuBar,
        status_bar: statusBar,
        active_tab: sysPid,
        panels: panels,
        node: node(),
        nodes: nodes
      )

    sysFont = :wxSystemSettings.getFont(16)

    fixed =
      case :wxFont.isFixedWidth(sysFont) do
        true ->
          sysFont

        false ->
          sysFontSize = :wxFont.getPointSize(sysFont)
          :wxFont.new(sysFontSize, 75, 90, :wxe_util.get_const(:wxFONTWEIGHT_NORMAL))
      end

    :erlang.put({:font, :fixed}, fixed)
    updState
  end

  def handle_event(
        r_wx(
          event:
            r_wxNotebook(
              type: :command_notebook_page_changed,
              nSel: next
            )
        ),
        r_state(active_tab: previous, node: node, panels: panels, status_bar: sB) = state
      ) do
    {_, obj, _} = :lists.nth(next + 1, panels)

    case :wx_object.get_pid(obj) do
      ^previous ->
        {:noreply, state}

      pid ->
        :wxStatusBar.setStatusText(sB, '')
        send(previous, :not_active)
        send(pid, {:active, node})
        {:noreply, r_state(state, active_tab: pid)}
    end
  end

  def handle_event(
        r_wx(id: 4, event: r_wxCommand(type: :command_menu_selected)),
        state
      ) do
    spawn(:crashdump_viewer, :start, [])
    {:noreply, state}
  end

  def handle_event(r_wx(event: r_wxClose()), state) do
    stop_servers(state)
    {:noreply, state}
  end

  def handle_event(
        r_wx(
          id: 5006,
          event: r_wxCommand(type: :command_menu_selected)
        ),
        state
      ) do
    stop_servers(state)
    {:noreply, state}
  end

  def handle_event(
        r_wx(
          id: 5009,
          event: r_wxCommand(type: :command_menu_selected)
        ),
        state
      ) do
    external = 'http://www.erlang.org/doc/apps/observer/index.html'
    internal = :filename.join([:code.lib_dir(:observer), 'doc', 'html', 'index.html'])

    help =
      case :filelib.is_file(internal) do
        true ->
          internal

        false ->
          external
      end

    :wx_misc.launchDefaultBrowser(help) or
      create_txt_dialog(
        r_state(state, :frame),
        'Could not launch browser: ~n ' ++ help,
        'Error',
        512
      )

    {:noreply, state}
  end

  def handle_event(
        r_wx(
          id: 5014,
          event: r_wxCommand(type: :command_menu_selected)
        ),
        state = r_state(frame: frame)
      ) do
    aboutString =
      'Observe an erlang system\nAuthors: Olle Mattson & Magnus Eriksson & Dan Gudmundsson'

    style = [{:style, 4 ||| 32768}, {:caption, 'About'}]
    :wxMessageDialog.showModal(:wxMessageDialog.new(frame, aboutString, style))
    {:noreply, state}
  end

  def handle_event(
        r_wx(id: 2, event: r_wxCommand(type: :command_menu_selected)),
        r_state(frame: frame) = state
      ) do
    updState =
      case create_connect_dialog(
             :connect,
             state
           ) do
        :cancel ->
          state

        {:value, [], _, _} ->
          create_txt_dialog(frame, 'Node must have a name', 'Error', 512)
          state

        {:value, nodeName, longOrShort, cookie} ->
          try do
            case connect(
                   :erlang.list_to_atom(nodeName),
                   longOrShort,
                   :erlang.list_to_atom(cookie)
                 ) do
              {:ok, :set_cookie} ->
                change_node_view(node(), state)

              {:error, :set_cookie} ->
                create_txt_dialog(frame, 'Could not set cookie', 'Error', 512)
                state

              {:error, :net_kernel, _Reason} ->
                create_txt_dialog(frame, 'Could not enable node', 'Error', 512)
                state
            end
          catch
            _, _ ->
              create_txt_dialog(frame, 'Could not enable node', 'Error', 512)
              state
          end
      end

    {:noreply, updState}
  end

  def handle_event(
        r_wx(id: 1, event: r_wxCommand(type: :command_menu_selected)),
        r_state(frame: frame) = state
      ) do
    updState =
      case create_connect_dialog(
             :ping,
             state
           ) do
        :cancel ->
          state

        {:value, value} when is_list(value) ->
          try do
            node = :erlang.list_to_atom(value)

            case :net_adm.ping(node) do
              :pang ->
                create_txt_dialog(frame, 'Connect failed', 'Pang', 256)
                r_state(state, prev_node: value)

              :pong ->
                state1 = change_node_view(node, state)
                r_state(state1, prev_node: value)
            end
          catch
            _, _ ->
              create_txt_dialog(frame, 'Connect failed', 'Pang', 256)
              r_state(state, prev_node: value)
          end
      end

    {:noreply, updState}
  end

  def handle_event(
        r_wx(id: 5, event: r_wxCommand(type: :command_menu_selected)),
        r_state(frame: frame, log: prevLog, node: node) = state
      ) do
    try do
      :ok = ensure_sasl_started(node)
      :ok = ensure_mf_h_handler_used(node)
      :ok = ensure_rb_mode(node, prevLog)

      case prevLog do
        false ->
          :rpc.block_call(node, :rb, :start, [])
          set_status('Observer - ' ++ :erlang.atom_to_list(node) ++ ' (rb_server started)')
          {:noreply, r_state(state, log: true)}

        true ->
          :rpc.block_call(node, :rb, :stop, [])
          set_status('Observer - ' ++ :erlang.atom_to_list(node) ++ ' (rb_server stopped)')
          {:noreply, r_state(state, log: false)}
      end
    catch
      reason ->
        create_txt_dialog(frame, reason, 'Log view status', 512)
        {:noreply, state}
    end
  end

  def handle_event(
        r_wx(
          id: id,
          event: r_wxCommand(type: :command_menu_selected)
        ),
        r_state(nodes: ns, node: prevNode, log: prevLog) = state
      )
      when id > 1000 and id < 2000 do
    node = :lists.nth(id - 1000, ns)

    lState =
      case prevLog do
        true ->
          case node == prevNode do
            false ->
              :rpc.block_call(prevNode, :rb, :stop, [])
              r_state(state, log: false)

            true ->
              state
          end

        false ->
          state
      end

    {:noreply, change_node_view(node, lState)}
  end

  def handle_event(event, r_state(active_tab: pid) = state) do
    send(pid, event)
    {:noreply, state}
  end

  def handle_cast({:status_bar, msg}, state = r_state(status_bar: sB)) do
    :wxStatusBar.setStatusText(sB, msg)
    {:noreply, state}
  end

  def handle_cast(_Cast, state) do
    {:noreply, state}
  end

  def handle_call(
        {:create_menus, tabMenus},
        _From,
        state = r_state(menubar: menuBar, menus: prevTabMenus)
      ) do
    cond do
      tabMenus == prevTabMenus ->
        :ignore

      true ->
        :wx.batch(fn ->
          clean_menus(prevTabMenus, menuBar)
          :observer_lib.create_menus(tabMenus, menuBar, :plugin)
        end)
    end

    {:reply, :ok, r_state(state, menus: tabMenus)}
  end

  def handle_call({:get_attrib, attrib}, _From, state) do
    {:reply, :erlang.get(attrib), state}
  end

  def handle_call(:get_tracer, _From, state = r_state(panels: panels)) do
    {_, traceP, _} = :lists.keyfind(:trace_panel, 1, panels)
    {:reply, traceP, state}
  end

  def handle_call(:get_active_node, _From, state = r_state(node: node)) do
    {:reply, node, state}
  end

  def handle_call(:get_menubar, _From, state = r_state(menubar: menuBar)) do
    {:reply, menuBar, state}
  end

  def handle_call(:stop, from, state) do
    stop_servers(state)
    {:noreply, r_state(state, reply_to: from)}
  end

  def handle_call(:log_status, _From, state) do
    {:reply, r_state(state, :log), state}
  end

  def handle_call(_Msg, _From, state) do
    {:reply, :ok, state}
  end

  def handle_info({:nodeup, _Node}, state) do
    state2 = update_node_list(state)
    {:noreply, state2}
  end

  def handle_info({:nodedown, node}, r_state(frame: frame) = state) do
    state2 =
      case node === r_state(state, :node) do
        true ->
          change_node_view(node(), state)

        false ->
          state
      end

    state3 = update_node_list(state2)
    msg = ['Node down: ' | :erlang.atom_to_list(node)]
    create_txt_dialog(frame, msg, 'Node down', 256)
    {:noreply, state3}
  end

  def handle_info(
        {:open_link, id0},
        state = r_state(panels: panels, frame: frame)
      ) do
    id =
      case id0 do
        [_ | _] ->
          try do
            :erlang.list_to_pid(id0)
          catch
            _, _ ->
              id0
          end

        _ ->
          id0
      end

    case id do
      pid when is_pid(pid) ->
        {:pro_panel, procViewer, _} = :lists.keyfind(:pro_panel, 1, panels)
        send(:wx_object.get_pid(procViewer), {:procinfo_open, pid})

      '#Port' ++ _ = port ->
        {:port_panel, portViewer, _} = :lists.keyfind(:port_panel, 1, panels)
        send(:wx_object.get_pid(portViewer), {:portinfo_open, port})

      _ ->
        msg = :io_lib.format('Information about ~p is not available or implemented', [id])
        info = :wxMessageDialog.new(frame, msg)
        :wxMessageDialog.showModal(info)
        :wxMessageDialog.destroy(info)
    end

    {:noreply, state}
  end

  def handle_info(
        {:get_debug_info, from},
        state = r_state(notebook: notebook, active_tab: pid)
      ) do
    send(from, {:observer_debug, :wx.get_env(), notebook, pid})
    {:noreply, state}
  end

  def handle_info({:EXIT, pid, reason}, state) do
    case reason do
      :normal ->
        {:noreply, state}

      _ ->
        :io.format('Observer: Child (~s) crashed exiting:  ~p ~tp~n', [
          pid2panel(pid, state),
          pid,
          reason
        ])

        {:stop, :normal, state}
    end
  end

  def handle_info({:stop, me}, state) when me === self() do
    {:stop, :normal, state}
  end

  def handle_info(_Info, state) do
    {:noreply, state}
  end

  defp stop_servers(r_state(node: node, log: logOn, panels: panels) = _State) do
    logOn and :rpc.block_call(node, :rb, :stop, [])
    me = self()
    save_config(panels)

    stop = fn ->
      try do
        _ =
          for {_, panel, _} <- panels do
            :wx_object.stop(panel)
          end

        :ok
      catch
        _, _ ->
          :ok
      end

      send(me, {:stop, me})
    end

    spawn(stop)
  end

  def terminate(_Reason, r_state(frame: frame, reply_to: from)) do
    :wxFrame.destroy(frame)
    :wx.destroy()

    case from do
      false ->
        :ignore

      _ ->
        :gen_server.reply(from, :ok)
    end

    :ok
  end

  defp load_config() do
    case :file.consult(config_file()) do
      {:ok, config} ->
        config

      _ ->
        []
    end
  end

  defp save_config(panels) do
    configs =
      for {name, panel, _} <- panels do
        {name, :wx_object.call(panel, :get_config)}
      end

    file = config_file()

    case :filelib.ensure_dir(file) do
      :ok ->
        format =
          for conf <- configs do
            :io_lib.format('~tp.~n', [conf])
          end

        _ = :file.write_file(file, format)

      _ ->
        :ignore
    end
  end

  defp config_file() do
    dir = :filename.basedir(:user_config, 'erl_observer')
    :filename.join(dir, 'config.txt')
  end

  def code_change(_, _, state) do
    {:ok, state}
  end

  def try_rpc(node, mod, func, args) do
    case :rpc.call(node, mod, func, args) do
      {:badrpc, reason} ->
        :error_logger.error_report([
          {:node, node},
          {:call, {mod, func, args}},
          {:reason, {:badrpc, reason}}
        ])

        send(:observer, {:nodedown, node})
        :erlang.error({:badrpc, reason})

      res ->
        res
    end
  end

  def return_to_localnode(frame, node) do
    case node() !== node do
      true ->
        create_txt_dialog(frame, 'Error occured on remote node', 'Error', 512)
        :erlang.disconnect_node(node)

      false ->
        :ok
    end
  end

  def create_txt_dialog(frame, msg, title, style) do
    mD = :wxMessageDialog.new(frame, msg, [{:style, style}, {:caption, title}])
    :wxDialog.showModal(mD)
    :wxDialog.destroy(mD)
  end

  defp connect(nodeName, 0, cookie) do
    connect2(nodeName, :shortnames, cookie)
  end

  defp connect(nodeName, 1, cookie) do
    connect2(nodeName, :longnames, cookie)
  end

  defp connect2(nodeName, opts, cookie) do
    case :net_adm.names() do
      {:ok, _} ->
        :ok

      {:error, :address} ->
        epmd = :os.find_executable('epmd')
        :os.cmd(epmd)
    end

    case :net_kernel.start([nodeName, opts]) do
      {:ok, _} ->
        case :erlang.is_alive() do
          true ->
            :erlang.set_cookie(node(), cookie)
            {:ok, :set_cookie}

          false ->
            {:error, :set_cookie}
        end

      {:error, reason} ->
        {:error, :net_kernel, reason}
    end
  end

  defp change_node_view(node, r_state(active_tab: tab) = state) do
    send(tab, :not_active)
    send(tab, {:active, node})
    statusText = ['Observer - ' | :erlang.atom_to_list(node)]
    :wxFrame.setTitle(r_state(state, :frame), statusText)

    :wxStatusBar.setStatusText(
      r_state(state, :status_bar),
      statusText
    )

    r_state(state, node: node)
  end

  def check_page_title(notebook) do
    selection = :wxNotebook.getSelection(notebook)
    :wxNotebook.getPageText(notebook, selection)
  end

  defp pid2panel(pid, r_state(panels: panels)) do
    panelPids =
      for {name, obj, _} <- panels do
        {name, :wx_object.get_pid(obj)}
      end

    case :lists.keyfind(pid, 2, panelPids) do
      false ->
        'unknown'

      {name, _} ->
        name
    end
  end

  defp create_connect_dialog(:ping, r_state(frame: frame, prev_node: prev)) do
    dialog = :wxTextEntryDialog.new(frame, 'Connect to node', [{:value, prev}])

    case :wxDialog.showModal(dialog) do
      5100 ->
        value = :wxTextEntryDialog.getValue(dialog)
        :wxDialog.destroy(dialog)
        {:value, value}

      5101 ->
        :wxDialog.destroy(dialog)
        :cancel
    end
  end

  defp create_connect_dialog(:connect, r_state(frame: frame)) do
    dialog =
      :wxDialog.new(frame, -1, 'Distribute node', [
        {:style, 2048 ||| 64 ||| 1024 ||| 512 ||| 4096 ||| 536_870_912 ||| 4_194_304 ||| 64}
      ])

    vSizer = :wxBoxSizer.new(8)
    choices = ['Short name', 'Long name']

    radioBox =
      :wxRadioBox.new(dialog, 1, '', {-1, -1}, {-1, -1}, choices, [{:majorDim, 2}, {:style, 4}])

    nameText = :wxStaticText.new(dialog, -1, 'Node name: ')
    nameCtrl = :wxTextCtrl.new(dialog, -1, [{:size, {300, -1}}])
    :wxTextCtrl.setValue(nameCtrl, 'observer')
    cookieText = :wxStaticText.new(dialog, -1, 'Secret cookie: ')
    cookieCtrl = :wxTextCtrl.new(dialog, -1, [{:style, 2048}])
    btnSizer = :wxDialog.createButtonSizer(dialog, 4 ||| 16)
    dir = 16 ||| 32 ||| 128
    flags = [{:flag, 8192 ||| dir ||| 2048}, {:border, 5}]
    :wxSizer.add(vSizer, radioBox, flags)
    :wxSizer.addSpacer(vSizer, 10)
    :wxSizer.add(vSizer, nameText, [{:flag, 16}, {:border, 5}])
    :wxSizer.add(vSizer, nameCtrl, flags)
    :wxSizer.addSpacer(vSizer, 10)
    :wxSizer.add(vSizer, cookieText, [{:flag, 16}, {:border, 5}])
    :wxSizer.add(vSizer, cookieCtrl, flags)
    :wxSizer.addSpacer(vSizer, 10)

    :wxSizer.add(vSizer, btnSizer, [
      {:proportion, 1},
      {:flag, 8192 ||| (64 ||| 128 ||| 32 ||| 16)},
      {:border, 5}
    ])

    :wxWindow.setSizerAndFit(dialog, vSizer)
    :wxSizer.setSizeHints(vSizer, dialog)
    {:ok, [[homeDir]]} = :init.get_argument(:home)
    cookiePath = :filename.join(homeDir, '.erlang.cookie')

    defaultCookie =
      case :filelib.is_file(cookiePath) do
        true ->
          {:ok, bin} = :file.read_file(cookiePath)
          :erlang.binary_to_list(bin)

        false ->
          ''
      end

    :wxTextCtrl.setValue(cookieCtrl, defaultCookie)

    case :wxDialog.showModal(dialog) do
      5100 ->
        nameValue = :wxTextCtrl.getValue(nameCtrl)
        nameLngthValue = :wxRadioBox.getSelection(radioBox)
        cookieValue = :wxTextCtrl.getValue(cookieCtrl)
        :wxDialog.destroy(dialog)
        {:value, nameValue, nameLngthValue, cookieValue}

      5101 ->
        :wxDialog.destroy(dialog)
        :cancel
    end
  end

  defp default_menus(nodesMenuItems) do
    cDV = r_create_menu(id: 4, text: 'Examine Crashdump')
    quit = r_create_menu(id: 5006, text: 'Quit')
    about = r_create_menu(id: 5014, text: 'About')
    help = r_create_menu(id: 5009)
    fileMenu = {'File', [cDV, quit]}

    nodeMenu =
      case :erlang.is_alive() do
        true ->
          {'Nodes', nodesMenuItems ++ [r_create_menu(id: 1, text: 'Connect Node')]}

        false ->
          {'Nodes', nodesMenuItems ++ [r_create_menu(id: 2, text: 'Enable distribution')]}
      end

    logMenu = {'Log', [r_create_menu(id: 5, text: 'Toggle log view')]}

    case :os.type() === {:unix, :darwin} do
      false ->
        ^fileMenu = {'File', [cDV, quit]}
        helpMenu = {'Help', [about, help]}
        [fileMenu, nodeMenu, logMenu, helpMenu]

      true ->
        {tag, menus} = fileMenu
        [{tag, menus ++ [quit, about]}, nodeMenu, logMenu, {'&Help', [help]}]
    end
  end

  defp clean_menus(menus, menuBar) do
    remove_menu_items(menus, menuBar)
  end

  defp remove_menu_items([{menuStr = 'File', menus} | rest], menuBar) do
    case :wxMenuBar.findMenu(menuBar, menuStr) do
      -1 ->
        remove_menu_items(rest, menuBar)

      menuId ->
        menu = :wxMenuBar.getMenu(menuBar, menuId)

        items =
          for r_create_menu(text: tag) <- menus do
            :wxMenu.findItem(menu, tag)
          end

        for mItem <- items do
          :wxMenu.delete(menu, mItem)
        end

        remove_menu_items(rest, menuBar)
    end
  end

  defp remove_menu_items([{'Nodes', _} | _], _MB) do
    :ok
  end

  defp remove_menu_items([{tag, _Menus} | rest], menuBar) do
    case :wxMenuBar.findMenu(menuBar, tag) do
      -1 ->
        remove_menu_items(rest, menuBar)

      menuId ->
        menu = :wxMenuBar.getMenu(menuBar, menuId)
        :wxMenuBar.remove(menuBar, menuId)
        items = :wxMenu.getMenuItems(menu)

        for item <- items do
          :wxMenu."Destroy"(menu, item)
        end

        :wxMenu.destroy(menu)
        remove_menu_items(rest, menuBar)
    end
  end

  defp remove_menu_items([], _MB) do
    :ok
  end

  defp get_nodes() do
    nodes0 =
      case :erlang.is_alive() do
        false ->
          []

        true ->
          case :net_adm.names() do
            {:error, _} ->
              :erlang.nodes()

            {:ok, names} ->
              epmd_nodes(names) ++ :erlang.nodes()
          end
      end

    nodes = :lists.usort(nodes0)

    {_, menues} =
      :lists.foldl(
        fn node, {id, acc}
           when id < 2000 ->
          {id + 1,
           [
             r_create_menu(
               id: id + 1000,
               text: :erlang.atom_to_list(node)
             )
             | acc
           ]}
        end,
        {1, []},
        nodes
      )

    {nodes, :lists.reverse(menues)}
  end

  defp epmd_nodes(names) do
    [_, host] = :string.lexemes(:erlang.atom_to_list(node()), '@')

    for {name, _} <- names do
      :erlang.list_to_atom(name ++ [?@ | host])
    end
  end

  defp update_node_list(state = r_state(menubar: menuBar)) do
    {nodes, nodesMenuItems} = get_nodes()

    nodeMenu =
      case :wxMenuBar.findMenu(menuBar, 'Nodes') do
        -1 ->
          menu = :wxMenu.new()
          :wxMenuBar.append(menuBar, menu, 'Nodes')
          menu

        nodeMenuId ->
          menu = :wxMenuBar.getMenu(menuBar, nodeMenuId)

          :wx.foreach(
            fn item ->
              :wxMenu."Destroy"(menu, item)
            end,
            :wxMenu.getMenuItems(menu)
          )

          menu
      end

    index =
      :wx.foldl(
        fn record, index ->
          :observer_lib.create_menu_item(record, nodeMenu, index)
        end,
        0,
        nodesMenuItems
      )

    dist =
      case :erlang.is_alive() do
        true ->
          r_create_menu(id: 1, text: 'Connect node')

        false ->
          r_create_menu(id: 2, text: 'Enable distribution')
      end

    :observer_lib.create_menu_item(dist, nodeMenu, index)
    r_state(state, nodes: nodes)
  end

  defp ensure_sasl_started(node) do
    apps = :rpc.block_call(node, :application, :which_applications, [])

    case :lists.keyfind(:sasl, 1, apps) do
      false ->
        throw('Error: sasl application not started.')
        :error

      {:sasl, _, _} ->
        :ok
    end
  end

  defp ensure_mf_h_handler_used(node) do
    handlers =
      case :rpc.block_call(node, :gen_event, :which_handlers, [:error_logger]) do
        {:badrpc, {:EXIT, :noproc}} ->
          []

        hs ->
          hs
      end

    case :lists.any(
           fn l ->
             l == :log_mf_h
           end,
           handlers
         ) do
      false ->
        throw('Error: log_mf_h handler not used in sasl.')
        :error

      true ->
        :ok
    end
  end

  defp ensure_rb_mode(node, prevLog) do
    :ok = ensure_rb_module_loaded(node)
    :ok = is_rb_compatible(node)
    :ok = is_rb_server_running(node, prevLog)
    :ok
  end

  defp ensure_rb_module_loaded(node) do
    case :rpc.block_call(node, :code, :ensure_loaded, [:rb]) do
      {:badrpc, reason} ->
        throw('Error: badrpc - ' ++ :io_lib.format('~tp', [reason]))

      {:error, reason} ->
        throw('Error: rb module load error - ' ++ :io_lib.format('~tp', [reason]))

      {:module, :rb} ->
        :ok
    end
  end

  defp is_rb_compatible(node) do
    case :rpc.block_call(node, :erlang, :function_exported, [:rb, :log_list, 0]) do
      false ->
        throw('Error: Node\'s Erlang release must be at least R16B02.')

      true ->
        :ok
    end
  end

  defp is_rb_server_running(node, logState) do
    case :rpc.block_call(node, :erlang, :whereis, [:rb_server]) do
      pid when is_pid(pid) and logState == false ->
        throw('Error: rb_server is already started and maybe used by someone.')

      pid when is_pid(pid) ->
        :ok

      :undefined ->
        :ok
    end
  end
end
