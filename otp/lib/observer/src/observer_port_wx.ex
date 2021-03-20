defmodule :m_observer_port_wx do
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

  Record.defrecord(:r_port, :port,
    id: :undefined,
    connected: :undefined,
    name: :undefined,
    controls: :undefined,
    slot: :undefined,
    id_str: :undefined,
    links: :undefined,
    monitors: :undefined,
    monitored_by: :undefined,
    parallelism: :undefined,
    locking: :undefined,
    queue_size: :undefined,
    memory: :undefined,
    inet: :undefined
  )

  Record.defrecord(:r_opt, :opt, sort_key: 2, sort_incr: true, odd_bg: :undefined)

  Record.defrecord(:r_state, :state,
    parent: :undefined,
    grid: :undefined,
    panel: :undefined,
    node: {node(), true},
    opt: :EFE_TODO_NESTED_RECORD,
    right_clicked_port: :undefined,
    ports: :undefined,
    timer: :undefined,
    open_wins: []
  )

  def start_link(notebook, parent, config) do
    :wx_object.start_link(:observer_port_wx, [notebook, parent, config], [])
  end

  def init([notebook, parent, config]) do
    panel = :wxPanel.new(notebook)
    sizer = :wxBoxSizer.new(8)
    style = 32 ||| 2

    grid =
      :wxListCtrl.new(
        panel,
        [{:winid, 300}, {:style, style}]
      )

    :wxSizer.add(sizer, grid, [
      {:flag, 8192 ||| (64 ||| 128 ||| 32 ||| 16)},
      {:proportion, 1},
      {:border, 5}
    ])

    :wxWindow.setSizer(panel, sizer)
    li = :wxListItem.new()

    addListEntry = fn {name, align, defSize}, col ->
      :wxListItem.setText(li, name)
      :wxListItem.setAlign(li, align)
      :wxListCtrl.insertColumn(grid, col, li)
      :wxListCtrl.setColumnWidth(grid, col, defSize)
      col + 1
    end

    scale = :observer_wx.get_scale()

    listItems = [
      {'Id', 0, scale * 150},
      {'Connected', 0, scale * 150},
      {'Name', 0, scale * 150},
      {'Controls', 0, scale * 200},
      {'Slot', 1, scale * 50}
    ]

    :lists.foldl(addListEntry, 0, listItems)
    :wxListItem.destroy(li)

    :wxListCtrl.connect(
      grid,
      :command_list_item_right_click
    )

    :wxListCtrl.connect(grid, :command_list_item_activated)
    :wxListCtrl.connect(grid, :command_list_col_click)
    :wxListCtrl.connect(grid, :size, [{:skip, true}])
    :wxWindow.setFocus(grid)
    even = :wxSystemSettings.getColour(1 + 14 + 1 + 4 + 5)
    odd = :observer_lib.mix(even, :wxSystemSettings.getColour(1 + 12), 0.8)
    opt = r_opt(odd_bg: odd)
    {panel, r_state(grid: grid, parent: parent, panel: panel, timer: config, opt: opt)}
  end

  def handle_event(
        r_wx(id: 301),
        state = r_state(node: node, grid: grid, opt: opt)
      ) do
    ports0 = get_ports(node)
    ports = update_grid(grid, sel(state), opt, ports0)
    {:noreply, r_state(state, ports: ports)}
  end

  def handle_event(
        r_wx(obj: obj, event: r_wxClose()),
        r_state(open_wins: opened) = state
      ) do
    newOpened =
      case :lists.keytake(obj, 2, opened) do
        false ->
          opened

        {:value, _, rest} ->
          rest
      end

    {:noreply, r_state(state, open_wins: newOpened)}
  end

  def handle_event(
        r_wx(
          event:
            r_wxList(
              type: :command_list_col_click,
              col: col
            )
        ),
        state = r_state(node: node, grid: grid, opt: opt0 = r_opt(sort_key: key, sort_incr: bool))
      ) do
    opt =
      case col + 2 do
        ^key ->
          r_opt(opt0, sort_incr: not bool)

        newKey ->
          r_opt(opt0, sort_key: newKey)
      end

    ports0 = get_ports(node)
    ports = update_grid(grid, sel(state), opt, ports0)
    :wxWindow.setFocus(grid)
    {:noreply, r_state(state, opt: opt, ports: ports)}
  end

  def handle_event(
        r_wx(event: r_wxSize(size: {w, _})),
        state = r_state(grid: grid)
      ) do
    :observer_lib.set_listctrl_col_size(grid, w)
    {:noreply, state}
  end

  def handle_event(
        r_wx(
          event:
            r_wxList(
              type: :command_list_item_activated,
              itemIndex: index
            )
        ),
        state = r_state(grid: grid, ports: ports, open_wins: opened)
      ) do
    port = :lists.nth(index + 1, ports)
    newOpened = display_port_info(grid, port, opened)
    {:noreply, r_state(state, open_wins: newOpened)}
  end

  def handle_event(
        r_wx(
          event:
            r_wxList(
              type: :command_list_item_right_click,
              itemIndex: index
            )
        ),
        state = r_state(panel: panel, ports: ports)
      ) do
    case index do
      -1 ->
        {:noreply, state}

      _ ->
        port = :lists.nth(index + 1, ports)
        menu = :wxMenu.new()
        :wxMenu.append(menu, 303, 'Port info for ' ++ :erlang.port_to_list(r_port(port, :id)))
        :wxMenu.append(menu, 305, 'Trace selected ports', [{:help, 'Trace selected ports'}])

        :wxMenu.append(menu, 306, 'Trace selected ports by name (all nodes)', [
          {:help,
           'Trace selected ports, if a process have a registered name processes with same name will be traced on all nodes'}
        ])

        :wxMenu.append(menu, 309, 'Close ' ++ :erlang.port_to_list(r_port(port, :id)))
        :wxWindow.popupMenu(panel, menu)
        :wxMenu.destroy(menu)
        {:noreply, r_state(state, right_clicked_port: port)}
    end
  end

  def handle_event(
        r_wx(id: 303),
        state = r_state(grid: grid, right_clicked_port: port, open_wins: opened)
      ) do
    case port do
      :undefined ->
        {:noreply, state}

      _ ->
        newOpened = display_port_info(grid, port, opened)

        {:noreply,
         r_state(state,
           right_clicked_port: :undefined,
           open_wins: newOpened
         )}
    end
  end

  def handle_event(
        r_wx(id: 304),
        state = r_state(grid: grid, ports: ports, open_wins: opened)
      ) do
    case get_selected_items(grid, ports) do
      [] ->
        :observer_wx.create_txt_dialog(
          r_state(state, :panel),
          'No selected ports',
          'Port Info',
          256
        )

        {:noreply, state}

      selected ->
        newOpened =
          :lists.foldl(
            fn p, o ->
              display_port_info(grid, p, o)
            end,
            opened,
            selected
          )

        {:noreply, r_state(state, open_wins: newOpened)}
    end
  end

  def handle_event(
        r_wx(id: 309),
        state = r_state(right_clicked_port: port)
      ) do
    case port do
      :undefined ->
        {:noreply, state}

      _ ->
        :erlang.port_close(r_port(port, :id))
        {:noreply, r_state(state, right_clicked_port: :undefined)}
    end
  end

  def handle_event(
        r_wx(id: 305),
        r_state(grid: grid, ports: ports) = state
      ) do
    case get_selected_items(grid, ports) do
      [] ->
        :observer_wx.create_txt_dialog(r_state(state, :panel), 'No selected ports', 'Tracer', 256)

      selected ->
        selectedIds =
          for port <- selected do
            r_port(port, :id)
          end

        :observer_trace_wx.add_ports(selectedIds)
    end

    {:noreply, state}
  end

  def handle_event(
        r_wx(id: 306),
        r_state(grid: grid, ports: ports) = state
      ) do
    case get_selected_items(grid, ports) do
      [] ->
        :observer_wx.create_txt_dialog(r_state(state, :panel), 'No selected ports', 'Tracer', 256)

      selected ->
        idsOrRegs =
          for port <- selected do
            case r_port(port, :name) do
              [] ->
                r_port(port, :id)

              name ->
                name
            end
          end

        :observer_trace_wx.add_ports(idsOrRegs)
    end

    {:noreply, state}
  end

  def handle_event(
        r_wx(
          id: 307,
          event: r_wxCommand(type: :command_menu_selected)
        ),
        state
      ) do
    :observer_trace_wx.add_ports([:new_ports])
    {:noreply, state}
  end

  def handle_event(
        r_wx(id: 302),
        state = r_state(grid: grid, timer: timer0)
      ) do
    timer = :observer_lib.interval_dialog(grid, timer0, 10, 5 * 60)
    {:noreply, r_state(state, timer: timer)}
  end

  def handle_event(
        r_wx(obj: moreEntry, event: r_wxMouse(type: :left_down), userData: {:more, more}),
        state
      ) do
    :observer_lib.add_scroll_entries(moreEntry, more)
    {:noreply, state}
  end

  def handle_event(
        r_wx(
          event: r_wxMouse(type: :left_down),
          userData: targetPid
        ),
        state
      ) do
    send(:observer, {:open_link, targetPid})
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

  def handle_event(event, _State) do
    :erlang.error({:unhandled_event, event})
  end

  def handle_sync_event(_Event, _Obj, _State) do
    :ok
  end

  def handle_call(:get_config, _, r_state(timer: timer) = state) do
    {:reply, :observer_lib.timer_config(timer), state}
  end

  def handle_call(event, from, _State) do
    :erlang.error({:unhandled_call, event, from})
  end

  def handle_cast(event, _State) do
    :erlang.error({:unhandled_cast, event})
  end

  def handle_info(
        {:portinfo_open, portIdStr},
        state =
          r_state(
            node: {activeNodeName, activeAvailable},
            grid: grid,
            opt: opt,
            open_wins: opened
          )
      ) do
    nodeName = node(:erlang.list_to_port(portIdStr))

    available =
      case nodeName do
        ^activeNodeName ->
          activeAvailable

        _ ->
          portinfo_available(nodeName)
      end

    cond do
      available ->
        ports0 = get_ports({nodeName, available})
        port = :lists.keyfind(portIdStr, r_port(:id_str), ports0)

        newOpened =
          case port do
            false ->
              send(self(), {:error, 'No such port: ' ++ portIdStr})
              opened

            _ ->
              display_port_info(grid, port, opened)
          end

        ports =
          case nodeName do
            ^activeNodeName ->
              update_grid(grid, sel(state), opt, ports0)

            _ ->
              r_state(state, :ports)
          end

        {:noreply, r_state(state, ports: ports, open_wins: newOpened)}

      true ->
        popup_unavailable_info(nodeName)
        {:noreply, state}
    end
  end

  def handle_info(
        :refresh_interval,
        state = r_state(node: node, grid: grid, opt: opt, ports: oldPorts)
      ) do
    case get_ports(node) do
      ^oldPorts ->
        {:noreply, state}

      ports0 ->
        ports = update_grid(grid, sel(state), opt, ports0)
        {:noreply, r_state(state, ports: ports)}
    end
  end

  def handle_info(
        {:active, nodeName},
        state = r_state(parent: parent, grid: grid, opt: opt, timer: timer0)
      ) do
    available = portinfo_available(nodeName)
    available or popup_unavailable_info(nodeName)
    ports0 = get_ports({nodeName, available})
    ports = update_grid(grid, sel(state), opt, ports0)
    :wxWindow.setFocus(grid)
    create_menus(parent)
    timer = :observer_lib.start_timer(timer0, 10)
    {:noreply, r_state(state, node: {nodeName, available}, ports: ports, timer: timer)}
  end

  def handle_info(:not_active, state = r_state(timer: timer0)) do
    timer = :observer_lib.stop_timer(timer0)
    {:noreply, r_state(state, timer: timer)}
  end

  def handle_info(
        {:info, {:port_info_not_available, nodeName}},
        state = r_state(panel: panel)
      ) do
    str = :io_lib.format('Can not fetch port info from ~p.~nToo old OTP version.', [nodeName])
    :observer_lib.display_info_dialog(panel, str)
    {:noreply, state}
  end

  def handle_info({:error, error}, r_state(panel: panel) = state) do
    str = :io_lib.format('ERROR: ~ts~n', [error])
    :observer_lib.display_info_dialog(panel, str)
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

  defp create_menus(parent) do
    menuEntries = [
      {'View',
       [
         r_create_menu(id: 304, text: 'Port info for selected ports\tCtrl-I'),
         :separator,
         r_create_menu(id: 301, text: 'Refresh\tCtrl-R'),
         r_create_menu(
           id: 302,
           text: 'Refresh Interval...'
         )
       ]},
      {'Trace',
       [
         r_create_menu(id: 305, text: 'Trace selected ports'),
         r_create_menu(id: 306, text: 'Trace selected ports by name (all nodes)'),
         r_create_menu(
           id: 307,
           text: 'Trace new ports'
         )
       ]}
    ]

    :observer_wx.create_menus(parent, menuEntries)
  end

  defp get_ports({_NodeName, false}) do
    []
  end

  defp get_ports({nodeName, true}) do
    case get_ports2(nodeName) do
      error = {:error, _} ->
        send(self(), error)
        []

      res ->
        res
    end
  end

  defp get_ports2(nodeName) do
    case :rpc.call(nodeName, :observer_backend, :get_port_list, []) do
      {:badrpc, error} ->
        {:error, error}

      error = {:error, _} ->
        error

      result ->
        for port <- result do
          list_to_portrec(port)
        end
    end
  end

  defp list_to_portrec(pL) do
    portId = :proplists.get_value(:port_id, pL)

    r_port(
      id: portId,
      id_str: :erlang.port_to_list(portId),
      slot: :proplists.get_value(:id, pL),
      connected: :proplists.get_value(:connected, pL),
      links: :proplists.get_value(:links, pL, []),
      name: :proplists.get_value(:registered_name, pL, []),
      monitors: :proplists.get_value(:monitors, pL, []),
      monitored_by: :proplists.get_value(:monitored_by, pL, []),
      controls: :proplists.get_value(:name, pL),
      parallelism: :proplists.get_value(:parallelism, pL),
      locking: :proplists.get_value(:locking, pL),
      queue_size: :proplists.get_value(:queue_size, pL, 0),
      memory: :proplists.get_value(:memory, pL, 0),
      inet: :proplists.get_value(:inet, pL, [])
    )
  end

  defp portrec_to_list(
         r_port(
           id: id,
           slot: slot,
           connected: connected,
           links: links,
           name: name,
           monitors: monitors,
           monitored_by: monitoredBy,
           controls: controls,
           parallelism: parallelism,
           locking: locking,
           queue_size: queueSize,
           memory: memory,
           inet: inet
         )
       ) do
    [
      {:id, id},
      {:slot, slot},
      {:connected, connected},
      {:links, links},
      {:name, name},
      {:monitors, monitors},
      {:monitored_by, monitoredBy},
      {:controls, controls},
      {:parallelism, parallelism},
      {:locking, locking},
      {:queue_size, queueSize},
      {:memory, memory}
      | inet
    ]
  end

  defp display_port_info(parent, portRec, opened) do
    portIdStr = r_port(portRec, :id_str)

    case :lists.keyfind(portIdStr, 1, opened) do
      false ->
        frame = do_display_port_info(parent, portRec)
        [{portIdStr, frame} | opened]

      {_, win} ->
        :wxFrame.raise(win)
        opened
    end
  end

  defp do_display_port_info(parent0, portRec) do
    parent = :observer_lib.get_wx_parent(parent0)
    title = 'Port Info: ' ++ r_port(portRec, :id_str)
    scale = :observer_wx.get_scale()

    frame =
      :wxMiniFrame.new(parent, -1, title, [
        {:style, 2048 ||| 536_870_912 ||| 4096 ||| 64},
        {:size, {scale * 600, scale * 400}}
      ])

    scrolledWin =
      :wxScrolledWindow.new(
        frame,
        [{:style, 1_073_741_824 ||| 2_147_483_648}]
      )

    :wxScrolledWindow.enableScrolling(scrolledWin, true, true)
    :wxScrolledWindow.setScrollbars(scrolledWin, 20, 20, 0, 0)
    sizer = :wxBoxSizer.new(8)
    :wxWindow.setSizer(scrolledWin, sizer)
    port = portrec_to_list(portRec)
    fields0 = port_info_fields(port)
    _UpFields = :observer_lib.display_info(scrolledWin, sizer, fields0)
    :wxFrame.center(frame)
    :wxFrame.connect(frame, :close_window, [{:skip, true}])
    :wxFrame.show(frame)
    frame
  end

  defp port_info_fields(port0) do
    {inetStruct, port} = inet_extra_fields(port0)

    struct = [
      {'Overview',
       [
         {'Registered Name', :name},
         {'Connected', {:click, :connected}},
         {'Slot', :slot},
         {'Controls', :controls},
         {'Parallelism', :parallelism},
         {'Locking', :locking},
         {'Queue Size', {:bytes, :queue_size}},
         {'Memory', {:bytes, :memory}}
       ]},
      {:scroll_boxes,
       [
         {'Links', 1, {:click, :links}},
         {'Monitors', 1, {:click, filter_monitor_info()}},
         {'Monitored by', 1, {:click, :monitored_by}}
       ]}
      | inetStruct
    ]

    :observer_lib.fill_info(struct, port)
  end

  defp inet_extra_fields(port) do
    statistics = :proplists.get_value(:statistics, port, [])
    options = :proplists.get_value(:options, port, [])

    struct =
      case :proplists.get_value(
             :controls,
             port
           ) do
        inet when inet == 'tcp_inet' or inet == 'udp_inet' or inet == 'sctp_inet' ->
          [
            {'Inet',
             [
               {'Local Address', {:inet, :local_address}},
               {'Local Port Number', :local_port},
               {'Remote Address', {:inet, :remote_address}},
               {'Remote Port Number', :remote_port}
             ]},
            {'Statistics',
             for {key, _} <- statistics do
               stat_name_and_unit(key)
             end},
            {'Options',
             for {key, _} <- options do
               {:erlang.atom_to_list(key), key}
             end}
          ]

        _ ->
          []
      end

    port1 = :lists.keydelete(:statistics, 1, port)
    port2 = :lists.keydelete(:options, 1, port1)
    {struct, port2 ++ statistics ++ options}
  end

  defp stat_name_and_unit(:recv_avg) do
    {'Average package size received', {:bytes, :recv_avg}}
  end

  defp stat_name_and_unit(:recv_cnt) do
    {'Number of packets received', :recv_cnt}
  end

  defp stat_name_and_unit(:recv_dvi) do
    {'Average packet size deviation received', {:bytes, :recv_dvi}}
  end

  defp stat_name_and_unit(:recv_max) do
    {'Largest packet received', {:bytes, :recv_max}}
  end

  defp stat_name_and_unit(:recv_oct) do
    {'Total received', {:bytes, :recv_oct}}
  end

  defp stat_name_and_unit(:send_avg) do
    {'Average packet size sent', {:bytes, :send_avg}}
  end

  defp stat_name_and_unit(:send_cnt) do
    {'Number of packets sent', :send_cnt}
  end

  defp stat_name_and_unit(:send_max) do
    {'Largest packet sent', {:bytes, :send_max}}
  end

  defp stat_name_and_unit(:send_oct) do
    {'Total sent', {:bytes, :send_oct}}
  end

  defp stat_name_and_unit(:send_pend) do
    {'Data waiting to be sent from driver', {:bytes, :send_pend}}
  end

  defp stat_name_and_unit(key) do
    {:erlang.atom_to_list(key), key}
  end

  defp filter_monitor_info() do
    fn data ->
      ms = :proplists.get_value(:monitors, data)

      for {:process, pid} <- ms do
        pid
      end
    end
  end

  defp update_grid(grid, sel, opt, ports) do
    :wx.batch(fn ->
      update_grid2(grid, sel, opt, ports)
    end)
  end

  defp update_grid2(grid, sel, r_opt(sort_key: sort, sort_incr: dir, odd_bg: bG), ports) do
    :wxListCtrl.deleteAllItems(grid)

    update = fn r_port(id: id, slot: slot, connected: connected, name: name, controls: ctrl),
                row ->
      _Item = :wxListCtrl.insertItem(grid, row, '')

      cond do
        rem(row, 2) === 1 ->
          :wxListCtrl.setItemBackgroundColour(grid, row, bG)

        true ->
          :ignore
      end

      :lists.foreach(
        fn {col, val} ->
          :wxListCtrl.setItem(grid, row, col, :observer_lib.to_str(val))
        end,
        [{0, id}, {1, connected}, {2, name}, {3, ctrl}, {4, slot}]
      )

      case :lists.member(id, sel) do
        true ->
          :wxListCtrl.setItemState(grid, row, 65535, 4)

        false ->
          :wxListCtrl.setItemState(grid, row, 0, 4)
      end

      row + 1
    end

    portInfo =
      case dir do
        false ->
          :lists.reverse(:lists.keysort(sort, ports))

        true ->
          :lists.keysort(sort, ports)
      end

    :lists.foldl(update, 0, portInfo)
    portInfo
  end

  defp sel(r_state(grid: grid, ports: ports)) do
    for r_port(id: id) <- get_selected_items(grid, ports) do
      id
    end
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

  defp portinfo_available(nodeName) do
    _ = :rpc.call(nodeName, :code, :ensure_loaded, [:observer_backend])

    case :rpc.call(nodeName, :erlang, :function_exported, [:observer_backend, :get_port_list, 0]) do
      true ->
        true

      false ->
        false
    end
  end

  defp popup_unavailable_info(nodeName) do
    send(self(), {:info, {:port_info_not_available, nodeName}})
    :ok
  end
end
