defmodule :m_observer_pro_wx do
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

  Record.defrecord(:r_etop_info, :etop_info,
    now: {0, 0, 0},
    n_procs: 0,
    wall_clock: :undefined,
    runtime: :undefined,
    run_queue: 0,
    alloc_areas: [],
    memi: [{:total, 0}, {:processes, 0}, {:ets, 0}, {:atom, 0}, {:code, 0}, {:binary, 0}],
    procinfo: []
  )

  Record.defrecord(:r_etop_proc_info, :etop_proc_info,
    pid: :undefined,
    mem: 0,
    reds: 0,
    name: :undefined,
    runtime: 0,
    cf: :undefined,
    mq: 0
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

  Record.defrecord(:r_opts, :opts,
    node: node(),
    port: 8415,
    accum: false,
    intv: 5000,
    lines: 10,
    width: 700,
    height: 340,
    sort: :runtime,
    tracing: :on,
    out_mod: :etop_txt,
    out_proc: :undefined,
    server: :undefined,
    host: :undefined,
    tracer: :undefined,
    store: :undefined,
    accum_tab: :undefined,
    remote: :undefined
  )

  Record.defrecord(:r_sort, :sort,
    sort_key: 0 + 1 + 1,
    sort_incr: false
  )

  Record.defrecord(:r_holder, :holder,
    parent: :undefined,
    info: :undefined,
    next: [],
    sort: :EFE_TODO_NESTED_RECORD,
    accum: [],
    next_accum: [],
    attrs: :undefined,
    node: :undefined,
    backend_pid: :undefined,
    old_backend: false
  )

  Record.defrecord(:r_state, :state,
    parent: :undefined,
    grid: :undefined,
    panel: :undefined,
    popup_menu: :undefined,
    parent_notebook: :undefined,
    timer: :undefined,
    procinfo_menu_pids: [],
    sel: {[], []},
    right_clicked_pid: :undefined,
    holder: :undefined
  )

  def start_link(notebook, parent, config) do
    :wx_object.start_link(:observer_pro_wx, [notebook, parent, config], [])
  end

  def init([notebook, parent, config]) do
    attrs = :observer_lib.create_attrs(notebook)
    self = self()
    acc = :maps.get(:acc, config, false)

    holder =
      spawn_link(fn ->
        init_table_holder(self, acc, attrs)
      end)

    {proPanel, state} = setup(notebook, parent, holder, config)
    {proPanel, r_state(state, holder: holder)}
  end

  defp setup(notebook, parent, holder, config) do
    proPanel = :wxPanel.new(notebook, [])
    grid = create_list_box(proPanel, holder)
    sizer = :wxBoxSizer.new(8)

    :wxSizer.add(sizer, grid, [
      {:flag, 8192 ||| (64 ||| 128 ||| 32 ||| 16)},
      {:proportion, 1},
      {:border, 4}
    ])

    :wxWindow.setSizer(proPanel, sizer)

    state =
      r_state(
        parent: parent,
        grid: grid,
        panel: proPanel,
        parent_notebook: notebook,
        holder: holder,
        timer: config
      )

    {proPanel, state}
  end

  defp create_pro_menu(parent, holder) do
    menuEntries = [
      {'File', [r_create_menu(id: 205, text: 'Dump to file')]},
      {'View',
       [
         r_create_menu(
           id: 210,
           text: 'Accumulate',
           type: :check,
           check:
             call(
               holder,
               {:get_accum, self()}
             )
         ),
         :separator,
         r_create_menu(
           id: 203,
           text: 'Refresh\tCtrl-R'
         ),
         r_create_menu(
           id: 204,
           text: 'Refresh Interval'
         )
       ]},
      {'Trace',
       [
         r_create_menu(id: 206, text: 'Trace processes'),
         r_create_menu(id: 207, text: 'Trace named processes (all nodes)'),
         r_create_menu(
           id: 208,
           text: 'Trace new processes'
         )
       ]}
    ]

    :observer_wx.create_menus(parent, menuEntries)
  end

  defp create_list_box(panel, holder) do
    style = 32 ||| 512 ||| 2

    listCtrl =
      :wxListCtrl.new(
        panel,
        [
          {:style, style},
          {:onGetItemText,
           fn _, row, col ->
             safe_call(
               holder,
               {:get_row, self(), row, col}
             )
           end},
          {:onGetItemAttr,
           fn _, item ->
             safe_call(
               holder,
               {:get_attr, self(), item}
             )
           end}
        ]
      )

    li = :wxListItem.new()

    addListEntry = fn {name, align, defSize}, col ->
      :wxListItem.setText(li, name)
      :wxListItem.setAlign(li, align)
      :wxListCtrl.insertColumn(listCtrl, col, li)
      :wxListCtrl.setColumnWidth(listCtrl, col, defSize)
      col + 1
    end

    scale = :observer_wx.get_scale()

    listItems = [
      {'Pid', 2, scale * 120},
      {'Name or Initial Func', 0, scale * 200},
      {'Reds', 1, scale * 100},
      {'Memory', 1, scale * 100},
      {'MsgQ', 1, scale * 50},
      {'Current Function', 0, scale * 200}
    ]

    :lists.foldl(addListEntry, 0, listItems)
    :wxListItem.destroy(li)
    :wxListCtrl.setItemCount(listCtrl, 1)
    :wxListCtrl.connect(listCtrl, :size, [{:skip, true}])

    :wxListCtrl.connect(
      listCtrl,
      :command_list_item_activated
    )

    :wxListCtrl.connect(
      listCtrl,
      :command_list_item_right_click
    )

    :wxListCtrl.connect(listCtrl, :command_list_col_click)

    :wxListCtrl.connect(
      listCtrl,
      :command_list_item_focused
    )

    listCtrl
  end

  defp dump_to_file(parent, fileName, holder) do
    case :file.open(fileName, [:write]) do
      {:ok, fd} ->
        send(holder, {:dump, fd})

      {:error, reason} ->
        failMsg = :file.format_error(reason)
        mD = :wxMessageDialog.new(parent, failMsg)
        :wxDialog.showModal(mD)
        :wxDialog.destroy(mD)
    end
  end

  defp start_procinfo(:undefined, _Frame, opened) do
    opened
  end

  defp start_procinfo(pid, frame, opened) do
    case :lists.keyfind(pid, 1, opened) do
      false ->
        case :observer_procinfo.start(pid, frame, self()) do
          {:error, _} ->
            opened

          pI ->
            [{pid, pI} | opened]
        end

      {_, pI} ->
        :wxFrame.raise(pI)
        opened
    end
  end

  defp safe_call(holder, what) do
    case call(holder, what, 2000) do
      res when is_atom(res) ->
        ''

      res ->
        res
    end
  end

  defp call(holder, what) do
    call(holder, what, :infinity)
  end

  defp call(holder, what, tMO) do
    ref = :erlang.monitor(:process, holder)
    send(holder, what)

    receive do
      {:DOWN, ^ref, _, _, _} ->
        :holder_dead

      {^holder, res} ->
        :erlang.demonitor(ref)
        res
    after
      tMO ->
        :timeout
    end
  end

  def handle_info(
        {:holder_updated, count},
        state0 = r_state(grid: grid)
      ) do
    state = update_selection(state0)
    :wxListCtrl.setItemCount(grid, count)
    count > 0 and :wxListCtrl.refreshItems(grid, 0, count - 1)
    :observer_wx.set_status(:io_lib.format('Number of Processes: ~w', [count]))
    {:noreply, state}
  end

  def handle_info(:refresh_interval, r_state(holder: holder) = state) do
    send(holder, :refresh)
    {:noreply, state}
  end

  def handle_info(
        {:procinfo_menu_closed, pid},
        r_state(procinfo_menu_pids: opened) = state
      ) do
    newPids = :lists.keydelete(pid, 1, opened)
    {:noreply, r_state(state, procinfo_menu_pids: newPids)}
  end

  def handle_info(
        {:procinfo_open, pid},
        r_state(panel: panel, procinfo_menu_pids: opened) = state
      ) do
    opened2 = start_procinfo(pid, panel, opened)
    {:noreply, r_state(state, procinfo_menu_pids: opened2)}
  end

  def handle_info(
        {:active, node},
        r_state(holder: holder, timer: timer, parent: parent) = state
      ) do
    create_pro_menu(parent, holder)
    send(holder, {:change_node, node})
    {:noreply, r_state(state, timer: :observer_lib.start_timer(timer, 10))}
  end

  def handle_info(:not_active, r_state(timer: timer0) = state) do
    timer = :observer_lib.stop_timer(timer0)
    {:noreply, r_state(state, timer: timer)}
  end

  def handle_info(info, state) do
    :io.format('~p:~p, Unexpected info: ~tp~n', [:observer_pro_wx, 270, info])
    {:noreply, state}
  end

  def terminate(_Reason, r_state(holder: holder)) do
    send(holder, :stop)
    :etop.stop()
    :ok
  end

  def code_change(_, _, state) do
    {:ok, state}
  end

  def handle_call(:get_config, _, r_state(holder: holder, timer: timer) = state) do
    conf = :observer_lib.timer_config(timer)

    accum =
      case safe_call(
             holder,
             {:get_accum, self()}
           ) do
        bool when is_boolean(bool) ->
          bool

        _ ->
          false
      end

    {:reply, Map.put(conf, :acc, accum), state}
  end

  def handle_call(msg, _From, state) do
    :io.format('~p:~p: Unhandled call ~tp~n', [:observer_pro_wx, 290, msg])
    {:reply, :ok, state}
  end

  def handle_cast(msg, state) do
    :io.format('~p:~p: Unhandled cast ~tp~n', [:observer_pro_wx, 294, msg])
    {:noreply, state}
  end

  def handle_event(
        r_wx(id: 205),
        r_state(panel: panel, holder: holder) = state
      ) do
    fD = :wxFileDialog.new(panel, [{:style, 2 ||| 4}])

    case :wxFileDialog.showModal(fD) do
      5100 ->
        path = :wxFileDialog.getPath(fD)
        :wxDialog.destroy(fD)
        dump_to_file(panel, path, holder)

      _ ->
        :wxDialog.destroy(fD)
    end

    {:noreply, state}
  end

  def handle_event(
        r_wx(
          id: 210,
          event:
            r_wxCommand(
              type: :command_menu_selected,
              commandInt: cmdInt
            )
        ),
        r_state(holder: holder) = state
      ) do
    send(holder, {:accum, cmdInt === 1})
    {:noreply, state}
  end

  def handle_event(
        r_wx(
          id: 203,
          event: r_wxCommand(type: :command_menu_selected)
        ),
        r_state(holder: holder) = state
      ) do
    send(holder, :refresh)
    {:noreply, state}
  end

  def handle_event(
        r_wx(id: 204),
        r_state(panel: panel, timer: timer0) = state
      ) do
    timer = :observer_lib.interval_dialog(panel, timer0, 1, 5 * 60)
    {:noreply, r_state(state, timer: timer)}
  end

  def handle_event(
        r_wx(id: 201),
        r_state(right_clicked_pid: pid, sel: sel0) = state
      ) do
    :erlang.exit(pid, :kill)
    sel = rm_selected(pid, sel0)
    {:noreply, r_state(state, sel: sel)}
  end

  def handle_event(r_wx(id: 211), r_state(sel: {_, pids}) = state) do
    _ =
      for pid <- pids do
        :rpc.call(node(pid), :erlang, :garbage_collect, [pid])
      end

    {:noreply, state}
  end

  def handle_event(
        r_wx(id: 202),
        r_state(panel: panel, right_clicked_pid: pid, procinfo_menu_pids: opened) = state
      ) do
    opened2 = start_procinfo(pid, panel, opened)
    {:noreply, r_state(state, procinfo_menu_pids: opened2)}
  end

  def handle_event(
        r_wx(id: 206),
        r_state(sel: {_, pids}, panel: panel) = state
      ) do
    case pids do
      [] ->
        :observer_wx.create_txt_dialog(panel, 'No selected processes', 'Tracer', 256)
        {:noreply, state}

      ^pids ->
        :observer_trace_wx.add_processes(pids)
        {:noreply, state}
    end
  end

  def handle_event(
        r_wx(id: 207),
        r_state(sel: {selIds, _Pids}, holder: holder, panel: panel) = state
      ) do
    case selIds do
      [] ->
        :observer_wx.create_txt_dialog(panel, 'No selected processes', 'Tracer', 256)
        {:noreply, state}

      _ ->
        pidsOrReg =
          call(
            holder,
            {:get_name_or_pid, self(), selIds}
          )

        :observer_trace_wx.add_processes(pidsOrReg)
        {:noreply, state}
    end
  end

  def handle_event(
        r_wx(
          id: 208,
          event: r_wxCommand(type: :command_menu_selected)
        ),
        state
      ) do
    :observer_trace_wx.add_processes([:new_processes])
    {:noreply, state}
  end

  def handle_event(
        r_wx(event: r_wxSize(size: {w, _})),
        r_state(grid: grid) = state
      ) do
    :observer_lib.set_listctrl_col_size(grid, w)
    {:noreply, state}
  end

  def handle_event(
        r_wx(
          event:
            r_wxList(
              type: :command_list_item_right_click,
              itemIndex: row
            )
        ),
        r_state(panel: panel, holder: holder) = state
      ) do
    pid =
      case call(
             holder,
             {:get_row, self(), row, :pid}
           ) do
        {:error, :undefined} ->
          :undefined

        {:ok, p} ->
          menu = :wxMenu.new()
          :wxMenu.append(menu, 202, 'Process info for ' ++ :erlang.pid_to_list(p))

          :wxMenu.append(menu, 206, 'Trace selected processes', [
            {:help, 'Trace selected process identifiers'}
          ])

          :wxMenu.append(menu, 207, 'Trace selected processes by name (all nodes)', [
            {:help,
             'Trace selected processes, if a process have a registered name processes with same name will be traced on all nodes'}
          ])

          :wxMenu.append(menu, 211, 'Garbage collect processes')
          :wxMenu.append(menu, 201, 'Kill process ' ++ :erlang.pid_to_list(p))
          :wxWindow.popupMenu(panel, menu)
          :wxMenu.destroy(menu)
          p
      end

    {:noreply, r_state(state, right_clicked_pid: pid)}
  end

  def handle_event(
        r_wx(
          event:
            r_wxList(
              type: :command_list_item_focused,
              itemIndex: row
            )
        ),
        r_state(grid: grid, holder: holder) = state
      ) do
    case row >= 0 do
      true ->
        selIds = [
          row
          | :lists.delete(
              row,
              get_selected_items(grid)
            )
        ]

        pids = call(holder, {:get_pids, self(), selIds})
        {:noreply, r_state(state, sel: {selIds, pids})}

      false ->
        {:noreply, state}
    end
  end

  def handle_event(
        r_wx(
          event:
            r_wxList(
              type: :command_list_col_click,
              col: col
            )
        ),
        r_state(holder: holder) = state
      ) do
    send(holder, {:change_sort, col})
    {:noreply, state}
  end

  def handle_event(
        r_wx(event: r_wxList(type: :command_list_item_activated)),
        r_state(panel: panel, procinfo_menu_pids: opened, sel: {_, [pid | _]}) = state
      ) do
    opened2 = start_procinfo(pid, panel, opened)
    {:noreply, r_state(state, procinfo_menu_pids: opened2)}
  end

  def handle_event(event, state) do
    :io.format('~p:~p: handle event ~tp\n', [:observer_pro_wx, 422, event])
    {:noreply, state}
  end

  defp update_selection(state = r_state(holder: holder, grid: grid, sel: {selIds0, selPids0})) do
    sel =
      {selIds, _SelPids} =
      call(
        holder,
        {:get_rows_from_pids, self(), selPids0}
      )

    set_focus(selIds0, selIds, grid)

    case selIds === selIds0 do
      true ->
        :ok

      false ->
        :wx.batch(fn ->
          for i <- selIds0 do
            :wxListCtrl.setItemState(grid, i, 0, 4)
          end

          for i <- selIds do
            :wxListCtrl.setItemState(grid, i, 65535, 4)
          end
        end)
    end

    r_state(state, sel: sel)
  end

  defp get_selected_items(grid) do
    get_selected_items(grid, -1, [])
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

  defp set_focus([], [], _Grid) do
    :ok
  end

  defp set_focus([same | _], [same | _], _Grid) do
    :ok
  end

  defp set_focus([], [new | _], grid) do
    :wxListCtrl.setItemState(grid, new, 65535, 2)
  end

  defp set_focus([old | _], [], grid) do
    :wxListCtrl.setItemState(grid, old, 0, 2)
  end

  defp set_focus([old | _], [new | _], grid) do
    :wxListCtrl.setItemState(grid, old, 0, 2)
    :wxListCtrl.setItemState(grid, new, 65535, 2)
  end

  defp rm_selected(pid, {ids, pids}) do
    rm_selected(pid, ids, pids, [], [])
  end

  defp rm_selected(pid, [_Id | ids], [pid | pids], accIds, accPids) do
    {:lists.reverse(accIds) ++ ids, :lists.reverse(accPids) ++ pids}
  end

  defp rm_selected(pid, [id | ids], [otherPid | pids], accIds, accPids) do
    rm_selected(pid, ids, pids, [id | accIds], [otherPid | accPids])
  end

  defp rm_selected(_, [], [], accIds, accPids) do
    {:lists.reverse(accIds), :lists.reverse(accPids)}
  end

  defp init_table_holder(parent, accum0, attrs) do
    :erlang.process_flag(:trap_exit, true)
    backend = :erlang.spawn_link(node(), :observer_backend, :procs_info, [self()])

    accum =
      case accum0 do
        true ->
          true

        _ ->
          []
      end

    table_holder(
      r_holder(
        parent: parent,
        info: :array.new(),
        node: node(),
        backend_pid: backend,
        attrs: attrs,
        accum: accum
      )
    )
  end

  defp table_holder(
         r_holder(info: info, attrs: attrs, node: node, backend_pid: backend, old_backend: old) =
           s0
       ) do
    receive do
      {:get_row, from, row, col} ->
        get_row(from, row, col, info)
        table_holder(s0)

      {:get_attr, from, row} ->
        get_attr(from, row, attrs)
        table_holder(s0)

      {:procs_info, ^backend, procs} ->
        state = handle_update(procs, s0)
        table_holder(state)

      {:EXIT, ^backend, :normal} when old === false ->
        s1 = update_complete(s0)
        table_holder(r_holder(s1, backend_pid: :undefined))

      {^backend, etopInfo = r_etop_info()} ->
        state = handle_update_old(etopInfo, s0)
        table_holder(r_holder(state, backend_pid: :undefined))

      :refresh when is_pid(backend) ->
        table_holder(s0)

      :refresh ->
        pid =
          case old do
            true ->
              :erlang.spawn_link(node, :observer_backend, :etop_collect, [self()])

            false ->
              :erlang.spawn_link(node, :observer_backend, :procs_info, [self()])
          end

        table_holder(r_holder(s0, backend_pid: pid))

      {:change_sort, col} ->
        state = change_sort(col, s0)
        table_holder(state)

      {:get_pids, from, indices} ->
        get_pids(from, indices, info)
        table_holder(s0)

      {:get_rows_from_pids, from, pids} ->
        get_rows_from_pids(from, pids, info)
        table_holder(s0)

      {:get_name_or_pid, from, indices} ->
        get_name_or_pid(from, indices, info)
        table_holder(s0)

      {:get_node, from} ->
        send(from, {self(), node})
        table_holder(s0)

      {:change_node, newNode} ->
        case node == newNode do
          true ->
            table_holder(s0)

          false ->
            _ = :rpc.call(newNode, :code, :ensure_loaded, [:observer_backend])

            case :rpc.call(newNode, :erlang, :function_exported, [
                   :observer_backend,
                   :procs_info,
                   1
                 ]) do
              true ->
                send(self(), :refresh)
                table_holder(r_holder(s0, node: newNode, old_backend: false))

              false ->
                send(self(), :refresh)
                table_holder(r_holder(s0, node: newNode, old_backend: true))

              _ ->
                table_holder(s0)
            end
        end

      {:accum, bool} ->
        table_holder(change_accum(bool, s0))

      {:get_accum, from} ->
        send(from, {self(), r_holder(s0, :accum) == true})
        table_holder(s0)

      {:dump, fd} ->
        collector = :erlang.spawn_link(node, :observer_backend, :etop_collect, [self()])

        receive do
          {^collector, etopInfo = r_etop_info()} ->
            :etop_txt.do_update(fd, etopInfo, r_etop_info(), r_opts(node: node))
            :file.close(fd)
            table_holder(s0)

          {:EXIT, ^collector, _} ->
            table_holder(s0)
        end

      :stop ->
        :ok

      {:EXIT, ^backend, :normal} ->
        table_holder(s0)

      {:EXIT, ^backend, _Reason} ->
        table_holder(r_holder(s0, backend_pid: :undefined))

      _What ->
        table_holder(s0)
    end
  end

  defp change_sort(
         col,
         s0 = r_holder(parent: parent, info: data, sort: sort0)
       ) do
    {sort, procInfo} = sort(col, sort0, data)
    send(parent, {:holder_updated, :array.size(data)})
    r_holder(s0, info: :array.from_list(procInfo), sort: sort)
  end

  defp change_accum(true, s0) do
    r_holder(s0, accum: true)
  end

  defp change_accum(false, s0 = r_holder(info: info)) do
    send(self(), :refresh)

    accum =
      for r_etop_proc_info(
            pid: pid,
            reds: reds
          ) <- :array.to_list(info) do
        {pid, reds}
      end

    r_holder(s0, accum: :lists.sort(accum))
  end

  defp handle_update_old(
         r_etop_info(procinfo: procInfo0),
         s0 =
           r_holder(
             parent: parent,
             sort: sort = r_sort(sort_key: keyField)
           )
       ) do
    {procInfo1, accum} = accum(procInfo0, s0)
    {_SO, procInfo} = sort(keyField, r_sort(sort, sort_key: :undefined), procInfo1)
    info = :array.from_list(procInfo)
    send(parent, {:holder_updated, :array.size(info)})
    r_holder(s0, info: info, accum: accum)
  end

  defp handle_update(
         procInfo0,
         s0 = r_holder(next: next, sort: r_sort(sort_key: keyField))
       ) do
    {procInfo1, accum} = accum(procInfo0, s0)
    sort = sort_fun(keyField, true)
    merge = merge_fun(keyField)
    merged = merge.(sort.(procInfo1), next)

    case accum do
      true ->
        r_holder(s0, next: merged)

      _List ->
        r_holder(s0, next: merged, next_accum: accum)
    end
  end

  defp update_complete(
         r_holder(
           parent: parent,
           sort: r_sort(sort_incr: incr),
           next: procInfo,
           accum: accum,
           next_accum: nextAccum
         ) = s0
       ) do
    info =
      case incr do
        true ->
          :array.from_list(procInfo)

        false ->
          :array.from_list(:lists.reverse(procInfo))
      end

    send(parent, {:holder_updated, :array.size(info)})
    r_holder(s0, info: info, accum: accum === true or nextAccum, next: [], next_accum: [])
  end

  defp accum(procInfo, r_holder(accum: true)) do
    {procInfo, true}
  end

  defp accum(
         procInfo0,
         r_holder(accum: previous, next_accum: next)
       ) do
    accum =
      for r_etop_proc_info(pid: pid, reds: reds) <- procInfo0 do
        {pid, reds}
      end

    procInfo = :lists.sort(procInfo0)
    {accum2(procInfo, previous, []), :lists.merge(:lists.sort(accum), next)}
  end

  defp accum2([pI = r_etop_proc_info(pid: pid, reds: reds) | pIs], [{pid, oldReds} | old], acc) do
    accum2(pIs, old, [r_etop_proc_info(pI, reds: reds - oldReds) | acc])
  end

  defp accum2(pIs = [r_etop_proc_info(pid: pid) | _], [{oldPid, _} | old], acc)
       when pid > oldPid do
    accum2(pIs, old, acc)
  end

  defp accum2([pI | pIs], old, acc) do
    accum2(pIs, old, [pI | acc])
  end

  defp accum2([], _, acc) do
    acc
  end

  defp sort(col, opt, table) when not is_list(table) do
    sort(col, opt, :array.to_list(table))
  end

  defp sort(col, opt = r_sort(sort_key: col, sort_incr: bool), table) do
    {r_sort(opt, sort_incr: not bool), :lists.reverse(table)}
  end

  defp sort(col, s = r_sort(sort_incr: incr), table) do
    sort = sort_fun(col, incr)
    {r_sort(s, sort_key: col), sort.(table)}
  end

  defp sort_fun(1, true) do
    fn table ->
      :lists.sort(&sort_name/2, table)
    end
  end

  defp sort_fun(1, false) do
    fn table ->
      :lists.sort(&sort_name_rev/2, table)
    end
  end

  defp sort_fun(col, true) do
    n = col_to_element(col)

    fn table ->
      :lists.keysort(n, table)
    end
  end

  defp sort_fun(col, false) do
    n = col_to_element(col)

    fn table ->
      :lists.reverse(:lists.keysort(n, table))
    end
  end

  defp merge_fun(1) do
    fn a, b ->
      :lists.merge(&sort_name/2, a, b)
    end
  end

  defp merge_fun(col) do
    keyField = col_to_element(col)

    fn a, b ->
      :lists.keymerge(keyField, a, b)
    end
  end

  defp sort_name(
         r_etop_proc_info(name: {_, _, _} = a),
         r_etop_proc_info(name: {_, _, _} = b)
       ) do
    a <= b
  end

  defp sort_name(r_etop_proc_info(name: a), r_etop_proc_info(name: b))
       when is_atom(a) and
              is_atom(b) do
    a <= b
  end

  defp sort_name(r_etop_proc_info(name: reg), r_etop_proc_info(name: {m, _F, _A}))
       when is_atom(reg) do
    reg < m
  end

  defp sort_name(r_etop_proc_info(name: {m, _, _}), r_etop_proc_info(name: reg))
       when is_atom(reg) do
    m < reg
  end

  defp sort_name_rev(
         r_etop_proc_info(name: {_, _, _} = a),
         r_etop_proc_info(name: {_, _, _} = b)
       ) do
    a >= b
  end

  defp sort_name_rev(r_etop_proc_info(name: a), r_etop_proc_info(name: b))
       when is_atom(a) and
              is_atom(b) do
    a >= b
  end

  defp sort_name_rev(r_etop_proc_info(name: reg), r_etop_proc_info(name: {m, _F, _A}))
       when is_atom(reg) do
    reg >= m
  end

  defp sort_name_rev(r_etop_proc_info(name: {m, _, _}), r_etop_proc_info(name: reg))
       when is_atom(reg) do
    m >= reg
  end

  defp get_procinfo_data(col, info) do
    :erlang.element(col_to_element(col), info)
  end

  defp col_to_element(0) do
    r_etop_proc_info(:pid)
  end

  defp col_to_element(1) do
    r_etop_proc_info(:name)
  end

  defp col_to_element(3) do
    r_etop_proc_info(:mem)
  end

  defp col_to_element(2) do
    r_etop_proc_info(:reds)
  end

  defp col_to_element(5) do
    r_etop_proc_info(:cf)
  end

  defp col_to_element(4) do
    r_etop_proc_info(:mq)
  end

  defp get_pids(from, indices, procInfo) do
    processes =
      for i <- indices do
        r_etop_proc_info(:array.get(i, procInfo), :pid)
      end

    send(from, {self(), processes})
  end

  defp get_name_or_pid(from, indices, procInfo) do
    get = fn
      r_etop_proc_info(name: name) when is_atom(name) ->
        name

      r_etop_proc_info(pid: pid) ->
        pid
    end

    processes =
      for i <- indices do
        get.(:array.get(i, procInfo))
      end

    send(from, {self(), processes})
  end

  defp get_row(from, row, :pid, info) do
    pid =
      case row === -1 do
        true ->
          {:error, :undefined}

        false ->
          {:ok, get_procinfo_data(0, :array.get(row, info))}
      end

    send(from, {self(), pid})
  end

  defp get_row(from, row, col, info) do
    data =
      case row >= :array.size(info) do
        true ->
          ''

        false ->
          procInfo = :array.get(row, info)
          get_procinfo_data(col, procInfo)
      end

    send(from, {self(), :observer_lib.to_str(data)})
  end

  defp get_rows_from_pids(from, pids0, info) do
    search = fn idx, r_etop_proc_info(pid: pid), acc0 = {pick0, {idxs, pids}} ->
      case :ordsets.is_element(pid, pick0) do
        true ->
          acc = {[idx | idxs], [pid | pids]}
          pick = :ordsets.del_element(pid, pick0)

          case pick === [] do
            true ->
              throw(acc)

            false ->
              {pick, acc}
          end

        false ->
          acc0
      end
    end

    res =
      try do
        {_, r} =
          :array.foldl(
            search,
            {:ordsets.from_list(pids0), {[], []}},
            info
          )

        r
      catch
        r0 ->
          r0
      end

    send(from, {self(), res})
  end

  defp get_attr(from, row, attrs) do
    attribute =
      case rem(row, 2) === 0 do
        true ->
          r_attrs(attrs, :even)

        false ->
          r_attrs(attrs, :odd)
      end

    send(from, {self(), attribute})
  end
end
