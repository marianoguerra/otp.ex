defmodule :m_observer_tv_table do
  use Bitwise
  import :observer_lib, only: [to_str: 1]
  require Record
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

  @behaviour :wx_object
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

  Record.defrecord(:r_tab, :tab,
    name: :undefined,
    id: :ignore,
    size: :undefined,
    memory: 0,
    owner: :undefined,
    reg_name: :undefined,
    protection: :public,
    type: :set,
    keypos: 1,
    heir: :none,
    compressed: false,
    fixed: false,
    storage: :undefined,
    index: :undefined
  )

  Record.defrecord(:r_state, :state,
    parent: :undefined,
    frame: :undefined,
    grid: :undefined,
    status: :undefined,
    sizer: :undefined,
    search: :undefined,
    selected: :undefined,
    node: node(),
    columns: :undefined,
    pid: :undefined,
    source: :undefined,
    tab: :undefined,
    attrs: :undefined,
    timer: {false, 30}
  )

  Record.defrecord(:r_opt, :opt, sort_key: 2, sort_incr: true)

  Record.defrecord(:r_search, :search,
    enable: true,
    win: :undefined,
    name: :undefined,
    search: :undefined,
    goto: :undefined,
    radio: :undefined,
    find: :undefined
  )

  Record.defrecord(:r_find, :find, start: :undefined, strlen: :undefined, found: :undefined)

  def start_link(parent, opts) do
    :wx_object.start_link(:observer_tv_table, [parent, opts], [])
  end

  def init([parent, opts]) do
    source = :proplists.get_value(:type, opts)
    table = :proplists.get_value(:table, opts)
    node = :proplists.get_value(:node, opts)
    title0 = :erlang.atom_to_list(r_tab(table, :name)) ++ ' @ ' ++ :erlang.atom_to_list(node)

    title =
      case source do
        :ets ->
          'TV Ets: ' ++ title0

        :mnesia ->
          'TV Mnesia: ' ++ title0
      end

    scale = :observer_wx.get_scale()
    frame = :wxFrame.new(parent, -1, title, [{:size, {scale * 800, scale * 600}}])
    iconFile = :filename.join(:code.priv_dir(:observer), 'erlang_observer.png')
    icon = :wxIcon.new(iconFile, [{:type, 2 + 13}])
    :wxFrame.setIcon(frame, icon)
    :wxIcon.destroy(icon)
    menuBar = :wxMenuBar.new()
    create_menus(menuBar)
    :wxFrame.setMenuBar(frame, menuBar)
    :wxMenu.connect(frame, :command_menu_selected)
    statusBar = :wxFrame.createStatusBar(frame, [])

    try do
      tabId = table_id(table)
      columnNames = column_names(node, source, tabId)
      keyPos = key_pos(node, source, tabId)
      panel = :wxPanel.new(frame)
      attrs = :observer_lib.create_attrs(panel)
      self = self()

      holder =
        spawn_link(fn ->
          init_table_holder(self, table, source, length(columnNames), node, attrs)
        end)

      sizer = :wxBoxSizer.new(8)
      style = 32 ||| 512 ||| 8192 ||| 2

      grid =
        :wxListCtrl.new(
          panel,
          [
            {:style, style},
            {:onGetItemText,
             fn _, item, col ->
               get_row(holder, item, col + 1)
             end},
            {:onGetItemAttr,
             fn _, item ->
               get_attr(
                 holder,
                 item
               )
             end}
          ]
        )

      :wxListCtrl.connect(grid, :command_list_item_activated)
      :wxListCtrl.connect(grid, :command_list_item_selected)
      :wxListCtrl.connect(grid, :command_list_col_click)
      :wxListCtrl.connect(grid, :size, [{:skip, true}])
      :wxWindow.setFocus(grid)
      search = search_area(panel)

      :wxSizer.add(sizer, grid, [
        {:flag, 8192 ||| (64 ||| 128 ||| 32 ||| 16)},
        {:proportion, 1},
        {:border, 5}
      ])

      :wxSizer.add(sizer, r_search(search, :win), [
        {:flag, 8192 ||| 16 ||| 32 ||| 2},
        {:border, 5}
      ])

      :wxWindow.setSizer(panel, sizer)
      :wxSizer.hide(sizer, r_search(search, :win))
      cols = add_columns(grid, 0, columnNames)
      :wxFrame.show(frame)

      {panel,
       r_state(
         frame: frame,
         grid: grid,
         status: statusBar,
         search: search,
         sizer: sizer,
         parent: parent,
         columns: cols,
         pid: holder,
         source: source,
         tab: r_tab(table, keypos: keyPos),
         attrs: attrs
       )}
    catch
      :node_or_table_down ->
        :wxFrame.destroy(frame)
        :stop
    end
  end

  defp add_columns(grid, start, columnNames) do
    li = :wxListItem.new()

    addListEntry = fn name, col ->
      :wxListItem.setText(li, to_str(name))
      :wxListItem.setAlign(li, 0)
      :wxListCtrl.insertColumn(grid, col, li)
      :wxListCtrl.setColumnWidth(grid, col, 150)
      col + 1
    end

    cols = :lists.foldl(addListEntry, start, columnNames)
    :wxListItem.destroy(li)
    cols
  end

  defp create_menus(mB) do
    file = :wxMenu.new()
    :wxMenu.append(file, 400, 'Table Information\tCtrl-I')
    :wxMenu.append(file, 5001, 'Close')
    :wxMenuBar.append(mB, file, 'File')
    edit = :wxMenu.new()
    :wxMenu.append(edit, 403, 'Edit Object')
    :wxMenu.append(edit, 404, 'Delete Object\tCtrl-D')
    :wxMenu.appendSeparator(edit)
    :wxMenu.append(edit, 405, 'Search\tCtrl-S')
    :wxMenu.appendSeparator(edit)
    :wxMenu.append(edit, 401, 'Refresh\tCtrl-R')
    :wxMenu.append(edit, 402, 'Refresh interval...')
    :wxMenuBar.append(mB, edit, 'Edit')
    help = :wxMenu.new()
    :wxMenu.append(help, 5009, 'Help')
    :wxMenuBar.append(mB, help, 'Help')
    :ok
  end

  defp search_area(parent) do
    hSz = :wxBoxSizer.new(4)
    :wxSizer.add(hSz, :wxStaticText.new(parent, -1, 'Find:'), [{:flag, 2048}])
    tC1 = :wxTextCtrl.new(parent, 420, [{:style, 1024}])
    :wxSizer.add(hSz, tC1, [{:proportion, 3}, {:flag, 8192}])
    nbtn = :wxRadioButton.new(parent, -1, 'Next')
    :wxRadioButton.setValue(nbtn, true)
    :wxSizer.add(hSz, nbtn, [{:flag, 2048}])
    pbtn = :wxRadioButton.new(parent, -1, 'Previous')
    :wxSizer.add(hSz, pbtn, [{:flag, 2048}])
    cbtn = :wxCheckBox.new(parent, -1, 'Match Case')
    :wxSizer.add(hSz, cbtn, [{:flag, 2048}])
    :wxSizer.add(hSz, 15, 15, [{:proportion, 1}, {:flag, 8192}])
    :wxSizer.add(hSz, :wxStaticText.new(parent, -1, 'Goto Entry:'), [{:flag, 2048}])
    tC2 = :wxTextCtrl.new(parent, 421, [{:style, 1024}])
    :wxSizer.add(hSz, tC2, [{:proportion, 0}, {:flag, 8192}])
    :wxTextCtrl.connect(tC1, :command_text_updated)
    :wxTextCtrl.connect(tC1, :command_text_enter)
    :wxTextCtrl.connect(tC1, :kill_focus)
    :wxTextCtrl.connect(tC2, :command_text_enter)
    :wxWindow.connect(parent, :command_button_clicked)
    r_search(name: :"Search Area", win: hSz, search: tC1, goto: tC2, radio: {nbtn, pbtn, cbtn})
  end

  defp edit(index, r_state(pid: pid, frame: frame)) do
    str = get_row(pid, index, :all_multiline)

    case :observer_lib.user_term_multiline(frame, 'Edit object:', str) do
      :cancel ->
        :ok

      {:ok, term} ->
        send(pid, {:edit, index, term})

      err = {:error, _} ->
        send(self(), err)
    end
  end

  def handle_event(r_wx(id: 401), state = r_state(pid: pid)) do
    send(pid, :refresh)
    {:noreply, state}
  end

  def handle_event(
        r_wx(
          event:
            r_wxList(
              type: :command_list_col_click,
              col: col
            )
        ),
        state = r_state(pid: pid, grid: grid, selected: oldSel)
      ) do
    selObj =
      case oldSel do
        :undefined ->
          :undefined

        _ ->
          get_row(pid, oldSel, :term)
      end

    send(pid, {:sort, col + 1})

    case selObj !== :undefined and search(pid, selObj, -1, true, :term) do
      false when is_integer(oldSel) ->
        :wxListCtrl.setItemState(grid, oldSel, 0, 4)
        {:noreply, r_state(state, selected: :undefined)}

      false ->
        {:noreply, r_state(state, selected: :undefined)}

      row ->
        :wxListCtrl.setItemState(grid, row, 65535, 4)
        {:noreply, r_state(state, selected: row)}
    end
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
              type: :command_list_item_selected,
              itemIndex: index
            )
        ),
        state = r_state(pid: pid, grid: grid, status: statusBar)
      ) do
    n = :wxListCtrl.getItemCount(grid)
    str = get_row(pid, index, :all)

    :wxStatusBar.setStatusText(
      statusBar,
      :io_lib.format('Objects: ~w: ~ts', [n, str])
    )

    {:noreply, r_state(state, selected: index)}
  end

  def handle_event(
        r_wx(
          event:
            r_wxList(
              type: :command_list_item_activated,
              itemIndex: index
            )
        ),
        state
      ) do
    edit(index, state)
    {:noreply, state}
  end

  def handle_event(r_wx(id: 403), state = r_state(selected: :undefined)) do
    {:noreply, state}
  end

  def handle_event(r_wx(id: 403), state = r_state(selected: index)) do
    edit(index, state)
    {:noreply, state}
  end

  def handle_event(r_wx(id: 404), state = r_state(selected: :undefined)) do
    {:noreply, state}
  end

  def handle_event(
        r_wx(id: 404),
        state = r_state(grid: grid, pid: pid, status: statusBar, selected: index)
      ) do
    str = get_row(pid, index, :all)
    send(pid, {:delete, index})

    :wxStatusBar.setStatusText(
      statusBar,
      :io_lib.format('Deleted object: ~ts', [str])
    )

    :wxListCtrl.setItemState(grid, index, 0, 2)
    {:noreply, r_state(state, selected: :undefined)}
  end

  def handle_event(r_wx(id: 5001), state = r_state(frame: frame)) do
    :wxFrame.destroy(frame)
    {:stop, :normal, state}
  end

  def handle_event(help = r_wx(id: 5009), state) do
    send(:observer, help)
    {:noreply, state}
  end

  def handle_event(
        r_wx(id: 421, event: r_wxCommand(cmdString: str)),
        state = r_state(grid: grid)
      ) do
    try do
      row0 = :erlang.list_to_integer(str)
      row1 = max(0, row0)
      row = min(:wxListCtrl.getItemCount(grid) - 1, row1)
      :wxListCtrl.ensureVisible(grid, row)
      :ok
    catch
      _, _ ->
        :ok
    end

    {:noreply, state}
  end

  def handle_event(
        r_wx(id: 405),
        state = r_state(grid: grid, sizer: sz, search: search, selected: index)
      ) do
    is_integer(index) and :wxListCtrl.setItemState(grid, index, 0, 2)
    :wxSizer.show(sz, r_search(search, :win))
    :wxWindow.setFocus(r_search(search, :search))
    :wxSizer.layout(sz)
    {:noreply, state}
  end

  def handle_event(
        r_wx(id: 420, event: r_wxFocus()),
        state = r_state(search: search, pid: pid)
      ) do
    send(pid, {:mark_search_hit, false})
    {:noreply, r_state(state, search: r_search(search, find: :undefined))}
  end

  def handle_event(
        r_wx(id: 420, event: r_wxCommand(cmdString: '')),
        state = r_state(search: search, pid: pid)
      ) do
    send(pid, {:mark_search_hit, false})
    {:noreply, r_state(state, search: r_search(search, find: :undefined))}
  end

  def handle_event(
        r_wx(
          id: 420,
          event: r_wxCommand(type: :command_text_enter, cmdString: str)
        ),
        state =
          r_state(
            grid: grid,
            pid: pid,
            status: sB,
            search:
              search =
                r_search(
                  radio: {next0, _, case0},
                  find: find
                )
          )
      )
      when find !== :undefined do
    dir =
      :erlang.xor(
        :wxRadioButton.getValue(next0),
        :wx_misc.getKeyState(306)
      )

    case__ = :wxCheckBox.getValue(case0)

    pos =
      cond do
        r_find(find, :found) and dir ->
          r_find(find, :start) + 1

        r_find(find, :found) ->
          r_find(find, :start) - 1

        dir ->
          0

        true ->
          :wxListCtrl.getItemCount(grid) - 1
      end

    send(pid, {:mark_search_hit, false})

    case search(pid, str, pos, dir, case__) do
      false ->
        :wxStatusBar.setStatusText(sB, :io_lib.format('Not found (regexp): ~ts', [str]))
        send(pid, {:mark_search_hit, r_find(find, :start)})
        :wxListCtrl.refreshItem(grid, r_find(find, :start))
        {:noreply, r_state(state, search: r_search(search, find: r_find(find, found: false)))}

      row ->
        :wxListCtrl.ensureVisible(grid, row)
        :wxListCtrl.refreshItem(grid, row)
        status = 'Found: (Hit Enter for next, Shift-Enter for previous)'
        :wxStatusBar.setStatusText(sB, status)

        {:noreply,
         r_state(state,
           search:
             r_search(search,
               find:
                 r_find(
                   start: row,
                   found: true
                 )
             )
         )}
    end
  end

  def handle_event(
        r_wx(id: 420, event: r_wxCommand(cmdString: str)),
        state =
          r_state(
            grid: grid,
            pid: pid,
            status: sB,
            search:
              search =
                r_search(
                  radio: {next0, _, case0},
                  find: find
                )
          )
      ) do
    try do
      dir = :wxRadioButton.getValue(next0)
      case__ = :wxCheckBox.getValue(case0)

      start =
        case dir do
          true ->
            0

          false ->
            :wxListCtrl.getItemCount(grid) - 1
        end

      cont =
        case find do
          :undefined ->
            r_find(start: start, strlen: length(str))

          r_find(strlen: old) when old < length(str) ->
            r_find(find, start: start, strlen: length(str))

          _ ->
            r_find(find, strlen: length(str))
        end

      send(pid, {:mark_search_hit, false})

      case search(pid, str, r_find(cont, :start), dir, case__) do
        false ->
          :wxStatusBar.setStatusText(sB, :io_lib.format('Not found (regexp): ~ts', [str]))
          {:noreply, state}

        row ->
          :wxListCtrl.ensureVisible(grid, row)
          :wxListCtrl.refreshItem(grid, row)
          status = 'Found: (Hit Enter for next, Shift-Enter for previous)'
          :wxStatusBar.setStatusText(sB, status)

          {:noreply,
           r_state(state,
             search:
               r_search(search,
                 find:
                   r_find(
                     start: row,
                     found: true
                   )
               )
           )}
      end
    catch
      _, _ ->
        {:noreply, state}
    end
  end

  def handle_event(
        r_wx(id: 400),
        state = r_state(frame: frame, node: node, source: source, tab: table)
      ) do
    :observer_tv_wx.display_table_info(frame, node, source, table)
    {:noreply, state}
  end

  def handle_event(
        r_wx(id: 402),
        state = r_state(grid: grid, timer: timer0)
      ) do
    timer = :observer_lib.interval_dialog(grid, timer0, 10, 5 * 60)
    {:noreply, r_state(state, timer: timer)}
  end

  def handle_event(_Event, state) do
    {:noreply, state}
  end

  def handle_sync_event(_Event, _Obj, _State) do
    :ok
  end

  def handle_call(_Event, _From, state) do
    {:noreply, state}
  end

  def handle_cast(_Event, state) do
    {:noreply, state}
  end

  def handle_info(
        {:no_rows, n},
        state = r_state(grid: grid, status: statusBar)
      ) do
    :wxListCtrl.setItemCount(grid, n)

    :wxStatusBar.setStatusText(
      statusBar,
      :io_lib.format('Objects: ~w', [n])
    )

    {:noreply, state}
  end

  def handle_info(
        {:new_cols, new},
        state = r_state(grid: grid, columns: cols0)
      ) do
    cols = add_columns(grid, cols0, new)
    {:noreply, r_state(state, columns: cols)}
  end

  def handle_info({:refresh, min, min}, state = r_state(grid: grid)) do
    :wxListCtrl.refreshItem(grid, min)
    {:noreply, state}
  end

  def handle_info({:refresh, min, max}, state = r_state(grid: grid)) do
    max > 0 and :wxListCtrl.refreshItems(grid, min, max)
    {:noreply, state}
  end

  def handle_info(:refresh_interval, state = r_state(pid: pid)) do
    send(pid, :refresh)
    {:noreply, state}
  end

  def handle_info({:error, error}, state = r_state(frame: frame)) do
    errorStr =
      try do
        :io_lib.format('~ts', [error])
        error
      catch
        _, _ ->
          :io_lib.format('~tp', [error])
      end

    dlg = :wxMessageDialog.new(frame, errorStr)
    :wxMessageDialog.showModal(dlg)
    :wxMessageDialog.destroy(dlg)
    {:noreply, state}
  end

  def handle_info(_Event, state) do
    {:noreply, state}
  end

  def terminate(_Event, r_state(pid: pid, attrs: attrs)) do
    r_attrs(
      odd: odd,
      even: even,
      deleted: d,
      searched: s,
      changed_odd: ch1,
      changed_even: ch2,
      new_odd: new1,
      new_even: new2
    ) = attrs

    :wxListItemAttr.destroy(odd)
    :wxListItemAttr.destroy(even)
    :wxListItemAttr.destroy(d)
    :wxListItemAttr.destroy(ch1)
    :wxListItemAttr.destroy(ch2)
    :wxListItemAttr.destroy(new1)
    :wxListItemAttr.destroy(new2)
    :wxListItemAttr.destroy(s)
    :erlang.unlink(pid)
    :erlang.exit(pid, :window_closed)
    :ok
  end

  def code_change(_, _, state) do
    state
  end

  defp get_row(table, item, column) do
    ref = :erlang.monitor(:process, table)
    send(table, {:get_row, self(), item, column})

    receive do
      {:DOWN, ^ref, _, _, _} ->
        ''

      {^table, res} ->
        :erlang.demonitor(ref)
        res
    end
  end

  defp get_attr(table, item) do
    ref = :erlang.monitor(:process, table)
    send(table, {:get_attr, self(), item})

    receive do
      {:DOWN, ^ref, _, _, _} ->
        :wx.null()

      {^table, res} ->
        :erlang.demonitor(ref)
        res
    end
  end

  defp search(table, str, row, dir, case__) do
    ref = :erlang.monitor(:process, table)
    send(table, {:search, [str, row, dir, case__]})

    receive do
      {:DOWN, ^ref, _, _, _} ->
        ''

      {^table, res} ->
        :erlang.demonitor(ref)
        res
    end
  end

  Record.defrecord(:r_holder, :holder,
    node: :undefined,
    parent: :undefined,
    pid: :undefined,
    table: :array.new(),
    n: 0,
    columns: :undefined,
    temp: [],
    search: :undefined,
    source: :undefined,
    tabid: :undefined,
    sort: :undefined,
    key: :undefined,
    type: :undefined,
    attrs: :undefined
  )

  defp init_table_holder(parent, table, mnesiaOrEts, cols, node, attrs) do
    tabId =
      case r_tab(table, :id) do
        :ignore ->
          r_tab(table, :name)

        id ->
          id
      end

    send(self(), :refresh)

    table_holder(
      r_holder(
        node: node,
        parent: parent,
        source: mnesiaOrEts,
        tabid: tabId,
        columns: cols,
        sort: r_opt(sort_key: r_tab(table, :keypos), sort_incr: true),
        type: r_tab(table, :type),
        key: r_tab(table, :keypos),
        attrs: attrs
      )
    )
  end

  defp table_holder(s0 = r_holder(parent: parent, pid: pid, table: table)) do
    receive do
      {:get_attr, from, row} ->
        get_attr(from, row, s0)
        table_holder(s0)

      {:get_row, from, row, col} ->
        get_row(from, row, col, table)
        table_holder(s0)

      {^pid, data} ->
        s1 = handle_new_data_chunk(data, s0)
        table_holder(s1)

      {:sort, col} ->
        send(parent, {:refresh, 0, r_holder(s0, :n) - 1})
        table_holder(sort(col, s0))

      {:search, data} ->
        table_holder(search(data, s0))

      {:mark_search_hit, row} ->
        old = r_holder(s0, :search)
        is_integer(old) and send(parent, {:refresh, old, old})
        table_holder(r_holder(s0, search: row))

      :refresh when is_pid(pid) ->
        table_holder(s0)

      :refresh ->
        getTab =
          :rpc.call(r_holder(s0, :node), :observer_backend, :get_table, [
            self(),
            r_holder(s0, :tabid),
            r_holder(s0, :source)
          ])

        table_holder(r_holder(s0, pid: getTab))

      {:delete, row} ->
        delete_row(row, s0)
        table_holder(s0)

      {:edit, row, term} ->
        edit_row(row, term, s0)
        table_holder(s0)

      what ->
        :io.format('Table holder got ~tp~n', [what])
        send(parent, {:refresh, 0, r_holder(s0, :n) - 1})
        table_holder(s0)
    end
  end

  defp handle_new_data_chunk(data, s0 = r_holder(columns: cols, parent: parent)) do
    s1 =
      r_holder(
        n: n,
        columns: newCols
      ) = handle_new_data_chunk2(data, s0)

    send(parent, {:no_rows, n})
    send(parent, {:refresh, 0, n - 1})

    case newCols === cols do
      true ->
        s1

      false ->
        send(parent, {:new_cols, :lists.seq(cols + 1, newCols)})
        s1
    end
  end

  defp handle_new_data_chunk2(
         :"$end_of_table",
         s0 = r_holder(sort: opt0, key: key, table: old, temp: new)
       ) do
    merged = merge(:array.to_list(old), new, key)
    {opt, sorted} = sort(r_opt(opt0, :sort_key), r_opt(opt0, sort_key: :undefined), merged)
    sortedA = :array.from_list(sorted)
    r_holder(s0, sort: opt, table: sortedA, n: :array.size(sortedA), temp: [], pid: :undefined)
  end

  defp handle_new_data_chunk2(
         data,
         s0 = r_holder(columns: cols0, source: :ets, temp: tab0)
       ) do
    {tab, cols} = parse_ets_data(data, cols0, tab0)
    r_holder(s0, columns: cols, temp: tab)
  end

  defp handle_new_data_chunk2(data, s0 = r_holder(source: :mnesia, temp: tab)) do
    r_holder(s0, temp: data ++ tab)
  end

  defp parse_ets_data([[rec] | rs], c, tab) do
    parse_ets_data(rs, max(tuple_size(rec), c), [rec | tab])
  end

  defp parse_ets_data([recs | rs], c0, tab0) do
    {tab, cols} = parse_ets_data(recs, c0, tab0)
    parse_ets_data(rs, cols, tab)
  end

  defp parse_ets_data([], cols, tab) do
    {tab, cols}
  end

  defp sort(col, s = r_holder(sort: opt0, table: table0)) do
    {opt, table} = sort(col, opt0, :array.to_list(table0))
    r_holder(s, sort: opt, table: :array.from_list(table))
  end

  defp sort(col, opt = r_opt(sort_key: col, sort_incr: bool), table) do
    {r_opt(opt, sort_incr: not bool), :lists.reverse(table)}
  end

  defp sort(col, s = r_opt(sort_incr: true), table) do
    {r_opt(s, sort_key: col), keysort(col, table)}
  end

  defp sort(col, s = r_opt(sort_incr: false), table) do
    {r_opt(s, sort_key: col), :lists.reverse(keysort(col, table))}
  end

  defp keysort(col, table) do
    sort = fn
      [a0 | _], [b0 | _] ->
        a =
          try do
            :erlang.element(col, a0)
          catch
            _, _ ->
              []
          end

        b =
          try do
            :erlang.element(col, b0)
          catch
            _, _ ->
              []
          end

        case a == b do
          true ->
            a0 <= b0

          false ->
            a < b
        end

      a0, b0 when is_tuple(a0) and is_tuple(b0) ->
        a =
          try do
            :erlang.element(col, a0)
          catch
            _, _ ->
              []
          end

        b =
          try do
            :erlang.element(col, b0)
          catch
            _, _ ->
              []
          end

        case a == b do
          true ->
            a0 <= b0

          false ->
            a < b
        end
    end

    :lists.sort(sort, table)
  end

  defp search(
         [term, -1, true, :term],
         s = r_holder(parent: parent, table: table)
       ) do
    search = fn idx, [tuple | _] ->
      tuple === term and throw(idx)
      tuple
    end

    try do
      :array.map(search, table)
    catch
      index ->
        send(parent, {self(), index})
    else
      _ ->
        send(parent, {self(), false})
    end

    s
  end

  defp search(
         [str, row, dir0, caseSens],
         s = r_holder(parent: parent, n: n, table: table)
       ) do
    opt =
      case caseSens do
        true ->
          []

        false ->
          [:caseless]
      end

    dir =
      case dir0 do
        true ->
          1

        false ->
          -1
      end

    res =
      case :re.compile(str, [:unicode | opt]) do
        {:ok, re} ->
          re_search(row, dir, n, re, table)

        {:error, _} ->
          false
      end

    send(parent, {self(), res})
    r_holder(s, search: res)
  end

  defp re_search(row, dir, n, re, table)
       when row >= 0 and
              row < n do
    [term | _] = :array.get(row, table)
    str = format(term)
    res = :re.run(str, re)

    case res do
      :nomatch ->
        re_search(row + dir, dir, n, re, table)

      {:match, _} ->
        row
    end
  end

  defp re_search(_, _, _, _, _) do
    false
  end

  defp get_row(from, row, col, table) do
    case :array.get(row, table) do
      [object | _] when col === :all ->
        send(from, {self(), format(object)})

      [object | _] when col === :all_multiline ->
        send(from, {self(), :io_lib.format('~tp', [object])})

      [object | _] when col === :term ->
        send(from, {self(), object})

      [object | _] when tuple_size(object) >= col ->
        send(from, {self(), format(:erlang.element(col, object))})

      _ ->
        send(from, {self(), ''})
    end
  end

  defp get_attr(from, row, r_holder(attrs: attrs, search: row)) do
    what = r_attrs(attrs, :searched)
    send(from, {self(), what})
  end

  defp get_attr(from, row, r_holder(table: table, attrs: attrs)) do
    odd = rem(row, 2) > 0

    what =
      case :array.get(row, table) do
        [_ | :deleted] ->
          r_attrs(attrs, :deleted)

        [_ | :changed] when odd ->
          r_attrs(attrs, :changed_odd)

        [_ | :changed] ->
          r_attrs(attrs, :changed_even)

        [_ | :new] when odd ->
          r_attrs(attrs, :new_odd)

        [_ | :new] ->
          r_attrs(attrs, :new_even)

        _ when odd ->
          r_attrs(attrs, :odd)

        _ ->
          r_attrs(attrs, :even)
      end

    send(from, {self(), what})
  end

  defp merge([], new, _Key) do
    for n <- new do
      [n]
    end
  end

  defp merge(old, new, key) do
    merge2(keysort(key, old), keysort(key, new), key)
  end

  defp merge2([[obj | _] | old], [obj | new], key) do
    [[obj] | merge2(old, new, key)]
  end

  defp merge2([[a | op] | old], [b | new], key)
       when :erlang.element(key, a) ==
              :erlang.element(
                key,
                b
              ) do
    case op do
      :deleted ->
        [[b | :new] | merge2(old, new, key)]

      _ ->
        [[b | :changed] | merge2(old, new, key)]
    end
  end

  defp merge2([[a | op] | old], new = [b | _], key)
       when :erlang.element(key, a) <
              :erlang.element(
                key,
                b
              ) do
    case op do
      :deleted ->
        merge2(old, new, key)

      _ ->
        [[a | :deleted] | merge2(old, new, key)]
    end
  end

  defp merge2(old = [[a | _] | _], [b | new], key)
       when :erlang.element(key, a) >
              :erlang.element(
                key,
                b
              ) do
    [[b | :new] | merge2(old, new, key)]
  end

  defp merge2([], new, _Key) do
    for n <- new do
      [n | :new]
    end
  end

  defp merge2(old, [], _Key) do
    :lists.foldl(
      fn
        [_O | :deleted], acc ->
          acc

        [o | _], acc ->
          [[o | :deleted] | acc]
      end,
      [],
      old
    )
  end

  defp delete_row(row, s0 = r_holder(parent: parent)) do
    case delete(row, s0) do
      :ok ->
        send(self(), :refresh)

      {:error, err} ->
        send(parent, {:error, 'Could not delete object: ' ++ err})
    end
  end

  defp delete(
         row,
         r_holder(tabid: id, table: table, source: source, node: node)
       ) do
    [object | _] = :array.get(row, table)

    try do
      case source do
        :ets ->
          true = :rpc.call(node, :ets, :delete_object, [id, object])

        :mnesia ->
          :ok = :rpc.call(node, :mnesia, :dirty_delete_object, [id, object])
      end

      :ok
    catch
      _, _Error ->
        {:error, 'node or table is not available'}
    end
  end

  defp edit_row(row, term, s0 = r_holder(parent: parent)) do
    case delete(row, s0) do
      :ok ->
        case insert(term, s0) do
          :ok ->
            send(self(), :refresh)

          err ->
            send(parent, {:error, err})
        end

      {:error, err} ->
        send(parent, {:error, 'Could not edit object: ' ++ err})
    end
  end

  defp insert(
         object,
         r_holder(tabid: id, source: source, node: node)
       ) do
    try do
      case source do
        :ets ->
          true = :rpc.call(node, :ets, :insert, [id, object])

        :mnesia ->
          :ok = :rpc.call(node, :mnesia, :dirty_write, [id, object])
      end

      :ok
    catch
      _, _Error ->
        {:error, 'node or table is not available'}
    end
  end

  defp column_names(node, type, table) do
    case type do
      :ets ->
        [1, 2]

      :mnesia ->
        attrs = :rpc.call(node, :mnesia, :table_info, [table, :attributes])
        is_list(attrs) or throw(:node_or_table_down)
        ['Record Name' | attrs]
    end
  end

  defp table_id(r_tab(id: :ignore, name: name)) do
    name
  end

  defp table_id(r_tab(id: id)) do
    id
  end

  defp key_pos(_, :mnesia, _) do
    2
  end

  defp key_pos(node, :ets, tabId) do
    keyPos = :rpc.call(node, :ets, :info, [tabId, :keypos])
    is_integer(keyPos) or throw(:node_or_table_down)
    keyPos
  end

  def format(tuple) when is_tuple(tuple) do
    [?{ | format_tuple(tuple, 1, tuple_size(tuple))]
  end

  def format(list) when is_list(list) do
    format_list(list)
  end

  def format(bin)
      when is_binary(bin) and
             byte_size(bin) > 100 do
    :io_lib.format('<<#Bin:~w>>', [byte_size(bin)])
  end

  def format(bin) when is_binary(bin) do
    try do
      true = :io_lib.printable_list(:unicode.characters_to_list(bin))
      :io_lib.format('<<"~ts">>', [bin])
    catch
      _, _ ->
        :io_lib.format('~w', [bin])
    end
  end

  def format(float) when is_float(float) do
    :io_lib.format('~.3g', [float])
  end

  def format(term) do
    :io_lib.format('~tw', [term])
  end

  defp format_tuple(tuple, i, max) when i < max do
    [
      format(:erlang.element(i, tuple)),
      ?,
      | format_tuple(tuple, i + 1, max)
    ]
  end

  defp format_tuple(tuple, max, max) do
    [format(:erlang.element(max, tuple)), ?}]
  end

  defp format_tuple(_Tuple, 1, 0) do
    [?}]
  end

  defp format_list([]) do
    '[]'
  end

  defp format_list(list) do
    case :io_lib.printable_list(list) do
      true ->
        :io_lib.format('"~ts"', [map_printable_list(list)])

      false ->
        [?[ | make_list(list)]
    end
  end

  defp make_list([last]) do
    [format(last), ?]]
  end

  defp make_list([head | tail]) when is_list(tail) do
    [format(head), ?, | make_list(tail)]
  end

  defp make_list([head | tail]) do
    [format(head), ?|, format(tail), ?]]
  end

  defp map_printable_list([?\n | cs]) do
    [?\\, ?n | map_printable_list(cs)]
  end

  defp map_printable_list([?\r | cs]) do
    [?\\, ?r | map_printable_list(cs)]
  end

  defp map_printable_list([?\t | cs]) do
    [?\\, ?t | map_printable_list(cs)]
  end

  defp map_printable_list([?\v | cs]) do
    [?\\, ?v | map_printable_list(cs)]
  end

  defp map_printable_list([?\b | cs]) do
    [?\\, ?b | map_printable_list(cs)]
  end

  defp map_printable_list([?\f | cs]) do
    [?\\, ?f | map_printable_list(cs)]
  end

  defp map_printable_list([?\e | cs]) do
    [?\\, ?e | map_printable_list(cs)]
  end

  defp map_printable_list([]) do
    []
  end

  defp map_printable_list([c | cs]) do
    [c | map_printable_list(cs)]
  end
end
