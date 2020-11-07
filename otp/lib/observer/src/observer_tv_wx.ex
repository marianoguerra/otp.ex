defmodule :m_observer_tv_wx do
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

  Record.defrecord(:r_opts, :opts, type: :ets, sys_hidden: true, unread_hidden: true)

  Record.defrecord(:r_sort, :sort,
    sort_incr: true,
    sort_key: 2
  )

  Record.defrecord(:r_state, :state,
    parent: :undefined,
    grid: :undefined,
    panel: :undefined,
    node: node(),
    opts: :EFE_TODO_NESTED_RECORD,
    holder: :undefined,
    selected: :undefined,
    timer: :undefined
  )

  def start_link(notebook, parent, config) do
    :wx_object.start_link(:observer_tv_wx, [notebook, parent, config], [])
  end

  def init([notebook, parent, config]) do
    panel = :wxPanel.new(notebook)
    sizer = :wxBoxSizer.new(8)

    opts =
      r_opts(
        type: :maps.get(:type, config, :ets),
        sys_hidden: :maps.get(:sys_hidden, config, true),
        unread_hidden: :maps.get(:unread_hidden, config, true)
      )

    style = 32 ||| 512 ||| 8192 ||| 2
    self = self()
    attrs = :observer_lib.create_attrs(panel)

    holder =
      spawn_link(fn ->
        init_table_holder(self, attrs)
      end)

    cBs = [
      {:onGetItemText,
       fn _, item, col ->
         get_row(holder, item, col)
       end},
      {:onGetItemAttr,
       fn _, item ->
         get_attr(holder, item)
       end}
    ]

    grid =
      :wxListCtrl.new(
        panel,
        [[{:winid, 500}, {:style, style}] | cBs]
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
      {'Table Name', 0, scale * 200},
      {'Objects', 1, scale * 100},
      {'Size (kB)', 1, scale * 100},
      {'Owner Pid', 2, scale * 150},
      {'Owner Name', 0, scale * 200},
      {'Table Id', 0, scale * 250}
    ]

    :lists.foldl(addListEntry, 0, listItems)
    :wxListItem.destroy(li)
    :wxListCtrl.connect(grid, :command_list_item_activated)

    :wxListCtrl.connect(
      grid,
      :command_list_item_right_click
    )

    :wxListCtrl.connect(grid, :command_list_item_selected)
    :wxListCtrl.connect(grid, :command_list_col_click)
    :wxListCtrl.connect(grid, :size, [{:skip, true}])
    :wxWindow.setFocus(grid)

    {panel,
     r_state(grid: grid, parent: parent, panel: panel, opts: opts, timer: config, holder: holder)}
  end

  def handle_event(
        r_wx(id: 401),
        state = r_state(holder: holder, node: node, opts: opts)
      ) do
    tables = get_tables(node, opts)
    send(holder, {:refresh, tables})
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
        state = r_state(holder: holder)
      ) do
    send(holder, {:sort, col})
    {:noreply, state}
  end

  def handle_event(
        r_wx(id: id),
        state = r_state(node: node, holder: holder, grid: grid, opts: opt0)
      )
      when id >= 403 and id <= 406 do
    opt =
      case id do
        403 ->
          r_opts(opt0, type: :ets)

        404 ->
          r_opts(opt0, type: :mnesia)

        405 ->
          r_opts(opt0, unread_hidden: not r_opts(opt0, :unread_hidden))

        406 ->
          r_opts(opt0, sys_hidden: not r_opts(opt0, :sys_hidden))
      end

    case get_tables2(node, opt) do
      error = {:error, _} ->
        id === 404 and :wxMenuBar.check(:observer_wx.get_menubar(), 403, true)
        send(self(), error)
        {:noreply, state}

      tables ->
        send(holder, {:refresh, tables})
        :wxWindow.setFocus(grid)
        {:noreply, r_state(state, opts: opt)}
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
              type: :command_list_item_activated,
              itemIndex: index
            )
        ),
        state = r_state(holder: holder, node: node, opts: r_opts(type: type), grid: grid)
      ) do
    case get_table(holder, index) do
      r_tab(protection: :private) ->
        send(self(), {:error, 'Table has \'private\' protection and cannot be read'})

      r_tab() = table ->
        :observer_tv_table.start_link(
          grid,
          [{:node, node}, {:type, type}, {:table, table}]
        )

      _ ->
        :ignore
    end

    {:noreply, state}
  end

  def handle_event(
        r_wx(event: r_wxList(type: :command_list_item_right_click)),
        state = r_state(panel: panel)
      ) do
    menu = :wxMenu.new()
    :wxMenu.append(menu, 407, 'Table info')
    :wxMenu.append(menu, 408, 'Show Table Content')
    :wxWindow.popupMenu(panel, menu)
    :wxMenu.destroy(menu)
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
        state = r_state(holder: holder)
      ) do
    send(holder, {:selected, index})
    {:noreply, r_state(state, selected: index)}
  end

  def handle_event(
        r_wx(id: 407),
        state =
          r_state(holder: holder, grid: grid, node: node, opts: r_opts(type: type), selected: sel)
      ) do
    case sel do
      :undefined ->
        {:noreply, state}

      r when is_integer(r) ->
        table = get_table(holder, sel)
        display_table_info(grid, node, type, table)
        {:noreply, state}
    end
  end

  def handle_event(
        r_wx(id: 408),
        state =
          r_state(holder: holder, grid: grid, node: node, opts: r_opts(type: type), selected: sel)
      ) do
    case sel do
      :undefined ->
        {:noreply, state}

      r when is_integer(r) ->
        case get_table(holder, r) do
          r_tab(protection: :private) ->
            send(self(), {:error, 'Table has \'private\' protection and cannot be read'})

          r_tab() = table ->
            :observer_tv_table.start_link(
              grid,
              [{:node, node}, {:type, type}, {:table, table}]
            )

          _ ->
            :ignore
        end

        {:noreply, state}
    end
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

  def handle_call(:get_config, _, r_state(timer: timer, opts: opt) = state) do
    r_opts(type: type, sys_hidden: sys, unread_hidden: unread) = opt
    conf0 = :observer_lib.timer_config(timer)
    conf = %{conf0 | :type => type, :sys_hidden => sys, :unread_hidden => unread}
    {:reply, conf, state}
  end

  def handle_call(event, from, _State) do
    :erlang.error({:unhandled_call, event, from})
  end

  def handle_cast(event, _State) do
    :erlang.error({:unhandled_cast, event})
  end

  def handle_info(
        :refresh_interval,
        state = r_state(holder: holder, node: node, opts: opt)
      ) do
    tables = get_tables(node, opt)
    send(holder, {:refresh, tables})
    {:noreply, state}
  end

  def handle_info(
        {:active, node},
        state = r_state(parent: parent, holder: holder, grid: grid, opts: opt0, timer: timer0)
      ) do
    {tables, opt} =
      case r_opts(opt0, :type) === :mnesia and
             get_tables2(
               node,
               opt0
             ) do
        ts when is_list(ts) ->
          {ts, opt0}

        _ ->
          opt1 = r_opts(opt0, type: :ets)
          {get_tables(node, opt1), opt1}
      end

    send(holder, {:refresh, tables})
    :wxWindow.setFocus(grid)
    create_menus(parent, opt)
    timer = :observer_lib.start_timer(timer0, 10)
    {:noreply, r_state(state, node: node, timer: timer, opts: opt)}
  end

  def handle_info(:not_active, state = r_state(timer: timer0)) do
    timer = :observer_lib.stop_timer(timer0)
    {:noreply, r_state(state, timer: timer)}
  end

  def handle_info(
        {:error, error},
        r_state(panel: panel, opts: opt) = state
      ) do
    str = :io_lib.format('ERROR: ~ts~n', [error])
    :observer_lib.display_info_dialog(panel, str)

    case r_opts(opt, :type) do
      :mnesia ->
        :wxMenuBar.check(:observer_wx.get_menubar(), 403, true)

      _ ->
        :ok
    end

    {:noreply, r_state(state, opts: r_opts(opt, type: :ets))}
  end

  def handle_info({:refresh, min, min}, state = r_state(grid: grid)) do
    :wxListCtrl.setItemCount(grid, min + 1)
    :wxListCtrl.refreshItem(grid, min)
    :observer_wx.set_status(:io_lib.format('Tables: ~w', [min + 1]))
    {:noreply, state}
  end

  def handle_info({:refresh, min, max}, state = r_state(grid: grid)) do
    :wxListCtrl.setItemCount(grid, max + 1)
    max > 0 and :wxListCtrl.refreshItems(grid, min, max)
    :observer_wx.set_status(:io_lib.format('Tables: ~w', [max + 1]))
    {:noreply, state}
  end

  def handle_info(
        {:selected, new, size},
        r_state(grid: grid, selected: old) = state
      ) do
    cond do
      is_integer(old) and old < size ->
        :wxListCtrl.setItemState(grid, old, 0, 4)

      true ->
        :ignore
    end

    cond do
      is_integer(new) ->
        :wxListCtrl.setItemState(grid, new, 65535, 4)
        :wxListCtrl.ensureVisible(grid, new)

      true ->
        :ignore
    end

    {:noreply, r_state(state, selected: new)}
  end

  def handle_info(_Event, state) do
    {:noreply, state}
  end

  def terminate(_Event, r_state(holder: holder)) do
    send(holder, :stop)
    :ok
  end

  def code_change(_, _, state) do
    state
  end

  defp create_menus(
         parent,
         r_opts(sys_hidden: sys, unread_hidden: unR, type: type)
       ) do
    menuEntries = [
      {'View',
       [
         r_create_menu(id: 407, text: 'Table information\tCtrl-I'),
         :separator,
         r_create_menu(id: 403, text: '&Ets Tables', type: :radio, check: type == :ets),
         r_create_menu(id: 404, text: '&Mnesia Tables', type: :radio, check: type == :mnesia),
         :separator,
         r_create_menu(id: 405, text: 'View &Unreadable Tables', type: :check, check: not unR),
         r_create_menu(id: 406, text: 'View &System Tables', type: :check, check: not sys),
         :separator,
         r_create_menu(id: 401, text: 'Refresh\tCtrl-R'),
         r_create_menu(id: 402, text: 'Refresh Interval...')
       ]}
    ]

    :observer_wx.create_menus(parent, menuEntries)
  end

  defp get_tables(node, opts) do
    case get_tables2(node, opts) do
      error = {:error, _} ->
        send(self(), error)
        []

      res ->
        res
    end
  end

  defp get_tables2(
         node,
         r_opts(type: type, sys_hidden: sys, unread_hidden: unread)
       ) do
    args = [type, [{:sys_hidden, sys}, {:unread_hidden, unread}]]

    case :rpc.call(node, :observer_backend, :get_table_list, args) do
      {:badrpc, error} ->
        {:error, error}

      error = {:error, _} ->
        error

      result ->
        for tab <- result do
          list_to_tabrec(tab)
        end
    end
  end

  defp col2key(0) do
    r_tab(:name)
  end

  defp col2key(1) do
    r_tab(:size)
  end

  defp col2key(2) do
    r_tab(:memory)
  end

  defp col2key(3) do
    r_tab(:owner)
  end

  defp col2key(4) do
    r_tab(:reg_name)
  end

  defp col2key(5) do
    r_tab(:id)
  end

  defp list_to_tabrec(pL) do
    r_tab(
      name: :proplists.get_value(:name, pL),
      id: :proplists.get_value(:id, pL, :ignore),
      size: :proplists.get_value(:size, pL, 0),
      memory: :proplists.get_value(:memory, pL, 0),
      owner: :proplists.get_value(:owner, pL),
      reg_name: :proplists.get_value(:reg_name, pL),
      protection: :proplists.get_value(:protection, pL, :public),
      type: :proplists.get_value(:type, pL, :set),
      keypos: :proplists.get_value(:keypos, pL, 1),
      heir: :proplists.get_value(:heir, pL, :none),
      compressed: :proplists.get_value(:compressed, pL, false),
      fixed: :proplists.get_value(:fixed, pL, false),
      storage: :proplists.get_value(:storage, pL),
      index: :proplists.get_value(:index, pL)
    )
  end

  def display_table_info(parent0, node, source, table) do
    parent = :observer_lib.get_wx_parent(parent0)
    title = 'Table Info: ' ++ :erlang.atom_to_list(r_tab(table, :name))
    frame = :wxMiniFrame.new(parent, -1, title, [{:style, 2048 ||| 536_870_912 ||| 4096 ||| 64}])

    idInfo =
      {'Identification and Owner',
       [
         {'Name', r_tab(table, :name)},
         {'Id',
          case r_tab(table, :id) do
            :ignore ->
              r_tab(table, :name)

            id ->
              id
          end},
         {'Named table', r_tab(table, :id) == :ignore},
         {'Owner', r_tab(table, :owner)},
         {'Owner Name',
          case r_tab(table, :reg_name) do
            :ignore ->
              '-'

            id ->
              id
          end},
         {'Heir', r_tab(table, :heir)},
         {'Node', node}
       ]}

    mnesiaSettings =
      case source do
        :ets ->
          []

        :mnesia ->
          [
            {'Local storage type',
             case r_tab(table, :storage) do
               :unknown ->
                 'Not available'

               sT ->
                 sT
             end},
            {'Index positions', list_to_strings(r_tab(table, :index))}
          ]
      end

    settings =
      {'Settings',
       [
         [
           {'Source', source},
           {'Key Position', r_tab(table, :keypos)},
           {'Table Type', r_tab(table, :type)},
           {'Protection Mode', r_tab(table, :protection)},
           {'Fixed', r_tab(table, :fixed)}
         ]
         | mnesiaSettings
       ]}

    memory =
      {'Memory Usage',
       [
         {'Number of objects', r_tab(table, :size)},
         {'Memory allocated', {:bytes, r_tab(table, :memory)}},
         {'Compressed', r_tab(table, :compressed)}
       ]}

    {_, sizer, _} =
      :observer_lib.display_info(
        frame,
        [idInfo, settings, memory]
      )

    :wxSizer.setSizeHints(sizer, frame)
    :wxWindow.setMinSize(frame, {300, -1})
    :wxFrame.center(frame)
    :wxFrame.show(frame)
  end

  defp list_to_strings([]) do
    'None'
  end

  defp list_to_strings([a]) do
    :erlang.integer_to_list(a)
  end

  defp list_to_strings([a | b]) do
    :erlang.integer_to_list(a) ++ ' ,' ++ list_to_strings(b)
  end

  defp get_table(table, item) do
    get_row(table, item, :all)
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

  Record.defrecord(:r_holder, :holder,
    node: :undefined,
    parent: :undefined,
    pid: :undefined,
    tabs: :array.new(),
    sort: :EFE_TODO_NESTED_RECORD,
    attrs: :undefined,
    sel: :undefined
  )

  defp init_table_holder(parent, attrs) do
    send(parent, :refresh)
    table_holder(r_holder(node: node(), parent: parent, attrs: attrs))
  end

  defp table_holder(s0 = r_holder(parent: parent, tabs: tabs0, sel: sel0)) do
    receive do
      {:get_attr, from, row} ->
        get_attr(from, row, s0)
        table_holder(s0)

      {:get_row, from, row, col} ->
        get_row(from, row, col, tabs0)
        table_holder(s0)

      {:sort, col} ->
        sTab = get_sel(sel0, tabs0)
        send(parent, {:refresh, 0, :array.size(tabs0) - 1})
        s1 = sort(col2key(col), s0)
        sel = sel_idx(sTab, r_holder(s1, :tabs))
        send(parent, {:selected, sel, :array.size(tabs0)})
        table_holder(r_holder(s1, sel: sel))

      {:refresh, tabs1} ->
        sTab = get_sel(sel0, tabs0)

        tabs =
          case r_holder(s0, :sort) do
            r_sort(sort_incr: false, sort_key: col) ->
              :array.from_list(
                :lists.reverse(
                  :lists.keysort(
                    col,
                    tabs1
                  )
                )
              )

            r_sort(sort_key: col) ->
              :array.from_list(:lists.keysort(col, tabs1))
          end

        send(parent, {:refresh, 0, :array.size(tabs) - 1})
        sel = sel_idx(sTab, tabs)
        send(parent, {:selected, sel, :array.size(tabs)})
        table_holder(r_holder(s0, tabs: tabs, sel: sel))

      {:selected, sel} ->
        table_holder(r_holder(s0, sel: sel))

      :stop ->
        :ok

      what ->
        :io.format('Table holder got ~tp~n', [what])
        send(parent, {:refresh, 0, :array.size(tabs0) - 1})
        table_holder(s0)
    end
  end

  defp get_sel(:undefined, _Tabs) do
    :undefined
  end

  defp get_sel(idx, tabs) do
    :array.get(idx, tabs)
  end

  defp sel_idx(:undefined, _Tabs) do
    :undefined
  end

  defp sel_idx(tab, tabs) do
    find = fn idx, c, acc ->
      c === tab and throw({:found, idx})
      acc
    end

    try do
      :array.foldl(find, :undefined, tabs)
    catch
      {:found, idx} ->
        idx
    end
  end

  defp sort(
         col,
         r_holder(
           sort: r_sort(sort_key: col, sort_incr: incr) = s,
           tabs: table0
         ) = h
       ) do
    table = :lists.reverse(:array.to_list(table0))

    r_holder(h,
      sort: r_sort(s, sort_incr: not incr),
      tabs: :array.from_list(table)
    )
  end

  defp sort(
         col,
         r_holder(sort: r_sort(sort_incr: incr) = s, tabs: table0) = h
       ) do
    table =
      case incr do
        false ->
          :lists.reverse(
            :lists.keysort(
              col,
              :array.to_list(table0)
            )
          )

        true ->
          :lists.keysort(col, :array.to_list(table0))
      end

    r_holder(h,
      sort: r_sort(s, sort_key: col),
      tabs: :array.from_list(table)
    )
  end

  defp get_row(from, row, col, table) do
    object = :array.get(row, table)
    send(from, {self(), get_col(col, object)})
  end

  defp get_col(:all, rec) do
    rec
  end

  defp get_col(2, r_tab() = rec) do
    :observer_lib.to_str(
      div(
        :erlang.element(
          r_tab(:memory),
          rec
        ),
        1024
      )
    )
  end

  defp get_col(col, r_tab() = rec) do
    case :erlang.element(col2key(col), rec) do
      :ignore ->
        ''

      val ->
        :observer_lib.to_str(val)
    end
  end

  defp get_col(_, _) do
    ''
  end

  defp get_attr(from, row, r_holder(tabs: tabs, attrs: attrs)) do
    evenOdd =
      case rem(row, 2) > 0 do
        true ->
          r_attrs(attrs, :odd)

        false ->
          r_attrs(attrs, :even)
      end

    what =
      try do
        :array.get(row, tabs)
      catch
        _ ->
          evenOdd
      else
        r_tab(protection: :private) ->
          r_attrs(attrs, :deleted)

        _ ->
          evenOdd
      end

    send(from, {self(), what})
  end
end
