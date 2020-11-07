defmodule :m_cdv_virtual_list_wx do
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

  Record.defrecord(:r_sort, :sort,
    sort_key: :undefined,
    sort_incr: true
  )

  Record.defrecord(:r_holder, :holder,
    parent: :undefined,
    info: :undefined,
    last_row: :undefined,
    sort: :undefined,
    attrs: :undefined,
    callback: :undefined
  )

  Record.defrecord(:r_state, :state,
    grid: :undefined,
    panel: :undefined,
    detail_wins: [],
    holder: :undefined,
    callback: :undefined,
    trunc_warn: [],
    menu_cols: [],
    menu_items: []
  )

  def start_link(parentWin, callback) do
    :wx_object.start_link(
      {:local, callback},
      :cdv_virtual_list_wx,
      [parentWin, callback, :all],
      []
    )
  end

  def start_link(parentWin, callback, owner) do
    :wx_object.start_link(:cdv_virtual_list_wx, [parentWin, callback, owner], [])
  end

  def start_detail_win(id) do
    case id do
      '<' ++ _ ->
        start_detail_win(id, :process)

      '#Port' ++ _ ->
        start_detail_win(id, :port)

      _ ->
        :io.format('cdv: unknown identifier: ~tp~n', [id])
        :ignore
    end
  end

  def start_detail_win(id, :process) do
    start_detail_win_2(:cdv_proc_cb, id)
  end

  def start_detail_win(id, :port) do
    start_detail_win_2(:cdv_port_cb, id)
  end

  def start_detail_win(id, :node) do
    start_detail_win_2(:cdv_dist_cb, id)
  end

  def start_detail_win(id, :module) do
    start_detail_win_2(:cdv_mod_cb, id)
  end

  def start_detail_win(id, :ets) do
    start_detail_win_2(:cdv_ets_cb, id)
  end

  def start_detail_win(id, :sched) do
    start_detail_win_2(:cdv_sched_cb, id)
  end

  defp start_detail_win_2(callback, id) do
    :wx_object.cast(callback, {:start_detail_win, id})
  end

  def init([parentWin, callback, owner]) do
    panel = :wxPanel.new(parentWin)
    attrs = :observer_lib.create_attrs(panel)
    {holder, tW} = spawn_table_holder(callback, owner, attrs)
    {grid, menuCols} = create_list_box(panel, holder, callback, owner)
    sizer = :wxBoxSizer.new(8)

    :wxSizer.add(sizer, grid, [
      {:flag, 8192 ||| (64 ||| 128 ||| 32 ||| 16)},
      {:proportion, 1},
      {:border, 4}
    ])

    :wxWindow.setSizer(panel, sizer)

    state =
      r_state(
        grid: grid,
        panel: panel,
        holder: holder,
        callback: callback,
        trunc_warn: tW,
        menu_cols: menuCols
      )

    {panel, state}
  end

  defp create_list_box(panel, holder, callback, owner) do
    style = 8192 ||| 32 ||| 512 ||| 2 ||| 1_073_741_824 ||| 2_147_483_648

    listCtrl =
      :wxListCtrl.new(
        panel,
        [
          {:style, style},
          {:onGetItemText,
           fn _, row, col ->
             call(
               holder,
               {:get_row, self(), row, col}
             )
           end},
          {:onGetItemAttr,
           fn _, item ->
             call(
               holder,
               {:get_attr, self(), item}
             )
           end}
        ]
      )

    li = :wxListItem.new()
    scale = :observer_wx.get_scale()

    addListEntry = fn {name, align, defSize}, col ->
      :wxListItem.setText(li, name)
      :wxListItem.setAlign(li, align)
      :wxListCtrl.insertColumn(listCtrl, col, li)
      :wxListCtrl.setColumnWidth(listCtrl, col, defSize * scale)
      col + 1
    end

    listItems = callback.col_spec()
    :lists.foldl(addListEntry, 0, listItems)
    :wxListItem.destroy(li)
    :wxListCtrl.setItemCount(listCtrl, 0)
    :wxListCtrl.connect(listCtrl, :size, [{:skip, true}])
    :wxListCtrl.connect(listCtrl, :command_list_col_click)

    detailCols =
      case (try do
              callback.get_detail_cols(owner)
            catch
              :error, e -> {:EXIT, {e, __STACKTRACE__}}
              :exit, e -> {:EXIT, e}
              e -> e
            end) do
        {dC, doubleClick} when is_list(dC) and dC !== [] ->
          :wxListCtrl.connect(
            listCtrl,
            :command_list_item_right_click
          )

          cond do
            doubleClick ->
              :wxListCtrl.connect(
                listCtrl,
                :command_list_item_activated
              )

            true ->
              :ok
          end

          dC

        _ ->
          []
      end

    {listCtrl, detailCols}
  end

  defp do_start_detail_win(:undefined, state) do
    state
  end

  defp do_start_detail_win(
         id,
         r_state(panel: panel, detail_wins: opened, holder: holder, callback: callback) = state
       ) do
    newOpened =
      case :lists.keyfind(id, 1, opened) do
        false ->
          data = call(holder, {:get_data, self(), id})

          case :cdv_detail_wx.start_link(id, data, panel, callback, :cdv) do
            {:error, _} ->
              opened

            iW ->
              [{id, iW} | opened]
          end

        {_, iW} ->
          :wxFrame.raise(iW)
          opened
      end

    r_state(state, detail_wins: newOpened)
  end

  defp call(holder, what) when is_atom(holder) do
    call(:erlang.whereis(holder), what)
  end

  defp call(holder, what) when is_pid(holder) do
    ref = :erlang.monitor(:process, holder)
    send(holder, what)

    receive do
      {:DOWN, ^ref, _, _, _} ->
        ''

      {^holder, res} ->
        :erlang.demonitor(ref)
        res
    after
      5000 ->
        :io.format('Hanging call ~tp~n', [what])
        ''
    end
  end

  defp call(_, _) do
    ''
  end

  def handle_info(
        {:holder_updated, count},
        state = r_state(grid: grid)
      ) do
    :wxListCtrl.setItemCount(grid, count)
    count > 0 and :wxListCtrl.refreshItems(grid, 0, count - 1)
    {:noreply, state}
  end

  def handle_info(:active, state) do
    :cdv_wx.set_status(r_state(state, :trunc_warn))
    {:noreply, state}
  end

  def handle_info(info, state) do
    :io.format('~p:~p, Unexpected info: ~tp~n', [:cdv_virtual_list_wx, 219, info])
    {:noreply, state}
  end

  def terminate(_Reason, r_state(holder: holder)) do
    send(holder, :stop)
    :ok
  end

  def code_change(_, _, state) do
    {:ok, state}
  end

  def handle_call(
        :new_dump,
        _From,
        r_state(grid: grid, detail_wins: opened, holder: holder, callback: callback) = state
      ) do
    :lists.foreach(
      fn {_Id, iW} ->
        :wxFrame.destroy(iW)
      end,
      opened
    )

    :wxListCtrl.deleteAllItems(grid)
    ref = :erlang.monitor(:process, holder)
    send(holder, :stop)

    receive do
      {:DOWN, ^ref, _, _, _} ->
        :ok
    end

    attrs = :observer_lib.create_attrs(grid)
    {newHolder, tW} = spawn_table_holder(callback, :all, attrs)
    {:reply, :ok, r_state(state, detail_wins: [], holder: newHolder, trunc_warn: tW)}
  end

  def handle_call(msg, _From, state) do
    :io.format('~p:~p: Unhandled call ~tp~n', [:cdv_virtual_list_wx, 242, msg])
    {:reply, :ok, state}
  end

  def handle_cast({:start_detail_win, id}, state) do
    state2 = do_start_detail_win(id, state)
    {:noreply, state2}
  end

  def handle_cast(
        {:detail_win_closed, id},
        r_state(detail_wins: opened) = state
      ) do
    opened2 = :lists.keydelete(id, 1, opened)
    {:noreply, r_state(state, detail_wins: opened2)}
  end

  def handle_cast(msg, state) do
    :io.format('~p:~p: Unhandled cast ~tp~n', [:cdv_virtual_list_wx, 254, msg])
    {:noreply, state}
  end

  def handle_event(
        r_wx(
          id: menuId,
          event: r_wxCommand(type: :command_menu_selected)
        ),
        r_state(menu_items: menuItems) = state
      ) do
    case :lists.keyfind(menuId, 1, menuItems) do
      {^menuId, type, id} ->
        start_detail_win(id, type)

      false ->
        :ok
    end

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
        r_state(panel: panel, holder: holder, menu_cols: menuCols) = state
      ) do
    menu = :wxMenu.new()

    menuItems =
      :lists.flatmap(
        fn {type, col} ->
          menuId = 202 + col

          colText =
            call(
              holder,
              {:get_row, self(), row, col}
            )

          case colText do
            empty when empty == '[]' or empty == '' ->
              []

            _ ->
              what =
                case (try do
                        :erlang.list_to_integer(colText)
                      catch
                        :error, e -> {:EXIT, {e, __STACKTRACE__}}
                        :exit, e -> {:EXIT, e}
                        e -> e
                      end) do
                  nodeId
                  when is_integer(nodeId) and
                         type === :node ->
                    'node ' ++ colText

                  _ ->
                    colText
                end

              text = 'Properties for ' ++ what
              :wxMenu.append(menu, menuId, text)
              [{menuId, type, colText}]
          end
        end,
        menuCols
      )

    case menuItems do
      [] ->
        :wxMenu.destroy(menu)

      _ ->
        :wxWindow.popupMenu(panel, menu)
        :wxMenu.destroy(menu)
    end

    {:noreply, r_state(state, menu_items: menuItems)}
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
        r_wx(
          event:
            r_wxList(
              type: :command_list_item_activated,
              itemIndex: row
            )
        ),
        r_state(holder: holder, menu_cols: menuCols) = state
      ) do
    case menuCols do
      [{type, _} | _] ->
        id = call(holder, {:get_row, self(), row, :id})
        start_detail_win(id, type)

      _ ->
        :ignore
    end

    {:noreply, state}
  end

  def handle_event(event, state) do
    :io.format('~p:~p: handle event ~tp\n', [:cdv_virtual_list_wx, 328, event])
    {:noreply, state}
  end

  defp spawn_table_holder(callback, owner, attrs) do
    {info, tW} = callback.get_info(owner)
    parent = self()

    holder =
      case owner do
        :all ->
          name = :erlang.list_to_atom(:erlang.atom_to_list(callback) ++ '__holder')

          spawn_link(fn ->
            :erlang.register(name, self())
            init_table_holder(parent, attrs, callback, info)
          end)

          name

        _ ->
          spawn_link(fn ->
            init_table_holder(parent, attrs, callback, info)
          end)
      end

    {holder, tW}
  end

  defp init_table_holder(parent, attrs, callback, infoList0) do
    sort = r_sort(sort_key: callback.col_to_elem(:id))
    {_Sort, infoList} = do_sort(sort, infoList0)
    info = :array.from_list(infoList)
    nRows = :array.size(info)
    send(parent, {:holder_updated, nRows})

    table_holder(
      r_holder(parent: parent, info: info, sort: sort, attrs: attrs, callback: callback)
    )
  end

  defp table_holder(r_holder(callback: callback, attrs: attrs, info: info) = s0) do
    receive do
      _M = {:get_row, from, row, col} ->
        state = get_row(from, row, col, s0)
        table_holder(state)

      _M = {:get_attr, from, row} ->
        get_attr(from, row, attrs)
        table_holder(s0)

      _M = {:change_sort, col} ->
        state = change_sort(callback.col_to_elem(col), s0)
        table_holder(state)

      _M = {:get_data, from, id} ->
        search_id(from, id, callback, info)
        table_holder(s0)

      :stop ->
        :ok

      what ->
        :io.format('Table holder got ~tp~n', [what])
        table_holder(s0)
    end
  end

  defp search_id(from, id, callback, info) do
    find = fn _, rowInfo, _ ->
      search_id(callback, rowInfo, id)
    end

    res =
      try do
        :array.foldl(find, :not_found, info)
      catch
        data ->
          data
      end

    send(from, {self(), res})
    :ok
  end

  defp search_id(callback, rowInfo, id) do
    case :observer_lib.to_str(get_cell_data(callback, :id, rowInfo)) do
      ^id ->
        throw(rowInfo)

      _Str ->
        :not_found
    end
  end

  defp change_sort(
         col,
         s0 = r_holder(parent: parent, info: info0, sort: sort0)
       ) do
    nRows = :array.size(info0)
    infoList0 = :array.to_list(info0)
    {sort, infoList} = sort(col, sort0, infoList0)
    info = :array.from_list(infoList)
    send(parent, {:holder_updated, nRows})
    r_holder(s0, info: info, last_row: :undefined, sort: sort)
  end

  defp sort(col, opt = r_sort(sort_key: col, sort_incr: bool), table) do
    do_sort(r_sort(opt, sort_incr: not bool), table)
  end

  defp sort(col, sort, table) do
    do_sort(r_sort(sort, sort_key: col, sort_incr: true), table)
  end

  defp do_sort(
         sort = r_sort(sort_key: col, sort_incr: true),
         table
       ) do
    {sort, :lists.keysort(col, table)}
  end

  defp do_sort(
         sort = r_sort(sort_key: col, sort_incr: false),
         table
       ) do
    {sort, :lists.reverse(:lists.keysort(col, table))}
  end

  defp get_cell_data(callback, colNo, rowInfo) do
    case :erlang.element(
           callback.col_to_elem(colNo),
           rowInfo
         ) do
      :undefined ->
        ''

      cell ->
        try do
          callback.format(cell)
        catch
          :error, :undef ->
            cell
        end
    end
  end

  defp get_row(
         from,
         row,
         col,
         r_holder(
           callback: callback,
           last_row: {row, rowInfo}
         ) = state
       ) do
    data = get_cell_data(callback, col, rowInfo)
    send(from, {self(), :observer_lib.to_str(data)})
    state
  end

  defp get_row(from, row, col, r_holder(callback: callback, info: info) = s0) do
    {data, state} =
      case row >= :array.size(info) do
        true ->
          {'', s0}

        false ->
          rowInfo = :array.get(row, info)
          cellData = get_cell_data(callback, col, rowInfo)
          {cellData, r_holder(s0, last_row: {row, rowInfo})}
      end

    send(from, {self(), :observer_lib.to_str(data)})
    state
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
