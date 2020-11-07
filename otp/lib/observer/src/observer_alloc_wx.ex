defmodule :m_observer_alloc_wx do
  use Bitwise

  import :observer_perf_wx,
    only: [
      add_data: 5,
      interval_dialog: 2,
      make_win: 4,
      precalc: 4,
      refresh_panel: 4,
      setup_graph_drawing: 1
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
    time: :EFE_TODO_NESTED_RECORD,
    active: false,
    parent: :undefined,
    wins: :undefined,
    mem: :undefined,
    samples: :undefined,
    max: :undefined,
    panel: :undefined,
    paint: :undefined,
    appmon: :undefined,
    async: :undefined
  )

  def start_link(notebook, parent, config) do
    :wx_object.start_link(:observer_alloc_wx, [notebook, parent, config], [])
  end

  def init([notebook, parent, config]) do
    try do
      topP = :wxPanel.new(notebook)
      main = :wxBoxSizer.new(8)
      panel = :wxPanel.new(topP)
      gSzr = :wxBoxSizer.new(8)
      borderFlags = 16 ||| 32
      carrier = make_win(:alloc, panel, gSzr, borderFlags ||| 64)
      utilz = make_win(:utilz, panel, gSzr, borderFlags)
      :wxWindow.setSizer(panel, gSzr)
      :wxSizer.add(main, panel, [{:flag, 8192}, {:proportion, 2}])
      memWin = create_mem_info(topP)

      :wxSizer.add(main, memWin, [
        {:flag, 8192 ||| borderFlags ||| 128},
        {:proportion, 1},
        {:border, 5}
      ])

      :wxWindow.setSizer(topP, main)
      windows = [carrier, utilz]
      paintInfo = setup_graph_drawing(windows)

      {topP,
       r_state(
         parent: parent,
         panel: panel,
         wins: windows,
         mem: memWin,
         paint: paintInfo,
         time: setup_time(config),
         max: %{}
       )}
    catch
      _, err ->
        :io.format('~p crashed ~tp: ~tp~n', [:observer_alloc_wx, err, __STACKTRACE__])
        {:stop, err}
    end
  end

  defp setup_time(config) do
    freq = :maps.get(:fetch, config, 1)
    r_ti(disp: 10 / freq, fetch: freq, secs: :maps.get(:secs, config, 60))
  end

  def handle_event(
        r_wx(
          id: 102,
          event: r_wxCommand(type: :command_menu_selected)
        ),
        r_state(
          active: active,
          panel: panel,
          appmon: old,
          wins: wins0,
          time: r_ti(fetch: f0) = ti0
        ) = state
      ) do
    case interval_dialog(panel, ti0) do
      ^ti0 ->
        {:noreply, state}

      r_ti(fetch: ^f0) = ti ->
        wins =
          for w <- wins0 do
            r_win(w, max: :undefined)
          end

        {:noreply, precalc(r_state(state, time: ti, wins: wins))}

      ti when not active ->
        {:noreply, r_state(state, time: ti)}

      ti ->
        {:noreply, restart_fetcher(old, r_state(state, time: ti))}
    end
  end

  def handle_event(
        r_wx(event: r_wxCommand(type: :command_menu_selected)),
        state = r_state()
      ) do
    {:noreply, state}
  end

  def handle_event(event, _State) do
    :erlang.error({:unhandled_event, event})
  end

  def handle_sync_event(
        r_wx(obj: panel, event: r_wxPaint()),
        _,
        r_state(active: active, time: ti, paint: paint, wins: windows)
      ) do
    win = :lists.keyfind(panel, r_win(:panel), windows)
    refresh_panel(active, win, ti, paint)
    :ok
  end

  def handle_call(:get_config, _, r_state(time: ti) = state) do
    r_ti(fetch: fetch, secs: range) = ti
    {:reply, %{:fetch => fetch, :secs => range}, state}
  end

  def handle_call(event, from, _State) do
    :erlang.error({:unhandled_call, event, from})
  end

  def handle_cast(event, _State) do
    :erlang.error({:unhandled_cast, event})
  end

  def handle_info(
        {key, {:promise_reply, {:badrpc, _}}},
        r_state(async: key) = state
      ) do
    {:noreply, r_state(state, active: false, appmon: :undefined)}
  end

  def handle_info(
        {key, {:promise_reply, sysInfo}},
        r_state(
          async: key,
          samples: data,
          max: max0,
          active: active,
          wins: wins0,
          time: r_ti(tick: tick, disp: disp0) = ti
        ) = s0
      ) do
    disp = trunc(disp0)
    next = max(tick - disp, 0)
    :erlang.send_after(div(1000, 10), self(), {:refresh, next})
    info = alloc_info(sysInfo)
    max = :lists.foldl(&calc_max/2, max0, info)
    {wins, samples} = add_data(info, data, wins0, ti, active)

    s1 =
      r_state(s0,
        time: r_ti(ti, tick: next),
        wins: wins,
        samples: samples,
        max: max,
        async: :undefined
      )

    cond do
      active ->
        update_alloc(s0, info, max)
        state = precalc(s1)
        {:noreply, state}

      true ->
        {:noreply, s1}
    end
  end

  def handle_info(
        {:refresh, seq},
        state = r_state(panel: panel, appmon: node, time: r_ti(tick: seq, disp: dispF) = ti)
      )
      when seq + 1 < dispF * 1.5 do
    next = seq + 1

    r_state(state, :active) and
      try do
        :wxWindow.refresh(panel)
      catch
        :error, e -> {:EXIT, {e, __STACKTRACE__}}
        :exit, e -> {:EXIT, e}
        e -> e
      end

    :erlang.send_after(div(1000, 10), self(), {:refresh, next})

    cond do
      seq === trunc(dispF) - 1 ->
        req = request_info(node)
        {:noreply, r_state(state, time: r_ti(ti, tick: next), async: req)}

      true ->
        {:noreply, r_state(state, time: r_ti(ti, tick: next))}
    end
  end

  def handle_info({:refresh, _S}, r_state() = state) do
    {:noreply, state}
  end

  def handle_info(
        {:active, node},
        state = r_state(parent: parent, panel: panel, appmon: old)
      ) do
    create_menus(parent, [])

    try do
      ^node = old
      :wxWindow.refresh(panel)
      {:noreply, precalc(r_state(state, active: true))}
    catch
      _, _ ->
        {:noreply, restart_fetcher(node, state)}
    end
  end

  def handle_info(:not_active, state = r_state(appmon: _Pid)) do
    {:noreply, r_state(state, active: false)}
  end

  def handle_info({:EXIT, old, _}, state = r_state(appmon: old)) do
    {:noreply, r_state(state, active: false, appmon: :undefined)}
  end

  def handle_info(_Event, state) do
    {:noreply, state}
  end

  def terminate(_Event, r_state()) do
    :ok
  end

  def code_change(_, _, state) do
    state
  end

  defp request_info(node) do
    replyTo = self()

    spawn(fn ->
      res = :rpc.call(node, :observer_backend, :sys_info, [])
      send(replyTo, {self(), {:promise_reply, res}})
    end)
  end

  defp restart_fetcher(
         node,
         r_state(panel: panel, wins: wins0, time: ti) = state
       ) do
    case :rpc.call(node, :observer_backend, :sys_info, []) do
      {:badrpc, _} ->
        state

      sysInfo ->
        info = alloc_info(sysInfo)
        max = :lists.foldl(&calc_max/2, %{}, info)
        {wins, samples} = add_data(info, {0, :queue.new()}, wins0, ti, true)
        :erlang.send_after(div(1000, 10), self(), {:refresh, 0})
        :wxWindow.refresh(panel)

        precalc(
          r_state(state,
            active: true,
            appmon: node,
            time: r_ti(ti, tick: 0),
            wins: wins,
            samples: samples,
            max: max
          )
        )
    end
  end

  defp precalc(r_state(samples: data0, paint: paint, time: ti, wins: wins0) = state) do
    wins =
      for win <- wins0 do
        precalc(ti, data0, paint, win)
      end

    r_state(state, wins: wins)
  end

  defp calc_max({name, _, cs}, max0) do
    case :maps.get(name, max0, 0) do
      value when value < cs ->
        %{max0 | name => cs}

      _V ->
        max0
    end
  end

  defp update_alloc(r_state(mem: grid), fields, max) do
    :wxWindow.freeze(grid)
    last = :wxListCtrl.getItemCount(grid)

    update = fn {name, bS, cS}, row ->
      row >= last and :wxListCtrl.insertItem(grid, row, '')
      maxV = :maps.get(name, max, cS)
      :wxListCtrl.setItem(grid, row, 0, :observer_lib.to_str(name))
      :wxListCtrl.setItem(grid, row, 1, :observer_lib.to_str(div(bS, 1024)))
      :wxListCtrl.setItem(grid, row, 2, :observer_lib.to_str(div(cS, 1024)))
      :wxListCtrl.setItem(grid, row, 3, :observer_lib.to_str(div(maxV, 1024)))
      row + 1
    end

    :wx.foldl(update, 0, fields)
    :wxWindow.thaw(grid)
    fields
  end

  defp alloc_info(sysInfo) do
    allocInfo = :proplists.get_value(:alloc_info, sysInfo, [])
    alloc_info(allocInfo, [], 0, 0, true)
  end

  defp alloc_info([{type, instances} | allocators], typeAcc, totalBS, totalCS, includeTotal) do
    {bS, cS, newTotalBS, newTotalCS, newIncludeTotal} =
      sum_alloc_instances(instances, 0, 0, totalBS, totalCS)

    alloc_info(
      allocators,
      [{type, bS, cS} | typeAcc],
      newTotalBS,
      newTotalCS,
      includeTotal and newIncludeTotal
    )
  end

  defp alloc_info([], typeAcc, totalBS, totalCS, includeTotal) do
    types =
      for x = {_, bS, cS} <- typeAcc,
          bS > 0 or cS > 0 do
        x
      end

    case includeTotal do
      true ->
        [{:total, totalBS, totalCS} | :lists.reverse(types)]

      false ->
        :lists.reverse(types)
    end
  end

  defp sum_alloc_instances(false, bS, cS, totalBS, totalCS) do
    {bS, cS, totalBS, totalCS, false}
  end

  defp sum_alloc_instances([{_, _, data} | instances], bS, cS, totalBS, totalCS) do
    {newBS, newCS, newTotalBS, newTotalCS} =
      sum_alloc_one_instance(data, bS, cS, totalBS, totalCS)

    sum_alloc_instances(instances, newBS, newCS, newTotalBS, newTotalCS)
  end

  defp sum_alloc_instances([], bS, cS, totalBS, totalCS) do
    {bS, cS, totalBS, totalCS, true}
  end

  defp sum_alloc_one_instance(
         [
           {_, [{:blocks, typedBlocks}, {:carriers_size, cS, _, _}]}
           | rest
         ],
         oldBS,
         oldCS,
         totalBS,
         totalCS
       ) do
    bS = sum_alloc_block_list(typedBlocks, 0)
    sum_alloc_one_instance(rest, oldBS + bS, oldCS + cS, totalBS + bS, totalCS + cS)
  end

  defp sum_alloc_one_instance(
         [
           {_, [{:blocks_size, bS, _, _}, {:carriers_size, cS, _, _}]}
           | rest
         ],
         oldBS,
         oldCS,
         totalBS,
         totalCS
       ) do
    sum_alloc_one_instance(rest, oldBS + bS, oldCS + cS, totalBS + bS, totalCS + cS)
  end

  defp sum_alloc_one_instance([_ | rest], bS, cS, totalBS, totalCS) do
    sum_alloc_one_instance(rest, bS, cS, totalBS, totalCS)
  end

  defp sum_alloc_one_instance([], bS, cS, totalBS, totalCS) do
    {bS, cS, totalBS, totalCS}
  end

  defp sum_alloc_block_list(
         [{_Type, [{:size, current, _, _}]} | rest],
         acc
       ) do
    sum_alloc_block_list(rest, current + acc)
  end

  defp sum_alloc_block_list([{_Type, [{:size, current}]} | rest], acc) do
    sum_alloc_block_list(rest, current + acc)
  end

  defp sum_alloc_block_list([_ | rest], acc) do
    sum_alloc_block_list(rest, acc)
  end

  defp sum_alloc_block_list([], acc) do
    acc
  end

  defp create_mem_info(parent) do
    style = 32 ||| 8192 ||| 2 ||| 1
    grid = :wxListCtrl.new(parent, [{:style, style}])
    li = :wxListItem.new()
    scale = :observer_wx.get_scale()

    addListEntry = fn {name, align, defSize}, col ->
      :wxListItem.setText(li, name)
      :wxListItem.setAlign(li, align)
      :wxListCtrl.insertColumn(grid, col, li)
      :wxListCtrl.setColumnWidth(grid, col, defSize * scale)
      col + 1
    end

    listItems = [
      {'Allocator Type', 0, 200},
      {'Block size (kB)', 1, 150},
      {'Carrier size (kB)', 1, 150},
      {'Max Carrier size (kB)', 1, 150}
    ]

    :lists.foldl(addListEntry, 0, listItems)
    :wxListItem.destroy(li)
    grid
  end

  defp create_menus(parent, _) do
    view = {'View', [r_create_menu(id: 102, text: 'Graph Settings')]}
    :observer_wx.create_menus(parent, [{'File', []}, view])
  end
end
