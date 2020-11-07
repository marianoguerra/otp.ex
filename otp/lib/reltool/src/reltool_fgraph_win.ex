defmodule :m_reltool_fgraph_win do
  use Bitwise
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

  Record.defrecord(:r_fg_e, :fg_e, l: 10.0, k: 10.0)

  Record.defrecord(:r_fg_v, :fg_v,
    p: {0.0, 0.0},
    v: {0.0, 0.0},
    q: 5.0,
    m: 1.0,
    type: :dynamic,
    color: :default,
    resides: :undefined,
    selected: false
  )

  Record.defrecord(:r_state, :state,
    parent_pid: :undefined,
    frame: :undefined,
    window: :undefined,
    width: :undefined,
    height: :undefined,
    q_slider: :undefined,
    l_slider: :undefined,
    k_slider: :undefined,
    mouse_act: :undefined,
    is_frozen: :undefined,
    ticker: :undefined
  )

  Record.defrecord(:r_graph, :graph,
    pen: :undefined,
    brush: :undefined,
    font: :undefined,
    select: :none,
    offset: {0, 0},
    offset_state: false,
    ke: 0,
    vs: [],
    es: []
  )

  def add_node(pid, key) do
    add_node(pid, key, :default)
  end

  def add_node(pid, key, color) do
    send(pid, {:add_node, key, color})
  end

  def del_node(pid, key) do
    send(pid, {:del_node, key})
  end

  def change_node(pid, key, color) do
    send(pid, {:change_node, key, color})
  end

  def add_link(pid, {fromKey, toKey}) do
    send(pid, {:add_link, {fromKey, toKey}})
  end

  def del_link(pid, {fromKey, toKey}) do
    send(pid, {:del_link, {fromKey, toKey}})
  end

  def stop(pid, reason) do
    ref = :erlang.monitor(:process, pid)
    send(pid, {:stop, reason})

    receive do
      {:DOWN, ^ref, _, _, _} ->
        :ok
    end
  end

  def set_dbl_click(pid, fun) do
    send(pid, {:set_dbl_click, fun})
  end

  def new(parent, options) do
    env = :wx.get_env()
    me = self()

    pid =
      spawn_link(fn ->
        init([parent, me, env, options])
      end)

    receive do
      {^pid, {:reltool_fgraph_win, panel}} ->
        {pid, panel}
    end
  end

  defp init([parentWin, pid, env, options]) do
    :wx.set_env(env)
    bReset = :wxButton.new(parentWin, 80, [{:label, 'Reset'}])
    bFreeze = :wxButton.new(parentWin, 86, [{:label, 'Freeze'}])
    bLock = :wxButton.new(parentWin, 81, [{:label, 'Lock'}])
    bUnlock = :wxButton.new(parentWin, 82, [{:label, 'Unlock'}])
    bDelete = :wxButton.new(parentWin, 85, [{:label, 'Delete'}])
    sQ = :wxSlider.new(parentWin, 90, 20, 1, 500, [{:style, 8}])
    sL = :wxSlider.new(parentWin, 91, 20, 1, 500, [{:style, 8}])
    sK = :wxSlider.new(parentWin, 92, 20, 1, 500, [{:style, 8}])
    win = :wxWindow.new(parentWin, -1, options)
    buttonSizer = :wxBoxSizer.new(8)
    :wxSizer.add(buttonSizer, bReset)
    :wxSizer.add(buttonSizer, bFreeze)
    :wxSizer.add(buttonSizer, bLock)
    :wxSizer.add(buttonSizer, bUnlock)
    :wxSizer.add(buttonSizer, bDelete)
    sliderSizer = :wxBoxSizer.new(4)
    :wxSizer.add(sliderSizer, sQ, [{:flag, 8192}, {:proportion, 1}])
    :wxSizer.add(sliderSizer, sL, [{:flag, 8192}, {:proportion, 1}])
    :wxSizer.add(sliderSizer, sK, [{:flag, 8192}, {:proportion, 1}])
    :wxSizer.add(buttonSizer, sliderSizer, [{:flag, 8192}, {:proportion, 1}])
    windowSizer = :wxBoxSizer.new(4)
    :wxSizer.add(windowSizer, buttonSizer, [{:flag, 8192}, {:proportion, 0}])
    :wxSizer.add(windowSizer, win, [{:flag, 8192}, {:proportion, 1}])
    :wxButton.setToolTip(bReset, 'Remove selection and unlock all nodes.')
    :wxButton.setToolTip(bFreeze, 'Start/stop redraw of screen.')
    :wxButton.setToolTip(bLock, 'Lock all selected nodes.')
    :wxButton.setToolTip(bUnlock, 'Unlock all selected nodes.')
    :wxButton.setToolTip(bDelete, 'Delete all selected nodes.')

    :wxButton.setToolTip(
      sQ,
      'Control repulsive force. This can also be controlled with the mouse wheel on the canvas.'
    )

    :wxButton.setToolTip(sL, 'Control link length.')
    :wxButton.setToolTip(sK, 'Control attractive force. Use with care.')

    :wxButton.setToolTip(
      win,
      'Drag mouse while left mouse button is pressed to perform various operations. Combine with control key to select. Combine with shift key to lock single node.'
    )

    :wxButton.connect(bReset, :command_button_clicked)
    :wxButton.connect(bFreeze, :command_button_clicked)
    :wxButton.connect(bLock, :command_button_clicked)
    :wxButton.connect(bUnlock, :command_button_clicked)
    :wxButton.connect(bDelete, :command_button_clicked)
    :wxWindow.connect(sQ, :command_slider_updated)
    :wxWindow.connect(sL, :command_slider_updated)
    :wxWindow.connect(sK, :command_slider_updated)
    :wxWindow.connect(win, :enter_window)
    :wxWindow.connect(win, :move)
    :wxWindow.connect(win, :motion)
    :wxWindow.connect(win, :mousewheel)
    :wxWindow.connect(win, :key_up)
    :wxWindow.connect(win, :left_down)
    :wxWindow.connect(win, :left_up)
    :wxWindow.connect(win, :right_down)
    :wxWindow.connect(win, :paint, [{:skip, true}])
    pen = :wxPen.new({0, 0, 0}, [{:width, 3}])
    font = :wxFont.new(12, 74, 90, 90, [])
    brush = :wxBrush.new({0, 0, 0})
    send(pid, {self(), {:reltool_fgraph_win, windowSizer}})
    :wxWindow.setFocus(win)
    vs = :reltool_fgraph.new()
    es = :reltool_fgraph.new()
    me = self()

    ticker =
      spawn_link(fn ->
        ticker_init(me)
      end)

    loop(
      r_state(
        parent_pid: pid,
        q_slider: sQ,
        l_slider: sL,
        k_slider: sK,
        mouse_act: 83,
        frame: parentWin,
        window: win,
        is_frozen: false,
        ticker: ticker
      ),
      r_graph(vs: vs, es: es, pen: pen, font: font, brush: brush)
    )
  end

  defp graph_add_node_unsure(key, state, g = r_graph(vs: vs)) do
    case :reltool_fgraph.is_defined(key, vs) do
      true ->
        g

      false ->
        graph_add_node(key, state, g)
    end
  end

  defp graph_add_node(key, color, g = r_graph(vs: vs)) do
    q = 20.0
    m = 0.5
    p = {:erlang.float(450 + :rand.uniform(100)), :erlang.float(450 + :rand.uniform(100))}
    r_graph(g, vs: :reltool_fgraph.add(key, r_fg_v(p: p, m: m, q: q, color: color), vs))
  end

  defp graph_change_node(key, color, g) do
    case :reltool_fgraph.get(key, r_graph(g, :vs)) do
      :undefined ->
        g

      v ->
        r_graph(g, vs: :reltool_fgraph.set(key, r_fg_v(v, color: color), r_graph(g, :vs)))
    end
  end

  defp graph_del_node(key, g = r_graph(vs: vs0, es: es0)) do
    vs = :reltool_fgraph.del(key, vs0)
    es = delete_edges(es0, [key])
    r_graph(g, vs: vs, es: es)
  end

  defp graph_add_link(key0, key1, g = r_graph(es: es)) do
    k = 60.0
    l = 5.0
    r_graph(g, es: :reltool_fgraph.add({key0, key1}, r_fg_e(k: k, l: l), es))
  end

  defp graph_del_link(key0, key1, g = r_graph(es: es)) do
    r_graph(g, es: :reltool_fgraph.del({key0, key1}, es))
  end

  defp ticker_init(pid) do
    ticker_loop(pid, 50)
  end

  defp ticker_loop(pid, time) do
    receive do
    after
      time ->
        send(pid, {self(), :redraw})
        t0 = :erlang.monotonic_time()

        receive do
          {^pid, :ok} ->
            :ok
        end

        t1 = :erlang.monotonic_time()
        d = :erlang.convert_time_unit(t1 - t0, :native, :milli_seconds)

        case round(40 - d) do
          ms when ms < 0 ->
            ticker_loop(pid, 0)

          ms ->
            ticker_loop(pid, ms)
        end
    end
  end

  defp delete_edges(es, []) do
    es
  end

  defp delete_edges(es, [key | keys]) do
    edges =
      :reltool_fgraph.foldl(
        fn
          {{k1, k2}, _}, out
          when k1 === key ->
            [{k1, k2} | out]

          {{k1, k2}, _}, out when k2 === key ->
            [{k1, k2} | out]

          _, out ->
            out
        end,
        [],
        es
      )

    es1 =
      :lists.foldl(
        fn k, esi ->
          :reltool_fgraph.del(k, esi)
        end,
        es,
        edges
      )

    delete_edges(es1, keys)
  end

  defp set_charge(q, vs) do
    f = fn {key, value} ->
      {key, r_fg_v(value, q: q)}
    end

    :reltool_fgraph.map(f, vs)
  end

  defp set_length(l, es) do
    f = fn {ps, e} ->
      {ps, r_fg_e(e, l: l)}
    end

    :reltool_fgraph.map(f, es)
  end

  defp set_spring(k, es) do
    f = fn {ps, e} ->
      {ps, r_fg_e(e, k: k)}
    end

    :reltool_fgraph.map(f, es)
  end

  defp loop(s, g) do
    receive do
      r_wx(id: 80, event: r_wxCommand(type: :command_button_clicked)) ->
        q = 20
        l = 20
        k = 20
        :wxSlider.setValue(r_state(s, :q_slider), q)
        :wxSlider.setValue(r_state(s, :l_slider), l)
        :wxSlider.setValue(r_state(s, :k_slider), k)
        es = set_length(l, r_graph(g, :es))
        es2 = set_spring(k, es)

        vs2 =
          :reltool_fgraph.map(
            fn {key, v} ->
              {key, r_fg_v(v, selected: false, type: :dynamic, q: q)}
            end,
            r_graph(g, :vs)
          )

        {xs, ys} =
          :reltool_fgraph.foldl(
            fn {_Key, r_fg_v(p: {x, y})}, {xs, ys} ->
              {[x | xs], [y | ys]}
            end,
            {[], []},
            vs2
          )

        offset =
          case length(xs) do
            0 ->
              {0, 0}

            n ->
              meanX = :lists.sum(xs) / n
              meanY = :lists.sum(ys) / n
              {sizeX, sizeY} = :wxWindow.getSize(r_state(s, :window))
              {0 - meanX + sizeX / 2, 0 - meanY + sizeY / 2}
          end

        loop(
          s,
          r_graph(g, vs: vs2, es: es2, offset: offset, offset_state: false)
        )

      r_wx(id: 86, event: r_wxCommand(type: :command_button_clicked)) ->
        isFrozen =
          case r_state(s, :is_frozen) do
            true ->
              send(r_state(s, :ticker), {self(), :ok})
              false

            false ->
              true
          end

        loop(r_state(s, is_frozen: isFrozen), g)

      r_wx(id: 81, event: r_wxCommand(type: :command_button_clicked)) ->
        vs =
          :reltool_fgraph.map(
            fn
              {key, v = r_fg_v(selected: true)} ->
                {key, r_fg_v(v, type: :static)}

              kV ->
                kV
            end,
            r_graph(g, :vs)
          )

        loop(s, r_graph(g, vs: vs))

      r_wx(id: 82, event: r_wxCommand(type: :command_button_clicked)) ->
        vs =
          :reltool_fgraph.map(
            fn
              {key, v = r_fg_v(selected: true)} ->
                {key, r_fg_v(v, type: :dynamic)}

              kV ->
                kV
            end,
            r_graph(g, :vs)
          )

        loop(s, r_graph(g, vs: vs))

      r_wx(id: 85, event: r_wxCommand(type: :command_button_clicked)) ->
        {vs1, keys} =
          :reltool_fgraph.foldl(
            fn
              {key, r_fg_v(selected: true)}, {vs, ks} ->
                {:reltool_fgraph.del(key, vs), [key | ks]}

              _, {vs, ks} ->
                {vs, ks}
            end,
            {r_graph(g, :vs), []},
            r_graph(g, :vs)
          )

        es = delete_edges(r_graph(g, :es), keys)
        loop(s, r_graph(g, vs: vs1, es: es))

      r_wx(id: 84, event: r_wxCommand(type: :command_button_clicked)) ->
        loop(r_state(s, mouse_act: 84), g)

      r_wx(id: 83, event: r_wxCommand(type: :command_button_clicked)) ->
        loop(r_state(s, mouse_act: 83), g)

      r_wx(
        id: 90,
        event:
          r_wxCommand(
            type: :command_slider_updated,
            commandInt: q
          )
      ) ->
        loop(s, r_graph(g, vs: set_charge(q, r_graph(g, :vs))))

      r_wx(
        id: 91,
        event:
          r_wxCommand(
            type: :command_slider_updated,
            commandInt: l
          )
      ) ->
        loop(s, r_graph(g, es: set_length(l, r_graph(g, :es))))

      r_wx(
        id: 92,
        event:
          r_wxCommand(
            type: :command_slider_updated,
            commandInt: k
          )
      ) ->
        loop(s, r_graph(g, es: set_spring(k, r_graph(g, :es))))

      r_wx(event: r_wxKey(type: :key_up, keyCode: 127)) ->
        {vs1, keys} =
          :reltool_fgraph.foldl(
            fn
              {key, r_fg_v(selected: true)}, {vs, ks} ->
                {:reltool_fgraph.del(key, vs), [key | ks]}

              _, {vs, ks} ->
                {vs, ks}
            end,
            {r_graph(g, :vs), []},
            r_graph(g, :vs)
          )

        es = delete_edges(r_graph(g, :es), keys)
        loop(s, r_graph(g, vs: vs1, es: es))

      r_wx(event: r_wxKey(type: :key_up)) ->
        loop(s, g)

      r_wx(event: r_wxKey(type: :key_down)) ->
        loop(s, g)

      r_wx(event: r_wxMouse(type: :left_down, shiftDown: shift, controlDown: ctrl, x: x, y: y)) ->
        cond do
          shift ->
            loop(s, mouse_left_down_move(g, {x, y}))

          ctrl ->
            loop(s, mouse_left_down_select(g, {x, y}))

          r_state(s, :mouse_act) === 83 ->
            loop(s, mouse_left_down_move(g, {x, y}))

          r_state(s, :mouse_act) === 84 ->
            loop(s, mouse_left_down_select(g, {x, y}))
        end

      r_wx(event: r_wxMouse(type: :motion, shiftDown: shift, controlDown: ctrl, x: x, y: y)) ->
        cond do
          shift ->
            loop(s, mouse_motion_move(g, {x, y}))

          ctrl ->
            loop(s, mouse_motion_select(g, {x, y}))

          r_state(s, :mouse_act) === 83 ->
            loop(s, mouse_motion_move(g, {x, y}))

          r_state(s, :mouse_act) === 84 ->
            loop(s, mouse_motion_select(g, {x, y}))
        end

      r_wx(event: r_wxMouse(type: :left_up, shiftDown: shift, controlDown: ctrl, x: x, y: y)) ->
        cond do
          shift ->
            loop(s, mouse_left_up_move(g, {x, y}, shift))

          ctrl ->
            loop(s, mouse_left_up_select(g, {x, y}))

          r_state(s, :mouse_act) === 83 ->
            loop(s, mouse_left_up_move(g, {x, y}, shift))

          r_state(s, :mouse_act) === 84 ->
            loop(s, mouse_left_up_select(g, {x, y}))
        end

      r_wx(event: r_wxMouse(type: :right_down, x: _X, y: _Y)) ->
        loop(s, g)

      r_wx(
        event:
          r_wxMouse(
            type: :mousewheel,
            wheelRotation: rotation
          )
      ) ->
        q = :wxSlider.getValue(r_state(s, :q_slider))

        cond do
          rotation > 0 and q > 5 ->
            :wxSlider.setValue(r_state(s, :q_slider), q - 4)
            loop(s, r_graph(g, vs: set_charge(q - 4, r_graph(g, :vs))))

          rotation < 0 ->
            :wxSlider.setValue(r_state(s, :q_slider), q + 4)
            loop(s, r_graph(g, vs: set_charge(q + 4, r_graph(g, :vs))))

          true ->
            loop(s, g)
        end

      r_wx(obj: _Win, event: r_wxPaint()) ->
        redraw(s, g)
        loop(s, g)

      r_wx(obj: win, event: r_wxMouse(type: :enter_window)) ->
        :wxWindow.setFocus(win)
        loop(s, g)

      {:add_node, key, state} ->
        loop(s, graph_add_node_unsure(key, state, g))

      {:del_node, key} ->
        loop(s, graph_del_node(key, g))

      {:change_node, key, color} ->
        loop(s, graph_change_node(key, color, g))

      {:add_link, {k0, k1}} ->
        loop(s, graph_add_link(k0, k1, g))

      {:del_link, {k0, k1}} ->
        loop(s, graph_del_link(k0, k1, g))

      {req, :redraw} ->
        {sizeX, sizeY} = :wxWindow.getSize(r_state(s, :window))

        vs =
          :reltool_fgraph.step(
            r_graph(g, :vs),
            r_graph(g, :es),
            {sizeX / 2.0 - 20.0, sizeY / 2.0}
          )

        case r_state(s, :is_frozen) do
          false ->
            send(req, {self(), :ok})

          true ->
            :ignore
        end

        redraw(s, g)
        loop(s, r_graph(g, vs: vs))

      {:stop, reason} ->
        :erlang.unlink(r_state(s, :parent_pid))
        exit(reason)

      other ->
        :error_logger.format(
          '~w~w got unexpected message:\n\t~tp\n',
          [:reltool_fgraph_win, self(), other]
        )

        loop(s, g)
    end
  end

  defp mouse_left_down_select(g, {x0, y0}) do
    r_graph(g, select: {{x0, y0}, {x0, y0}})
  end

  defp mouse_left_down_move(r_graph(vs: vs) = g, {x, y}) do
    case coord_to_key(g, {x, y}) do
      false ->
        r_graph(g, offset_state: {x, y})

      {true, key} ->
        v = r_fg_v(type: type) = :reltool_fgraph.get(key, vs)

        r_graph(g,
          vs: :reltool_fgraph.set(key, r_fg_v(v, type: :moving), vs),
          select: {:node, key, type, x, y}
        )
    end
  end

  defp coord_to_key(r_graph(vs: vs, offset: {xo, yo}), {x, y}) do
    xr = x - xo
    yr = y - yo

    :reltool_fgraph.foldl(
      fn
        {key, r_fg_v(p: {px, py})}, _
        when abs(px - xr) < 10 and abs(py - yr) < 10 ->
          {true, key}

        _, out ->
          out
      end,
      false,
      vs
    )
  end

  defp mouse_left_up_select(g, {_X, _Y}) do
    case r_graph(g, :select) do
      {{x0, y0}, {x1, y1}} ->
        {xo, yo} = r_graph(g, :offset)
        xmin = :lists.min([x0, x1]) - xo
        xmax = :lists.max([x1, x0]) - xo
        ymin = :lists.min([y0, y1]) - yo
        ymax = :lists.max([y1, y0]) - yo

        vs =
          :reltool_fgraph.map(
            fn
              {key, value = r_fg_v(p: {px, py})}
              when px > xmin and px < xmax and
                     py > ymin and py < ymax ->
                {key, r_fg_v(value, selected: true)}

              {key, value} ->
                {key, r_fg_v(value, selected: false)}
            end,
            r_graph(g, :vs)
          )

        r_graph(g, select: :none, vs: vs)

      _ ->
        r_graph(g, select: :none)
    end
  end

  defp mouse_left_up_move(g = r_graph(select: select, vs: vs) = g, {x, y}, shift) do
    case select do
      {:node, key, _, ^x, ^y} ->
        :io.format('click: ~p\n', [key])
        r_graph(g, select: :none, offset_state: false)

      {:node, key, type, _, _} ->
        v = :reltool_fgraph.get(key, vs)

        type2 =
          case shift do
            true ->
              :static

            false ->
              type
          end

        r_graph(g,
          select: :none,
          vs: :reltool_fgraph.set(key, r_fg_v(v, type: type2), vs),
          offset_state: false
        )

      _ ->
        r_graph(g, select: :none, offset_state: false)
    end
  end

  defp mouse_motion_select(g, {x, y}) do
    case r_graph(g, :select) do
      {p0, _P1} ->
        r_graph(g, select: {p0, {x, y}})

      _ ->
        g
    end
  end

  defp mouse_motion_move(
         g = r_graph(select: {:node, key, _, _, _}, vs: vs),
         {x, y}
       ) do
    {xo, yo} = r_graph(g, :offset)
    v = :reltool_fgraph.get(key, vs)
    v2 = r_fg_v(v, p: {:erlang.float(x - xo), :erlang.float(y - yo)})
    r_graph(g, vs: :reltool_fgraph.set(key, v2, vs))
  end

  defp mouse_motion_move(g, {x, y}) do
    case r_graph(g, :offset_state) do
      {x1, y1} ->
        {x0, y0} = r_graph(g, :offset)

        r_graph(g,
          offset_state: {x, y},
          offset: {x0 - (x1 - x), y0 - (y1 - y)}
        )

      _ ->
        g
    end
  end

  defp redraw(r_state(window: win), g) do
    dC0 = :wxClientDC.new(win)
    dC = :wxBufferedDC.new(dC0)
    size = :wxWindow.getSize(win)
    redraw(dC, size, g)
    :wxBufferedDC.destroy(dC)
    :wxClientDC.destroy(dC0)
    :ok
  end

  defp redraw(dC, _Size, g) do
    :wx.batch(fn ->
      pen = r_graph(g, :pen)
      font = r_graph(g, :font)
      brush = r_graph(g, :brush)
      :wxDC.setTextForeground(dC, {235, 245, 230})
      :wxBrush.setColour(brush, {45, 50, 95})
      :wxDC.setBrush(dC, brush)
      :wxDC.setBackground(dC, brush)
      :wxPen.setWidth(pen, 1)
      :wxDC.clear(dC)
      :wxPen.setColour(pen, {235, 245, 230})
      :wxDC.setPen(dC, pen)
      draw_es(dC, r_graph(g, :vs), r_graph(g, :es), r_graph(g, :offset), pen, brush)
      draw_vs(dC, r_graph(g, :vs), r_graph(g, :offset), pen, brush)
      :wxPen.setColour(pen, {235, 245, 230})
      :wxDC.setPen(dC, pen)
      draw_select_box(dC, r_graph(g, :select))
      :wxFont.setWeight(font, 90)

      draw_text(
        dC,
        :reltool_fgraph.size(r_graph(g, :vs)),
        :reltool_fgraph.size(r_graph(g, :es)),
        r_graph(g, :ke)
      )

      :ok
    end)
  end

  defp draw_select_box(dC, {{x0, y0}, {x1, y1}}) do
    draw_line(dC, {x0, y0}, {x1, y0}, {0, 0})
    draw_line(dC, {x1, y1}, {x1, y0}, {0, 0})
    draw_line(dC, {x1, y1}, {x0, y1}, {0, 0})
    draw_line(dC, {x0, y0}, {x0, y1}, {0, 0})
    :ok
  end

  defp draw_select_box(_DC, _) do
    :ok
  end

  defp draw_es(dC, vs, es, po, pen, brush) do
    :reltool_fgraph.foreach(
      fn {{k1, k2}, _} ->
        r_fg_v(p: p1) = :reltool_fgraph.get(k1, vs)
        r_fg_v(p: p2) = :reltool_fgraph.get(k2, vs)
        draw_arrow(dC, p1, p2, po, pen, brush)
      end,
      es
    )
  end

  defp draw_arrow(dC, {x0, y0}, {x1, y1}, {x, y}, pen, brush) do
    xdiff = (x0 - x1) / 4
    ydiff = (y0 - y1) / 4
    x2 = x1 + xdiff + x
    y2 = y1 + ydiff + y
    :wxDC.setPen(dC, pen)
    :wxDC.setBrush(dC, brush)
    draw_line(dC, {x0, y0}, {x1, y1}, {x, y})
    radians = calc_angle({x0, y0}, {x1, y1})
    len = 10
    radians2 = radians + 3.665191429188092
    radians3 = radians + 2.617993877991494
    {x3, y3} = calc_point({x2, y2}, len, radians2)
    {x4, y4} = calc_point({x2, y2}, len, radians3)
    points = [{round(x2), round(y2)}, {round(x3), round(y3)}, {round(x4), round(y4)}]
    :wxDC.drawPolygon(dC, points, [])
  end

  defp draw_line(dC, {x0, y0}, {x1, y1}, {x, y}) do
    :wxDC.drawLine(dC, {round(x0 + x), round(y0 + y)}, {round(x1 + x), round(y1 + y)})
  end

  defp draw_vs(dC, vs, {xo, yo}, pen, brush) do
    :reltool_fgraph.foreach(
      fn
        {key, r_fg_v(p: {x, y}, color: color, selected: sel)} ->
          string = s(key)

          case sel do
            true ->
              :wxPen.setColour(pen, {235, 245, 230})
              :wxBrush.setColour(brush, {45, 50, 95})
              :wxDC.setPen(dC, pen)
              :wxDC.setBrush(dC, brush)
              selProps = {round(x - 12 + xo), round(y - 12 + yo), 24, 24}
              :wxDC.drawRoundedRectangle(dC, selProps, :erlang.float(10))
              :ok

            false ->
              :ok
          end

          case color do
            :default ->
              :wxPen.setColour(pen, {10, 220, 20})
              :wxBrush.setColour(brush, {20, 230, 30})

            :alternate ->
              :wxPen.setColour(pen, {220, 10, 20})
              :wxBrush.setColour(brush, {230, 20, 30})

            {fgColor, bgColor} ->
              :wxPen.setColour(pen, fgColor)
              :wxBrush.setColour(brush, bgColor)

            ^color ->
              :wxPen.setColour(pen, color)
              :wxBrush.setColour(brush, color)
          end

          :wxDC.setPen(dC, pen)
          :wxDC.setBrush(dC, brush)
          nodeProps = {round(x - 8 + xo), round(y - 8 + yo), 17, 17}
          :wxDC.drawRoundedRectangle(dC, nodeProps, :erlang.float(10))
          :wxDC.drawText(dC, string, {round(x + xo), round(y + yo)})
          :ok

        _ ->
          :ok
      end,
      vs
    )
  end

  defp draw_text(dC, nvs, nes, _KE) do
    vsString = '#nodes: ' ++ :erlang.integer_to_list(nvs)
    esString = '#links: ' ++ :erlang.integer_to_list(nes)
    :wxDC.drawText(dC, vsString, {10, 10})
    :wxDC.drawText(dC, esString, {10, 25})
    :ok
  end

  defp s(format, terms) do
    :lists.flatten(:io_lib.format(format, terms))
  end

  defp s(term) when is_float(term) do
    s('~.2f', [term])
  end

  defp s(term) when is_integer(term) do
    :erlang.integer_to_list(term)
  end

  defp s(term) when is_atom(term) do
    :erlang.atom_to_list(term)
  end

  defp s(term) do
    s('~p', [term])
  end

  defp calc_angle({x1, y1}, {x2, y2}) do
    :math.atan2(y2 - y1, x2 - x1)
  end

  defp calc_point({x, y}, length, radians) do
    x2 = round(x + length * :math.cos(radians))
    y2 = round(y + length * :math.sin(radians))
    {x2, y2}
  end
end
