defmodule :m_observer_lib do
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

  def get_wx_parent(window) do
    parent = :wxWindow.getParent(window)

    case :wx.is_null(parent) do
      true ->
        window

      false ->
        get_wx_parent(parent)
    end
  end

  def interval_dialog(parent0, {timer, value}, min, max) do
    parent = get_wx_parent(parent0)

    dialog =
      :wxDialog.new(parent, -1, 'Update Interval', [
        {:style, 536_870_912 ||| 2048 ||| 4096 ||| 64}
      ])

    panel = :wxPanel.new(dialog)
    check = :wxCheckBox.new(panel, -1, 'Periodical refresh')
    :wxCheckBox.setValue(check, timer != false)
    style = 4 ||| 16 ||| :wxe_util.get_const(:wxSL_LABELS)
    slider = :wxSlider.new(panel, -1, value, min, max, [{:style, style}, {:size, {200, -1}}])
    :wxWindow.enable(slider, [{:enable, timer != false}])
    innerSizer = :wxBoxSizer.new(8)
    buttons = :wxDialog.createButtonSizer(dialog, 4 ||| 16)
    flags = [{:flag, 8192 ||| (64 ||| 128 ||| 32 ||| 16)}, {:border, 2}]
    :wxSizer.add(innerSizer, check, flags)
    :wxSizer.add(innerSizer, slider, flags)
    :wxPanel.setSizer(panel, innerSizer)
    topSizer = :wxBoxSizer.new(8)
    :wxSizer.add(topSizer, panel, [{:flag, 8192 ||| (64 ||| 128 ||| 32 ||| 16)}, {:border, 5}])
    :wxSizer.add(topSizer, buttons, [{:flag, 8192}])
    :wxWindow.setSizerAndFit(dialog, topSizer)
    :wxSizer.setSizeHints(topSizer, dialog)

    :wxCheckBox.connect(check, :command_checkbox_clicked, [
      {:callback,
       fn r_wx(event: r_wxCommand(commandInt: enable0)), _ ->
         enable = enable0 > 0
         :wxWindow.enable(slider, [{:enable, enable}])
       end}
    ])

    res =
      case :wxDialog.showModal(dialog) do
        5100 ->
          enabled = :wxCheckBox.isChecked(check)

          setup_timer(
            enabled,
            {timer, :wxSlider.getValue(slider)}
          )

        5101 ->
          {timer, value}
      end

    :wxDialog.destroy(dialog)
    res
  end

  def stop_timer(timer = {false, _}) do
    timer
  end

  def stop_timer(timer = {true, _}) do
    timer
  end

  def stop_timer(timer = {_, intv}) do
    setup_timer(false, timer)
    {true, intv}
  end

  def start_timer(%{:interval => intv}, _Def) do
    setup_timer(true, {false, intv})
  end

  def start_timer(_, def__) do
    setup_timer(true, {false, def__})
  end

  def start_timer(intv) when is_integer(intv) do
    setup_timer(true, {true, intv})
  end

  def start_timer(timer) do
    setup_timer(true, timer)
  end

  defp setup_timer(false, {timer, value}) when is_boolean(timer) do
    {false, value}
  end

  defp setup_timer(true, {false, value}) do
    {:ok, timer} =
      :timer.send_interval(
        value * 1000,
        :refresh_interval
      )

    {timer, value}
  end

  defp setup_timer(bool, {timer, old}) do
    :timer.cancel(timer)
    setup_timer(bool, {false, old})
  end

  def timer_config({_, interval}) do
    %{:interval => interval}
  end

  def timer_config(%{} = config) do
    config
  end

  def display_info_dialog(parent, str) do
    display_info_dialog(parent, '', str)
  end

  defp display_info_dialog(parent, title, str) do
    dlg = :wxMessageDialog.new(parent, str, [{:caption, title}])
    :wxMessageDialog.showModal(dlg)
    :wxMessageDialog.destroy(dlg)
    :ok
  end

  def display_yes_no_dialog(str) do
    dlg = :wxMessageDialog.new(:wx.null(), str, [{:style, 2 ||| 8}])
    r = :wxMessageDialog.showModal(dlg)
    :wxMessageDialog.destroy(dlg)
    r
  end

  def display_info(frame, info) do
    panel = :wxPanel.new(frame)

    :wxWindow.setBackgroundStyle(
      panel,
      :wxe_util.get_const(:wxBG_STYLE_SYSTEM)
    )

    sizer = :wxBoxSizer.new(8)
    infoFs = display_info(panel, sizer, info)
    :wxWindow.setSizerAndFit(panel, sizer)
    {panel, sizer, infoFs}
  end

  def display_info(panel, sizer, info) do
    :wxSizer.addSpacer(sizer, 5)

    add = fn boxInfo ->
      case create_box(panel, boxInfo) do
        {box, infoFs} ->
          :wxSizer.add(sizer, box, [{:flag, 8192 ||| (64 ||| 128 ||| 32 ||| 16)}, {:border, 5}])
          :wxSizer.addSpacer(sizer, 5)
          infoFs

        :undefined ->
          []
      end
    end

    for i <- info do
      add.(i)
    end
  end

  def fill_info([{:dynamic, key} | rest], data)
      when is_atom(key) or is_function(key) do
    case get_value(key, data) do
      :undefined ->
        [:undefined | fill_info(rest, data)]

      {str, value} ->
        [{str, value} | fill_info(rest, data)]
    end
  end

  def fill_info([{str, key} | rest], data)
      when is_atom(key) or
             is_function(key) do
    case get_value(key, data) do
      :undefined ->
        [:undefined | fill_info(rest, data)]

      value ->
        [{str, value} | fill_info(rest, data)]
    end
  end

  def fill_info([{str, attrib, key} | rest], data)
      when is_atom(key) or is_function(key) do
    case get_value(key, data) do
      :undefined ->
        [:undefined | fill_info(rest, data)]

      value ->
        [{str, attrib, value} | fill_info(rest, data)]
    end
  end

  def fill_info([{str, {format, key}} | rest], data)
      when is_atom(key) or is_function(key) do
    case get_value(key, data) do
      :undefined ->
        [:undefined | fill_info(rest, data)]

      value ->
        [{str, {format, value}} | fill_info(rest, data)]
    end
  end

  def fill_info([{str, attrib, {format, key}} | rest], data)
      when is_atom(key) or is_function(key) do
    case get_value(key, data) do
      :undefined ->
        [:undefined | fill_info(rest, data)]

      value ->
        [{str, attrib, {format, value}} | fill_info(rest, data)]
    end
  end

  def fill_info([{str, subStructure} | rest], data)
      when is_list(subStructure) do
    [
      {str, fill_info(subStructure, data)}
      | fill_info(
          rest,
          data
        )
    ]
  end

  def fill_info([{str, attrib, subStructure} | rest], data) do
    [
      {str, attrib, fill_info(subStructure, data)}
      | fill_info(rest, data)
    ]
  end

  def fill_info([{str, key = {k, n}} | rest], data)
      when is_atom(k) and is_integer(n) do
    case get_value(key, data) do
      :undefined ->
        [:undefined | fill_info(rest, data)]

      value ->
        [{str, value} | fill_info(rest, data)]
    end
  end

  def fill_info([], _) do
    []
  end

  defp get_value(fun, data) when is_function(fun) do
    fun.(data)
  end

  defp get_value(key, data) do
    :proplists.get_value(key, data)
  end

  def update_info(
        [fields | fs],
        [{_Header, subStructure} | rest]
      ) do
    update_info2(fields, subStructure)
    update_info(fs, rest)
  end

  def update_info(
        [fields | fs],
        [{_Header, _Attrib, subStructure} | rest]
      ) do
    update_info2(fields, subStructure)
    update_info(fs, rest)
  end

  def update_info([], []) do
    :ok
  end

  defp update_info2([:undefined | fs], [_ | rest]) do
    update_info2(fs, rest)
  end

  defp update_info2(
         [scroll = {_, _, _} | fs],
         [{_, newInfo} | rest]
       ) do
    update_scroll_boxes(scroll, newInfo)
    update_info2(fs, rest)
  end

  defp update_info2(
         [field | fs],
         [{_Str, {:click, value}} | rest]
       ) do
    :wxStaticText.setLabel(field, to_str(value))
    update_info2(fs, rest)
  end

  defp update_info2([field | fs], [{_Str, value} | rest]) do
    :wxStaticText.setLabel(field, to_str(value))
    update_info2(fs, rest)
  end

  defp update_info2([field | fs], [:undefined | rest]) do
    :wxStaticText.setLabel(field, '')
    update_info2(fs, rest)
  end

  defp update_info2([], []) do
    :ok
  end

  defp update_scroll_boxes({_, _, 0}, {_, []}) do
    :ok
  end

  defp update_scroll_boxes({win, sizer, _}, {type, list}) do
    for child <- :wxSizer.getChildren(sizer) do
      :wxSizerItem.deleteWindows(child)
    end

    cursor = :wxCursor.new(6)
    add_entries(type, list, win, sizer, cursor)
    :wxCursor.destroy(cursor)
    :wxSizer.recalcSizes(sizer)
    :wxWindow.refresh(win)
    :ok
  end

  def to_str(value) when is_atom(value) do
    :erlang.atom_to_list(value)
  end

  def to_str({unit, x})
      when (unit == :bytes or unit == :time_ms) and is_list(x) do
    try do
      :erlang.list_to_integer(x)
    catch
      :error, :badarg ->
        x
    else
      b ->
        to_str({unit, b})
    end
  end

  def to_str({:bytes, b}) do
    kB = div(b, 1024)
    mB = div(kB, 1024)
    gB = div(mB, 1024)

    cond do
      gB > 10 ->
        :erlang.integer_to_list(gB) ++ ' GB'

      mB > 10 ->
        :erlang.integer_to_list(mB) ++ ' MB'

      kB > 0 ->
        :erlang.integer_to_list(kB) ++ ' kB'

      true ->
        :erlang.integer_to_list(b) ++ ' B'
    end
  end

  def to_str({{:words, wSz}, sz}) do
    to_str({:bytes, wSz * sz})
  end

  def to_str({:time_ms, mS}) do
    s = div(mS, 1000)
    min = div(s, 60)
    hours = div(min, 60)
    days = div(hours, 24)

    cond do
      days > 0 ->
        :erlang.integer_to_list(days) ++ ' Days'

      hours > 0 ->
        :erlang.integer_to_list(hours) ++ ' Hours'

      min > 0 ->
        :erlang.integer_to_list(min) ++ ' Mins'

      true ->
        :erlang.integer_to_list(s) ++ ' Secs'
    end
  end

  def to_str({:func, {f, a}})
      when is_atom(f) and
             is_integer(a) do
    :lists.concat([f, '/', a])
  end

  def to_str({:func, {f, :_}}) when is_atom(f) do
    :erlang.atom_to_list(f)
  end

  def to_str({:inet, addr}) do
    case :inet.ntoa(addr) do
      {:error, :einval} ->
        to_str(addr)

      addrStr ->
        addrStr
    end
  end

  def to_str({{:format, fun}, value}) when is_function(fun) do
    fun.(value)
  end

  def to_str({a, b}) when is_atom(a) and is_atom(b) do
    :lists.concat([a, ':', b])
  end

  def to_str({m, f, a})
      when is_atom(m) and is_atom(f) and
             is_integer(a) do
    :lists.concat([m, ':', f, '/', a])
  end

  def to_str(value) when is_list(value) do
    case :lists.all(
           fn x ->
             is_integer(x)
           end,
           value
         ) do
      true ->
        value

      false ->
        :lists.foldl(
          fn x, acc ->
            to_str(x) ++ ' ' ++ acc
          end,
          '',
          value
        )
    end
  end

  def to_str(port) when is_port(port) do
    :erlang.port_to_list(port)
  end

  def to_str(pid) when is_pid(pid) do
    :erlang.pid_to_list(pid)
  end

  def to_str(no) when is_integer(no) do
    :erlang.integer_to_list(no)
  end

  def to_str(float) when is_float(float) do
    :io_lib.format('~.3f', [float])
  end

  def to_str({:trunc, float}) when is_float(float) do
    :erlang.float_to_list(float, [{:decimals, 0}])
  end

  def to_str(term) do
    :io_lib.format('~tw', [term])
  end

  def create_menus([], _MenuBar, _Type) do
    :ok
  end

  def create_menus(menus, menuBar, type) do
    add = fn {tag, ms}, index ->
      create_menu(tag, ms, index, menuBar, type)
    end

    [{first, _} | _] = menus

    index =
      cond do
        type === :default ->
          0

        first === 'File' ->
          0

        true ->
          1
      end

    :wx.foldl(add, index, menus)
    :ok
  end

  defp create_menu('File', menuItems, index, menuBar, type) do
    cond do
      type === :plugin ->
        menuId = :wxMenuBar.findMenu(menuBar, 'File')
        menu = :wxMenuBar.getMenu(menuBar, menuId)

        :lists.foldl(
          fn record, n ->
            create_menu_item(record, menu, n)
          end,
          0,
          menuItems
        )

        index + 1

      true ->
        menu = :wxMenu.new()

        :lists.foldl(
          fn record, n ->
            create_menu_item(record, menu, n)
          end,
          0,
          menuItems
        )

        :wxMenuBar.insert(menuBar, index, menu, 'File')
        index + 1
    end
  end

  defp create_menu(name, menuItems, index, menuBar, _Type) do
    menu = :wxMenu.new()

    :lists.foldl(
      fn record, n ->
        create_menu_item(record, menu, n)
      end,
      0,
      menuItems
    )

    :wxMenuBar.insert(menuBar, index, menu, name)
    index + 1
  end

  def create_menu_item(r_create_menu(id: 5009 = id), menu, index) do
    :wxMenu.insert(menu, index, id)
    index + 1
  end

  def create_menu_item(
        r_create_menu(id: id, text: text, help: help, type: type, check: check),
        menu,
        index
      ) do
    opts =
      case help do
        [] ->
          []

        _ ->
          [{:help, help}]
      end

    case type do
      :append ->
        :wxMenu.insert(menu, index, id, [{:text, text} | opts])

      :check ->
        :wxMenu.insertCheckItem(menu, index, id, text, opts)
        :wxMenu.check(menu, id, check)

      :radio ->
        :wxMenu.insertRadioItem(menu, index, id, text, opts)
        :wxMenu.check(menu, id, check)

      :separator ->
        :wxMenu.insertSeparator(menu, index)
    end

    index + 1
  end

  def create_menu_item(:separator, menu, index) do
    :wxMenu.insertSeparator(menu, index)
    index + 1
  end

  def colors(window) do
    darkMode = is_darkmode(:wxWindow.getBackgroundColour(window))

    text =
      case :wxSystemSettings.getColour(1 + 14 + 1 + 4 + 11) do
        {255, 255, 255, _} when not darkMode ->
          {10, 10, 10}

        color ->
          color
      end

    even = :wxSystemSettings.getColour(1 + 14 + 1 + 4 + 5)
    odd = mix(even, :wxSystemSettings.getColour(1 + 12), 0.8)
    r_colors(fg: rgb(text), even: rgb(even), odd: rgb(odd))
  end

  def create_attrs(window) do
    font = :wxSystemSettings.getFont(17)
    r_colors(fg: text, even: even, odd: odd) = colors(window)

    r_attrs(
      even: :wxListItemAttr.new(text, even, font),
      odd: :wxListItemAttr.new(text, odd, font),
      deleted: :wxListItemAttr.new({230, 230, 230}, {100, 100, 100}, font),
      changed_even:
        :wxListItemAttr.new(
          text,
          mix({184, 207, 184}, {230, 230, 250}, 0.9),
          font
        ),
      changed_odd:
        :wxListItemAttr.new(
          text,
          mix({184, 207, 184}, {255, 255, 255}, 0.9),
          font
        ),
      new_even:
        :wxListItemAttr.new(
          text,
          mix({123, 168, 123}, {230, 230, 250}, 0.9),
          font
        ),
      new_odd:
        :wxListItemAttr.new(
          text,
          mix({123, 168, 123}, {255, 255, 255}, 0.9),
          font
        ),
      searched: :wxListItemAttr.new(text, {235, 215, 90}, font)
    )
  end

  defp rgb({r, g, b, _}) do
    {r, g, b}
  end

  defp rgb({_, _, _} = rGB) do
    rGB
  end

  def mix(rGB, {mR, mG, mB, _}, v) do
    mix(rGB, {mR, mG, mB}, v)
  end

  def mix({r, g, b, _}, rGB, v) do
    mix({r, g, b}, rGB, v)
  end

  def mix({r, g, b}, {mR, mG, mB}, v) when v <= 1.0 do
    {min(255, round(r * v + mR * (1.0 - v))), min(255, round(g * v + mG * (1.0 - v))),
     min(255, round(b * v + mB * (1.0 - v)))}
  end

  def is_darkmode({r, g, b, _}) do
    div(r + g + b, 3) < 100
  end

  defp get_box_info({title, list}) when is_list(list) do
    {title, 0, list}
  end

  defp get_box_info({title, :left, list}) do
    {title, 0, list}
  end

  defp get_box_info({title, :right, list}) do
    {title, 512, list}
  end

  defp add_box(panel, outerBox, cursor, title, proportion, {format, list}) do
    numStr = ' (' ++ :erlang.integer_to_list(length(list)) ++ ')'
    box = :wxStaticBoxSizer.new(8, panel, [{:label, title ++ numStr}])
    scroll = :wxScrolledWindow.new(panel)
    :wxScrolledWindow.enableScrolling(scroll, true, true)
    :wxScrolledWindow.setScrollbars(scroll, 1, 1, 0, 0)
    scrollSizer = :wxBoxSizer.new(8)
    :wxScrolledWindow.setSizer(scroll, scrollSizer)

    :wxWindow.setBackgroundStyle(
      scroll,
      :wxe_util.get_const(:wxBG_STYLE_SYSTEM)
    )

    entries = add_entries(format, list, scroll, scrollSizer, cursor)
    :wxSizer.add(box, scroll, [{:proportion, 1}, {:flag, 8192}])
    :wxSizer.add(outerBox, box, [{:proportion, proportion}, {:flag, 8192}])
    {scroll, scrollSizer, length(entries)}
  end

  defp add_entries(:click, list, scroll, scrollSizer, cursor) do
    add = fn link ->
      tC = link_entry(scroll, link, cursor)

      :wxWindow.setBackgroundStyle(
        tC,
        :wxe_util.get_const(:wxBG_STYLE_SYSTEM)
      )

      :wxSizer.add(scrollSizer, tC, [{:flag, 8192}])
    end

    cond do
      length(list) > 8 ->
        {list1, rest} = :lists.split(8, list)

        linkEntries =
          for link <- list1 do
            add.(link)
          end

        nStr = :erlang.integer_to_list(length(rest))

        tC =
          link_entry2(
            scroll,
            {{:more, {rest, scroll, scrollSizer}}, 'more...'},
            cursor,
            'Click to see ' ++ nStr ++ ' more entries'
          )

        :wxWindow.setBackgroundStyle(
          tC,
          :wxe_util.get_const(:wxBG_STYLE_SYSTEM)
        )

        e = :wxSizer.add(scrollSizer, tC, [{:flag, 8192}])
        linkEntries ++ [e]

      true ->
        for link <- list do
          add.(link)
        end
    end
  end

  defp add_entries(:plain, list, scroll, scrollSizer, _) do
    add = fn string ->
      tC = :wxStaticText.new(scroll, -1, string)
      :wxSizer.add(scrollSizer, tC, [{:flag, 8192}])
    end

    for string <- list do
      add.(string)
    end
  end

  def add_scroll_entries(moreEntry, {list, scroll, scrollSizer}) do
    :wx.batch(fn ->
      :wxSizer.remove(scrollSizer, 8)
      :wxStaticText.destroy(moreEntry)
      cursor = :wxCursor.new(6)

      add = fn link ->
        tC = link_entry(scroll, link, cursor)

        :wxWindow.setBackgroundStyle(
          tC,
          :wxe_util.get_const(:wxBG_STYLE_SYSTEM)
        )

        :wxSizer.add(scrollSizer, tC, [{:flag, 8192}])
      end

      entries =
        for link <- list do
          add.(link)
        end

      :wxCursor.destroy(cursor)
      :wxSizer.layout(scrollSizer)
      :wxSizer.setVirtualSizeHints(scrollSizer, scroll)
      entries
    end)
  end

  defp create_box(_Panel, {:scroll_boxes, []}) do
    :undefined
  end

  defp create_box(panel, {:scroll_boxes, data}) do
    outerBox = :wxBoxSizer.new(4)
    cursor = :wxCursor.new(6)

    addBox = fn
      {title, proportion, format = {_, _}} ->
        add_box(panel, outerBox, cursor, title, proportion, format)

      {title, format = {_, _}} ->
        add_box(panel, outerBox, cursor, title, 1, format)

      :undefined ->
        :undefined
    end

    boxes =
      for entry <- data do
        addBox.(entry)
      end

    :wxCursor.destroy(cursor)

    maxL =
      :lists.foldl(
        fn
          {_, _, l}, max when l > max ->
            l

          _, max ->
            max
        end,
        0,
        boxes
      )

    dummy = :wxTextCtrl.new(panel, -1, [{:style, 2_097_152 ||| 16 ||| 32768}])
    {_, h} = :wxWindow.getSize(dummy)
    :wxTextCtrl.destroy(dummy)

    maxH =
      cond do
        maxL > 8 ->
          8 * h

        true ->
          maxL * h
      end

    for {b, _, _} <- boxes do
      :wxWindow.setMinSize(b, {0, maxH})
    end

    :wxSizer.layout(outerBox)
    {outerBox, boxes}
  end

  defp create_box(parent, data) do
    {title, _Align, info} = get_box_info(data)
    top = :wxStaticBoxSizer.new(8, parent, [{:label, title}])
    panel = :wxPanel.new(parent)
    box = :wxBoxSizer.new(8)
    leftSize = 30 + get_max_width(panel, info)
    rightProportion = [{:flag, 8192}]

    addRow = fn
      {desc0, value0} ->
        desc = desc0 ++ ':'
        line = :wxBoxSizer.new(4)
        label = :wxStaticText.new(panel, -1, desc)
        :wxSizer.add(line, 5, 0)
        :wxSizer.add(line, label)
        :wxSizer.setItemMinSize(line, label, leftSize, -1)

        field =
          case value0 do
            {:click, 'unknown'} ->
              :wxStaticText.new(panel, -1, 'unknown')

            {:click, value} ->
              link_entry(panel, value)

            _ ->
              value = to_str(value0)

              case :string.nth_lexeme(
                     :lists.sublist(
                       value,
                       80
                     ),
                     1,
                     [?\n]
                   ) do
                ^value ->
                  :wxStaticText.new(panel, -1, value)

                shown ->
                  tCtrl = :wxStaticText.new(panel, -1, [shown, '...'])

                  :wxWindow.setToolTip(
                    tCtrl,
                    :wxToolTip.new(value)
                  )

                  tCtrl
              end
          end

        :wxSizer.add(line, 10, 0)
        :wxSizer.add(line, field, rightProportion)
        :wxSizer.add(box, line, [{:proportion, 1}])
        field

      :undefined ->
        :undefined
    end

    infoFields =
      for entry <- info do
        addRow.(entry)
      end

    :wxWindow.setSizer(panel, box)
    :wxSizer.add(top, panel, [{:proportion, 1}, {:flag, 8192}])
    {top, infoFields}
  end

  defp link_entry(panel, link) do
    cursor = :wxCursor.new(6)
    tC = link_entry(panel, link, cursor)
    :wxCursor.destroy(cursor)
    tC
  end

  defp link_entry(panel, link, cursor) do
    link_entry2(panel, to_link(link), cursor)
  end

  defp link_entry2(panel, {target, str}, cursor) do
    link_entry2(panel, {target, str}, cursor, 'Click to see properties for ' ++ str)
  end

  defp link_entry2(panel, {target, str}, cursor, toolTipText) do
    tC = :wxStaticText.new(panel, -1, str)

    :wxWindow.setForegroundColour(
      tC,
      :wxe_util.get_const(:wxBLUE)
    )

    :wxWindow.setCursor(tC, cursor)
    :wxWindow.connect(tC, :left_down, [{:userData, target}])
    :wxWindow.connect(tC, :enter_window)
    :wxWindow.connect(tC, :leave_window)
    toolTip = :wxToolTip.new(toolTipText)
    :wxWindow.setToolTip(tC, toolTip)
    tC
  end

  defp to_link(regName = {name, node})
       when is_atom(name) and
              is_atom(node) do
    str = :io_lib.format('{~tp,~p}', [name, node])
    {regName, str}
  end

  defp to_link(tI = {_Target, _Identifier}) do
    tI
  end

  defp to_link(target0) do
    target = to_str(target0)
    {target, target}
  end

  def html_window(panel) do
    win = :wxHtmlWindow.new(panel, [{:style, 4}])
    :wxHtmlWindow.connect(win, :command_html_link_clicked)
    win
  end

  def html_window(panel, html) do
    win = html_window(panel)
    :wxHtmlWindow.setPage(win, html)
    win
  end

  defp get_max_width(parent, info) do
    :lists.foldl(
      fn
        {desc, _}, max ->
          {w, _, _, _} = :wxWindow.getTextExtent(parent, desc)
          max(w, max)

        _, max ->
          max
      end,
      0,
      info
    )
  end

  def set_listctrl_col_size(lCtrl, total) do
    :wx.batch(fn ->
      calc_last(lCtrl, total)
    end)
  end

  defp calc_last(lCtrl, _Total) do
    cols = :wxListCtrl.getColumnCount(lCtrl)
    {total, _} = :wxWindow.getClientSize(lCtrl)
    sBSize = scroll_size(lCtrl)

    last =
      :lists.foldl(
        fn i, last ->
          last - :wxListCtrl.getColumnWidth(lCtrl, i)
        end,
        total - sBSize,
        :lists.seq(0, cols - 2)
      )

    size = max(150, last)
    :wxListCtrl.setColumnWidth(lCtrl, cols - 1, size)
  end

  defp scroll_size(lCtrl) do
    case :os.type() do
      {:win32, :nt} ->
        0

      {:unix, :darwin} ->
        0

      _ ->
        case :wxWindow.hasScrollbar(lCtrl, 8) do
          true ->
            :wxSystemSettings.getMetric(28)

          false ->
            0
        end
    end
  end

  def user_term(parent, title, default) do
    dialog = :wxTextEntryDialog.new(parent, title, [{:value, default}])

    case :wxTextEntryDialog.showModal(dialog) do
      5100 ->
        str = :wxTextEntryDialog.getValue(dialog)
        :wxTextEntryDialog.destroy(dialog)
        parse_string(ensure_last_is_dot(str))

      5101 ->
        :wxTextEntryDialog.destroy(dialog)
        :cancel
    end
  end

  def user_term_multiline(parent, title, default) do
    dialog = :wxDialog.new(parent, -1, title, [{:style, 536_870_912 ||| 2048 ||| 4096 ||| 64}])
    panel = :wxPanel.new(dialog)
    textCtrl = :wxTextCtrl.new(panel, -1, [{:value, default}, {:style, 70 ||| 32}])
    line = :wxStaticLine.new(panel, [{:style, 4}])
    buttons = :wxDialog.createButtonSizer(dialog, 4 ||| 16)
    innerSizer = :wxBoxSizer.new(8)

    :wxSizer.add(innerSizer, textCtrl, [
      {:flag, 8192 ||| (64 ||| 128 ||| 32 ||| 16)},
      {:proportion, 1},
      {:border, 5}
    ])

    :wxSizer.add(innerSizer, line, [{:flag, 8192}, {:proportion, 0}, {:border, 5}])
    :wxPanel.setSizer(panel, innerSizer)
    topSizer = :wxBoxSizer.new(8)

    :wxSizer.add(topSizer, panel, [
      {:flag, 8192 ||| (64 ||| 128 ||| 32 ||| 16)},
      {:proportion, 1},
      {:border, 5}
    ])

    :wxSizer.add(topSizer, buttons, [{:flag, 8192 ||| 128 ||| 32}, {:border, 10}])
    dC = :wxClientDC.new(panel)
    w = :wxDC.getCharWidth(dC)
    h = :wxDC.getCharHeight(dC)
    {eW, eH} = :wxDC.getMultiLineTextExtent(dC, default)
    :wxSizer.setItemMinSize(innerSizer, 0, eW + 2 * w, eH + h)
    topSize = :wxSizer.getMinSize(topSizer)
    :wxSizer.setItemMinSize(innerSizer, 0, 40 * w, 4 * h)
    :wxWindow.setSizerAndFit(dialog, topSizer)
    :wxSizer.setSizeHints(topSizer, dialog)
    :wxWindow.setClientSize(dialog, topSize)

    case :wxDialog.showModal(dialog) do
      5100 ->
        str = :wxTextCtrl.getValue(textCtrl)
        :wxDialog.destroy(dialog)
        parse_string(ensure_last_is_dot(str))

      5101 ->
        :wxDialog.destroy(dialog)
        :cancel
    end
  end

  defp parse_string(str) do
    try do
      tokens =
        case :erl_scan.string(str, 1, [:text]) do
          {:ok, ts, _} ->
            ts

          {:error, {_SLine, sMod, sError}, _} ->
            throw(:io_lib.format('~ts', [sMod.format_error(sError)]))
        end

      case :erl_eval.extended_parse_term(tokens) do
        {:error, {_PLine, pMod, pError}} ->
          throw(:io_lib.format('~ts', [pMod.format_error(pError)]))

        res ->
          res
      end
    catch
      errStr ->
        {:error, errStr}

      _, _Err ->
        {:error, ['Syntax error in: ', str]}
    end
  end

  defp ensure_last_is_dot([]) do
    '.'
  end

  defp ensure_last_is_dot(string) do
    case :lists.last(string) === ?. do
      true ->
        string

      false ->
        string ++ '.'
    end
  end

  def create_status_bar(panel) do
    statusStyle = 32 ||| 16 ||| 32768
    red = :wxTextAttr.new(:wxe_util.get_const(:wxRED))
    dummy = :wxTextCtrl.new(panel, -1, [{:style, statusStyle}])
    {x, y, _, _} = :wxTextCtrl.getTextExtent(dummy, 'WARNING')
    :wxTextCtrl.destroy(dummy)
    statusBar = :wxTextCtrl.new(panel, -1, [{:style, statusStyle}, {:size, {x, y + 2}}])
    :wxTextCtrl.setDefaultStyle(statusBar, red)
    :wxTextAttr.destroy(red)
    statusBar
  end

  def display_progress_dialog(parent, title, str) do
    caller = self()
    env = :wx.get_env()

    spawn_link(fn ->
      progress_handler(caller, env, parent, title, str)
    end)

    :ok
  end

  def wait_for_progress() do
    receive do
      :continue ->
        :ok

      error ->
        error
    end
  end

  def destroy_progress_dialog() do
    report_progress(:finish)
  end

  def sync_destroy_progress_dialog() do
    ref = :erlang.monitor(:process, :cdv_progress_handler)
    destroy_progress_dialog()

    receive do
      {:DOWN, ^ref, :process, _, _} ->
        :ok
    end
  end

  def report_progress(progress) do
    case :erlang.whereis(:cdv_progress_handler) do
      pid when is_pid(pid) ->
        send(pid, {:progress, progress})
        :ok

      _ ->
        :ok
    end
  end

  defp progress_handler(caller, env, parent, title, str) do
    :erlang.register(:cdv_progress_handler, self())
    :wx.set_env(env)
    pD = progress_dialog(env, parent, title, str)

    try do
      progress_loop(title, pD, caller, :infinity)
    catch
      :closed ->
        :normal
    end
  end

  defp progress_loop(title, pD, caller, pulse) do
    receive do
      {:progress, {:ok, :done}} ->
        send(caller, :continue)
        progress_loop(title, pD, caller, pulse)

      {:progress, {:ok, :start_pulse}} ->
        update_progress_pulse(pD)
        progress_loop(title, pD, caller, 50)

      {:progress, {:ok, :stop_pulse}} ->
        progress_loop(title, pD, caller, :infinity)

      {:progress, {:ok, percent}} when is_integer(percent) ->
        update_progress(pD, percent)
        progress_loop(title, pD, caller, pulse)

      {:progress, {:ok, msg}} ->
        update_progress_text(pD, msg)
        progress_loop(title, pD, caller, pulse)

      {:progress, {:error, reason}} ->
        {dialog, _, _} = pD
        parent = :wxWindow.getParent(dialog)
        finish_progress(pD)

        failMsg =
          cond do
            is_list(reason) ->
              reason

            true ->
              :file.format_error(reason)
          end

        display_info_dialog(parent, 'Crashdump Viewer Error', failMsg)
        send(caller, :error)
        :erlang.unregister(:cdv_progress_handler)
        :erlang.unlink(caller)

      {:progress, :finish} ->
        finish_progress(pD)
        :erlang.unregister(:cdv_progress_handler)
        :erlang.unlink(caller)
    after
      pulse ->
        update_progress_pulse(pD)
        progress_loop(title, pD, caller, 50)
    end
  end

  defp progress_dialog(_Env, parent, title, str) do
    progress_dialog_new(parent, title, str)
  end

  defp update_progress(pD, value) do
    try do
      progress_dialog_update(pD, value)
    catch
      _, _ ->
        throw(:closed)
    end
  end

  defp update_progress_text(pD, text) do
    try do
      progress_dialog_update(pD, text)
    catch
      _, _ ->
        throw(:closed)
    end
  end

  defp update_progress_pulse(pD) do
    try do
      progress_dialog_pulse(pD)
    catch
      _, _ ->
        throw(:closed)
    end
  end

  defp finish_progress(pD) do
    try do
      progress_dialog_update(pD, 100)
    catch
      _, _ ->
        :ok
    after
      progress_dialog_destroy(pD)
    end
  end

  defp progress_dialog_new(parent, title, str) do
    dialog = :wxDialog.new(parent, -1, title, [{:style, 536_870_912 ||| 2048 ||| 4096}])
    panel = :wxPanel.new(dialog)
    sizer = :wxBoxSizer.new(8)
    message = :wxStaticText.new(panel, 1, str, [{:size, {220, -1}}])
    gauge = :wxGauge.new(panel, 2, 100, [{:style, 4}])
    sizerFlags = 8192 ||| 16 ||| 32 ||| 64
    :wxSizer.add(sizer, message, [{:flag, sizerFlags}, {:border, 15}])
    :wxSizer.add(sizer, gauge, [{:flag, sizerFlags ||| 128}, {:border, 15}])
    :wxPanel.setSizer(panel, sizer)
    :wxSizer.setSizeHints(sizer, dialog)
    :wxDialog.show(dialog)
    {dialog, message, gauge}
  end

  defp progress_dialog_update({_, _, gauge}, value) when is_integer(value) do
    :wxGauge.setValue(gauge, value)
  end

  defp progress_dialog_update({_, message, gauge}, text) when is_list(text) do
    :wxGauge.setValue(gauge, 0)
    :wxStaticText.setLabel(message, text)
  end

  defp progress_dialog_pulse({_, _, gauge}) do
    :wxGauge.pulse(gauge)
  end

  defp progress_dialog_destroy({dialog, _, _}) do
    :wxDialog.destroy(dialog)
  end

  def make_obsbin(bin, tab) do
    size = byte_size(bin)

    {preview, previewBitSize} =
      try do
        pL1 = min(:string.length(bin), 10)
        pB1 = :string.slice(bin, 0, pL1)
        pS1 = byte_size(pB1) * 8
        <<p1::size(pS1)>> = pB1
        {p1, pS1}
      catch
        _, _ ->
          pS2 = min(size, 10) * 8
          <<p2::size(pS2), _::binary>> = bin
          {p2, pS2}
      end

    hash = :erlang.phash2(bin)
    key = {preview, size, hash}
    :ets.insert(tab, {key, bin})
    [:"#OBSBin", preview, previewBitSize, size, hash]
  end
end
