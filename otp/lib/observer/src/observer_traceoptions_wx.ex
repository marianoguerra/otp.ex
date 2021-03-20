defmodule :m_observer_traceoptions_wx do
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

  def process_trace(parent, default) do
    dialog =
      :wxDialog.new(parent, -1, 'Process Options', [
        {:style, 536_870_912 ||| 2048 ||| 4096 ||| 64}
      ])

    panel = :wxPanel.new(dialog)
    mainSz = :wxBoxSizer.new(8)
    panelSz = :wxBoxSizer.new(4)
    leftSz = :wxStaticBoxSizer.new(8, panel, [{:label, 'Tracing options'}])
    rightSz = :wxStaticBoxSizer.new(8, panel, [{:label, 'Inheritance options:'}])
    funcBox = :wxCheckBox.new(panel, -1, 'Trace function call', [])
    check_box(funcBox, :lists.member(:functions, default))
    arityBox = :wxCheckBox.new(panel, -1, 'Trace arity instead of arguments', [])
    check_box(arityBox, :lists.member(:functions, default))
    sendBox = :wxCheckBox.new(panel, -1, 'Trace send message', [])
    check_box(sendBox, :lists.member(:send, default))
    recBox = :wxCheckBox.new(panel, -1, 'Trace receive message', [])
    check_box(recBox, :lists.member(:receive, default))
    eventBox = :wxCheckBox.new(panel, -1, 'Trace process events', [])
    check_box(eventBox, :lists.member(:events, default))
    schedBox = :wxCheckBox.new(panel, -1, 'Trace scheduling of processes', [])

    check_box(
      schedBox,
      :lists.member(:running_procs, default)
    )

    exitBox = :wxCheckBox.new(panel, -1, 'Trace scheduling of exiting processes', [])
    check_box(exitBox, :lists.member(:exiting, default))
    gCBox = :wxCheckBox.new(panel, -1, 'Trace garbage collections', [])

    check_box(
      gCBox,
      :lists.member(:garbage_collection, default)
    )

    {spawnBox, spwnAllRadio, spwnFirstRadio} =
      optionpage_top_right(panel, rightSz, [{:flag, 128}, {:border, 5}], 'spawn')

    {linkBox, linkAllRadio, linkFirstRadio} =
      optionpage_top_right(panel, rightSz, [{:flag, 128}, {:border, 5}], 'link')

    spawnBool =
      :lists.member(
        :on_spawn,
        default
      ) or
        :lists.member(
          :on_first_spawn,
          default
        )

    linkBool =
      :lists.member(
        :on_link,
        default
      ) or
        :lists.member(
          :on_first_link,
          default
        )

    check_box(spawnBox, spawnBool)
    check_box(linkBox, linkBool)
    enable(spawnBox, [spwnAllRadio, spwnFirstRadio])
    enable(linkBox, [linkAllRadio, linkFirstRadio])

    for {radio, opt} <- [
          {spwnAllRadio, :on_spawn},
          {spwnFirstRadio, :on_first_spawn},
          {linkAllRadio, :on_link},
          {linkFirstRadio, :on_first_link}
        ] do
      :wxRadioButton.setValue(
        radio,
        :lists.member(opt, default)
      )
    end

    for checkBox <- [funcBox, arityBox, sendBox, recBox, eventBox, schedBox, exitBox, gCBox] do
      :wxSizer.add(leftSz, checkBox, [])
    end

    :wxSizer.add(leftSz, 150, -1)
    :wxSizer.add(panelSz, leftSz, [{:flag, 8192}, {:proportion, 1}])
    :wxSizer.add(panelSz, rightSz, [{:flag, 8192}, {:proportion, 1}])
    :wxPanel.setSizer(panel, panelSz)
    :wxSizer.add(mainSz, panel, [{:flag, 8192}, {:proportion, 1}])
    buttons = :wxDialog.createButtonSizer(dialog, 4 ||| 16)
    :wxSizer.add(mainSz, buttons, [{:flag, 8192 ||| (64 ||| 128 ||| 32 ||| 16)}, {:border, 5}])
    :wxWindow.setSizerAndFit(dialog, mainSz)
    :wxSizer.setSizeHints(mainSz, dialog)

    :wxCheckBox.connect(spawnBox, :command_checkbox_clicked, [
      {:callback,
       fn r_wx(event: r_wxCommand()), _ ->
         enable(
           spawnBox,
           [spwnAllRadio, spwnFirstRadio]
         )
       end}
    ])

    :wxCheckBox.connect(linkBox, :command_checkbox_clicked, [
      {:callback,
       fn r_wx(event: r_wxCommand()), _ ->
         enable(
           linkBox,
           [linkAllRadio, linkFirstRadio]
         )
       end}
    ])

    case :wxDialog.showModal(dialog) do
      5100 ->
        all = [
          {sendBox, :send},
          {recBox, :receive},
          {funcBox, :functions},
          {arityBox, :arity},
          {eventBox, :events},
          {schedBox, :running_procs},
          {exitBox, :exiting},
          {gCBox, :garbage_collection},
          {{spawnBox, spwnAllRadio}, :on_spawn},
          {{spawnBox, spwnFirstRadio}, :on_first_spawn},
          {{linkBox, linkAllRadio}, :on_link},
          {{linkBox, linkFirstRadio}, :on_first_link}
        ]

        check = fn
          {box, radio} ->
            :wxCheckBox.getValue(box) and :wxRadioButton.getValue(radio)

          box ->
            :wxCheckBox.getValue(box)
        end

        opts =
          for {tick, id} <- all, check.(tick) do
            id
          end

        :wxDialog.destroy(dialog)
        :lists.reverse(opts)

      5101 ->
        :wxDialog.destroy(dialog)
        throw(:cancel)
    end
  end

  def port_trace(parent, default) do
    dialog =
      :wxDialog.new(parent, -1, 'Port Options', [{:style, 536_870_912 ||| 2048 ||| 4096 ||| 64}])

    panel = :wxPanel.new(dialog)
    mainSz = :wxBoxSizer.new(8)
    optsSz = :wxStaticBoxSizer.new(8, panel, [{:label, 'Tracing options'}])
    sendBox = :wxCheckBox.new(panel, -1, 'Trace send message', [])
    check_box(sendBox, :lists.member(:send, default))
    recBox = :wxCheckBox.new(panel, -1, 'Trace receive message', [])
    check_box(recBox, :lists.member(:receive, default))
    eventBox = :wxCheckBox.new(panel, -1, 'Trace port events', [])
    check_box(eventBox, :lists.member(:events, default))
    schedBox = :wxCheckBox.new(panel, -1, 'Trace scheduling of ports', [])

    check_box(
      schedBox,
      :lists.member(:running_ports, default)
    )

    for checkBox <- [sendBox, recBox, eventBox, schedBox] do
      :wxSizer.add(optsSz, checkBox, [])
    end

    :wxSizer.add(optsSz, 150, -1)
    :wxPanel.setSizer(panel, optsSz)
    :wxSizer.add(mainSz, panel, [{:flag, 8192}, {:proportion, 1}])
    buttons = :wxDialog.createButtonSizer(dialog, 4 ||| 16)
    :wxSizer.add(mainSz, buttons, [{:flag, 8192 ||| (64 ||| 128 ||| 32 ||| 16)}, {:border, 5}])
    :wxWindow.setSizerAndFit(dialog, mainSz)
    :wxSizer.setSizeHints(mainSz, dialog)

    case :wxDialog.showModal(dialog) do
      5100 ->
        all = [
          {sendBox, :send},
          {recBox, :receive},
          {eventBox, :events},
          {schedBox, :running_ports}
        ]

        opts =
          for {tick, id} <- all,
              :wxCheckBox.getValue(tick) do
            id
          end

        :wxDialog.destroy(dialog)
        :lists.reverse(opts)

      5101 ->
        :wxDialog.destroy(dialog)
        throw(:cancel)
    end
  end

  def trace_pattern(parentPid, parent, node, matchSpecs) do
    try do
      {module, mFAs, matchSpec} =
        case module_selector(parent, node) do
          {:"$trace_event", event} ->
            mS = select_matchspec(parentPid, parent, matchSpecs, event)
            {:Events, [{:Events, event}], mS}

          mod ->
            mFAs0 = function_selector(parent, node, mod)
            mS = select_matchspec(parentPid, parent, matchSpecs, :funcs)
            {mod, mFAs0, mS}
        end

      {module,
       for {m, fA} <- mFAs do
         r_tpattern(m: m, fa: fA, ms: matchSpec)
       end}
    catch
      :cancel ->
        :cancel
    end
  end

  def select_nodes(parent, nodes) do
    choices =
      for x <- nodes do
        {:erlang.atom_to_list(x), x}
      end

    check_selector(parent, choices)
  end

  defp module_selector(parent, node) do
    scale = :observer_wx.get_scale()

    dialog =
      :wxDialog.new(parent, -1, 'Select Module or Event', [
        {:style, 536_870_912 ||| 2048 ||| 4096 ||| 64},
        {:size, {400 * scale, 400 * scale}}
      ])

    panel = :wxPanel.new(dialog)
    panelSz = :wxBoxSizer.new(8)
    mainSz = :wxBoxSizer.new(8)
    txtCtrl = :wxTextCtrl.new(panel, -1)
    listBox = :wxListBox.new(panel, -1, [{:style, 32}])
    :wxSizer.add(panelSz, txtCtrl, [{:flag, 8192}])
    :wxSizer.add(panelSz, listBox, [{:flag, 8192}, {:proportion, 1}])
    :wxPanel.setSizer(panel, panelSz)

    :wxSizer.add(mainSz, panel, [
      {:flag, 8192 ||| (64 ||| 128 ||| 32 ||| 16)},
      {:border, 5},
      {:proportion, 1}
    ])

    buttons = :wxDialog.createButtonSizer(dialog, 4 ||| 16)

    :wxSizer.add(mainSz, buttons, [
      {:flag, 8192 ||| (64 ||| 128 ||| 32 ||| 16)},
      {:border, 5},
      {:proportion, 0}
    ])

    :wxWindow.setSizer(dialog, mainSz)
    okId = :wxDialog.getAffirmativeId(dialog)
    okButt = :wxWindow.findWindowById(okId)
    :wxWindow.disable(okButt)
    :wxWindow.setFocus(txtCtrl)
    modules = get_modules(node)

    events = [
      {'Messages sent', {:"$trace_event", :send}},
      {'Messages received', {:"$trace_event", :receive}}
    ]

    allModules =
      events ++
        for x <- modules do
          {:erlang.atom_to_list(x), x}
        end

    filter_listbox_data('', allModules, listBox)

    :wxTextCtrl.connect(txtCtrl, :command_text_updated, [
      {:callback,
       fn r_wx(event: r_wxCommand(cmdString: input)), _ ->
         filter_listbox_data(input, allModules, listBox)
       end}
    ])

    :wxListBox.connect(
      listBox,
      :command_listbox_doubleclicked,
      [
        {:callback,
         fn _, _ ->
           :wxDialog.endModal(dialog, 5100)
         end}
      ]
    )

    :wxListBox.connect(listBox, :command_listbox_selected, [
      {:callback,
       fn r_wx(event: r_wxCommand(commandInt: id)), _ ->
         id >= 0 and :wxWindow.enable(okButt)
       end}
    ])

    case :wxDialog.showModal(dialog) do
      5100 ->
        selId = :wxListBox.getSelection(listBox)

        case selId >= 0 do
          true ->
            module = :wxListBox.getClientData(listBox, selId)
            :wxDialog.destroy(dialog)
            module

          false ->
            :wxDialog.destroy(dialog)
            throw(:cancel)
        end

      5101 ->
        :wxDialog.destroy(dialog)
        throw(:cancel)
    end
  end

  defp function_selector(parent, node, module) do
    functions = :observer_wx.try_rpc(node, module, :module_info, [:functions])
    externals = :observer_wx.try_rpc(node, module, :module_info, [:exports])

    choices =
      :lists.usort(
        for {name, arity} <- externals ++ functions,
            not :erl_internal.guard_bif(name, arity) do
          {name, arity}
        end
      )

    parsedChoices = parse_function_names(choices)

    case check_selector(parent, parsedChoices) do
      [] ->
        [{module, {:_, :_}}]

      fAs ->
        for {f, a} <- fAs do
          {module, {f, a}}
        end
    end
  end

  defp check_selector(parent, parsedChoices) do
    scale = :observer_wx.get_scale()

    dialog =
      :wxDialog.new(parent, -1, 'Trace Functions', [
        {:style, 536_870_912 ||| 2048 ||| 4096 ||| 64},
        {:size, {400 * scale, 400 * scale}}
      ])

    panel = :wxPanel.new(dialog)
    panelSz = :wxBoxSizer.new(8)
    mainSz = :wxBoxSizer.new(8)
    txtCtrl = :wxTextCtrl.new(panel, -1)
    listBox = :wxCheckListBox.new(panel, -1, [{:style, 128}])
    :wxSizer.add(panelSz, txtCtrl, [{:flag, 8192}])
    :wxSizer.add(panelSz, listBox, [{:flag, 8192}, {:proportion, 1}])
    selAllBtn = :wxButton.new(panel, -1, [{:label, 'Check Visible'}])
    deSelAllBtn = :wxButton.new(panel, -1, [{:label, 'Uncheck Visible'}])
    buttonSz = :wxBoxSizer.new(4)

    for button <- [selAllBtn, deSelAllBtn] do
      :wxSizer.add(buttonSz, button, [])
    end

    :wxSizer.add(panelSz, buttonSz, [
      {:flag, 8192 ||| (64 ||| 128 ||| 32 ||| 16)},
      {:border, 5},
      {:proportion, 0}
    ])

    :wxPanel.setSizer(panel, panelSz)

    :wxSizer.add(mainSz, panel, [
      {:flag, 8192 ||| (64 ||| 128 ||| 32 ||| 16)},
      {:border, 5},
      {:proportion, 1}
    ])

    buttons = :wxDialog.createButtonSizer(dialog, 4 ||| 16)

    :wxSizer.add(mainSz, buttons, [
      {:flag, 8192 ||| (64 ||| 128 ||| 32 ||| 16)},
      {:border, 5},
      {:proportion, 0}
    ])

    :wxWindow.setSizer(dialog, mainSz)
    :wxWindow.setFocus(txtCtrl)
    filter_listbox_data('', parsedChoices, listBox, false)

    :wxTextCtrl.connect(txtCtrl, :command_text_updated, [
      {:callback,
       fn r_wx(event: r_wxCommand(cmdString: input)), _ ->
         filter_listbox_data(input, parsedChoices, listBox, false)
       end}
    ])

    self = self()

    getClientData = fn lB, n ->
      string = :wxListBox.getString(lB, n)
      {_, data} = :lists.keyfind(string, 1, parsedChoices)
      data
    end

    :wxCheckListBox.connect(
      listBox,
      :command_checklistbox_toggled,
      [
        {:callback,
         fn r_wx(event: r_wxCommand(commandInt: n)), _ ->
           send(
             self,
             {listBox,
              :wxCheckListBox.isChecked(
                listBox,
                n
              ),
              getClientData.(
                listBox,
                n
              )}
           )
         end}
      ]
    )

    check = fn id, bool ->
      :wxCheckListBox.check(listBox, id, [{:check, bool}])
      send(self, {listBox, bool, getClientData.(listBox, id)})
    end

    :wxButton.connect(selAllBtn, :command_button_clicked, [
      {:callback,
       fn r_wx(), _ ->
         count = :wxListBox.getCount(listBox)

         for selId <- :lists.seq(0, count - 1),
             not :wxCheckListBox.isChecked(
               listBox,
               selId
             ) do
           check.(selId, true)
         end
       end}
    ])

    :wxButton.connect(deSelAllBtn, :command_button_clicked, [
      {:callback,
       fn r_wx(), _ ->
         count = :wxListBox.getCount(listBox)

         for selId <- :lists.seq(0, count - 1),
             :wxCheckListBox.isChecked(
               listBox,
               selId
             ) do
           check.(selId, false)
         end
       end}
    ])

    case :wxDialog.showModal(dialog) do
      5100 ->
        :wxDialog.destroy(dialog)
        get_checked(listBox, [])

      5101 ->
        :wxDialog.destroy(dialog)
        get_checked(listBox, [])
        throw(:cancel)
    end
  end

  defp get_checked(listBox, acc) do
    receive do
      {^listBox, true, fA} ->
        get_checked(listBox, [fA | :lists.delete(fA, acc)])

      {^listBox, false, fA} ->
        get_checked(listBox, :lists.delete(fA, acc))
    after
      0 ->
        :lists.reverse(acc)
    end
  end

  def select_matchspec(pid, parent, allMatchSpecs, key) do
    {matchSpecs, restMS} =
      case :lists.keytake(key, 1, allMatchSpecs) do
        {:value, {^key, mSs0}, rest} ->
          {mSs0, rest}

        false ->
          {[], allMatchSpecs}
      end

    scale = :observer_wx.get_scale()

    dialog =
      :wxDialog.new(parent, -1, 'Trace Match Specifications', [
        {:style, 536_870_912 ||| 2048 ||| 4096 ||| 64},
        {:size, {400 * scale, 400 * scale}}
      ])

    panel = :wxPanel.new(dialog)
    panelSz = :wxBoxSizer.new(8)
    mainSz = :wxBoxSizer.new(8)
    txtSz = :wxStaticBoxSizer.new(8, panel, [{:label, 'Match specification:'}])
    btnSz = :wxBoxSizer.new(4)
    savedSz = :wxStaticBoxSizer.new(8, panel, [{:label, 'Saved match specifications:'}])
    textCtrl = create_styled_txtctrl(panel)
    :wxSizer.add(txtSz, textCtrl, [{:flag, 8192}, {:proportion, 1}])
    addMsBtn = :wxButton.new(panel, -1, [{:label, 'New'}])
    editMsBtn = :wxButton.new(panel, -1, [{:label, 'Edit'}])
    delMsBtn = :wxButton.new(panel, -1, [{:label, 'Delete'}])
    :wxSizer.add(btnSz, addMsBtn)
    :wxSizer.add(btnSz, editMsBtn)
    :wxSizer.add(btnSz, delMsBtn)
    listBox = :wxListBox.new(panel, -1, [])
    :wxSizer.add(savedSz, listBox, [{:flag, 8192}, {:proportion, 1}])
    :wxSizer.add(panelSz, txtSz, [{:flag, 8192}, {:proportion, 1}])
    :wxSizer.add(panelSz, btnSz)
    :wxSizer.add(panelSz, savedSz, [{:flag, 8192}, {:proportion, 1}])
    :wxWindow.setSizer(panel, panelSz)

    :wxSizer.add(mainSz, panel, [
      {:flag, 8192 ||| (64 ||| 128 ||| 32 ||| 16)},
      {:border, 5},
      {:proportion, 1}
    ])

    buttons = :wxDialog.createButtonSizer(dialog, 4 ||| 16)

    :wxSizer.add(mainSz, buttons, [
      {:flag, 8192 ||| (64 ||| 128 ||| 32 ||| 16)},
      {:border, 5},
      {:proportion, 0}
    ])

    :wxWindow.setSizer(dialog, mainSz)
    okId = :wxDialog.getAffirmativeId(dialog)
    okButt = :wxWindow.findWindowById(okId)
    :wxWindow.disable(okButt)
    :wxWindow.disable(editMsBtn)
    :wxWindow.disable(delMsBtn)
    choices = ms_names(matchSpecs)
    filter_listbox_data('', choices, listBox)

    add = fn _, _ ->
      case edit_ms(textCtrl, :new, dialog) do
        ms = r_match_spec() ->
          add_and_select(-1, ms, listBox)
          :wxWindow.enable(okButt)
          :wxWindow.enable(editMsBtn)
          :wxWindow.enable(delMsBtn)

        else__ ->
          else__
      end
    end

    edit = fn _, _ ->
      selId = :wxListBox.getSelection(listBox)

      case selId >= 0 do
        true ->
          r_match_spec(name: name) = :wxListBox.getClientData(listBox, selId)

          case edit_ms(textCtrl, name, dialog) do
            ms = r_match_spec() ->
              add_and_select(selId, ms, listBox)
              :wxWindow.enable(okButt)
              :wxWindow.enable(editMsBtn)
              :wxWindow.enable(delMsBtn)

            else__ ->
              else__
          end

        false ->
          :ok
      end
    end

    del = fn _, _ ->
      selId = :wxListBox.getSelection(listBox)

      case selId >= 0 do
        true ->
          :wxListBox.delete(listBox, selId)

        false ->
          :ok
      end
    end

    sel = fn r_wx(event: r_wxCommand(commandInt: id)), _ ->
      case id >= 0 do
        true ->
          :wxWindow.enable(okButt)
          :wxWindow.enable(editMsBtn)
          :wxWindow.enable(delMsBtn)
          r_match_spec(func: str) = :wxListBox.getClientData(listBox, id)
          :wxStyledTextCtrl.setText(textCtrl, str)

        false ->
          try do
            :wxWindow.disable(okButt)
            :wxWindow.disable(editMsBtn)
            :wxWindow.disable(delMsBtn)
          catch
            _, _ ->
              :ok
          end
      end
    end

    :wxButton.connect(addMsBtn, :command_button_clicked, [{:callback, add}])
    :wxButton.connect(editMsBtn, :command_button_clicked, [{:callback, edit}])
    :wxButton.connect(delMsBtn, :command_button_clicked, [{:callback, del}])
    :wxListBox.connect(listBox, :command_listbox_selected, [{:callback, sel}])

    case :wxDialog.showModal(dialog) do
      5100 ->
        selId = :wxListBox.getSelection(listBox)
        count = :wxListBox.getCount(listBox)

        mSs =
          for id <- :lists.seq(0, count - 1) do
            :wxListBox.getClientData(listBox, id)
          end

        send(pid, {:update_ms, [{key, mSs} | restMS]})
        mS = :lists.nth(selId + 1, mSs)
        :wxDialog.destroy(dialog)
        mS

      5101 ->
        :wxDialog.destroy(dialog)
        throw(:cancel)
    end
  end

  def output(parent, default) do
    dialog =
      :wxDialog.new(parent, -1, 'Process Options', [
        {:style, 536_870_912 ||| 2048 ||| 4096 ||| 64}
      ])

    panel = :wxPanel.new(dialog)
    mainSz = :wxBoxSizer.new(8)
    panelSz = :wxStaticBoxSizer.new(8, panel, [{:label, 'Output'}])
    winB = :wxCheckBox.new(panel, -1, 'Window', [])

    check_box(
      winB,
      :proplists.get_value(:window, default, true)
    )

    shellB = :wxCheckBox.new(panel, -1, 'Shell', [])

    check_box(
      shellB,
      :proplists.get_value(:shell, default, false)
    )

    for checkBox <- [winB, shellB] do
      :wxSizer.add(panelSz, checkBox, [])
    end

    getFileOpts = ttb_file_options(panel, panelSz, default)
    :wxPanel.setSizer(panel, panelSz)

    :wxSizer.add(mainSz, panel, [
      {:flag, 8192 ||| (64 ||| 128 ||| 32 ||| 16)},
      {:proportion, 1},
      {:border, 3}
    ])

    buttons = :wxDialog.createButtonSizer(dialog, 4 ||| 16)
    :wxSizer.add(mainSz, buttons, [{:flag, 8192 ||| (64 ||| 128 ||| 32 ||| 16)}, {:border, 5}])
    :wxWindow.setSizerAndFit(dialog, mainSz)
    :wxSizer.setSizeHints(mainSz, dialog)

    case :wxDialog.showModal(dialog) do
      5100 ->
        res = [
          {:window, :wxCheckBox.getValue(winB)},
          {:shell, :wxCheckBox.getValue(shellB)}
          | getFileOpts.()
        ]

        :wxDialog.destroy(dialog)
        res

      5101 ->
        :wxDialog.destroy(dialog)
        throw(:cancel)
    end
  end

  defp edit_ms(textCtrl, label0, parent) do
    str = ensure_last_is_dot(:wxStyledTextCtrl.getText(textCtrl))

    try do
      matchSpec = ms_from_string(str)

      label =
        case label0 == :new do
          true ->
            get_label(parent)

          _ ->
            label0
        end

      r_match_spec(
        name: label,
        term: matchSpec,
        str: :io_lib.format('~tw', [matchSpec]),
        func: str
      )
    catch
      :cancel ->
        :ok

      error ->
        :observer_wx.create_txt_dialog(parent, error, 'Error', 512)
        :ok
    end
  end

  defp get_label(frame) do
    dialog = :wxTextEntryDialog.new(frame, 'Enter alias: ')

    case :wxDialog.showModal(dialog) do
      5100 ->
        :wxTextEntryDialog.getValue(dialog)

      5101 ->
        throw(:cancel)
    end
  end

  defp ms_from_string(str) do
    try do
      tokens =
        case :erl_scan.string(str) do
          {:ok, ts, _} ->
            ts

          {:error, {sLine, sMod, sError}, _} ->
            throw(
              :io_lib.format(
                '~w: ~ts',
                [sLine, sMod.format_error(sError)]
              )
            )
        end

      exprs =
        case :erl_parse.parse_exprs(tokens) do
          {:ok, t} ->
            t

          {:error, {pLine, pMod, pError}} ->
            throw(
              :io_lib.format(
                '~w: ~ts',
                [pLine, pMod.format_error(pError)]
              )
            )
        end

      term =
        case exprs do
          [{:fun, _, {:clauses, clauses}} | _] ->
            case :ms_transform.transform_from_shell(:dbg, clauses, :orddict.new()) do
              {:error, [{_, [{mSLine, mod, mSInfo}]}], _} ->
                throw(
                  :io_lib.format(
                    '~w: ~tp',
                    [mSLine, mod.format_error(mSInfo)]
                  )
                )

              {:error, _} ->
                throw('Could not convert fun() to match spec')

              ms ->
                ms
            end

          [expr | _] ->
            :erl_parse.normalise(expr)
        end

      case :erlang.match_spec_test([], term, :trace) do
        {:ok, _, _, _} ->
          term

        {:error, list} ->
          throw(
            for {_, error} <- list do
              [error, ?\n]
            end
          )
      end
    catch
      :error, _Reason ->
        throw('Invalid term')
    end
  end

  defp add_and_select(id, mS0, listBox) do
    [{str, user}] = ms_names([mS0])

    sel =
      case id >= 0 do
        true ->
          :wxListBox.setString(listBox, id, str)
          :wxListBox.setClientData(listBox, id, user)
          id

        false ->
          :wxListBox.append(listBox, str, user)
      end

    :wxListBox.setSelection(listBox, sel)
  end

  defp filter_listbox_data(input, data, listBox) do
    filter_listbox_data(input, data, listBox, true)
  end

  defp filter_listbox_data(input, data, listBox, addClientData) do
    filteredData =
      for x = {str, _} <- data,
          :re.run(str, input, [:unicode]) !== :nomatch do
        x
      end

    :wxListBox.clear(listBox)

    :wxListBox.appendStrings(
      listBox,
      for {str, _} <- filteredData do
        str
      end
    )

    addClientData and
      :wx.foldl(
        fn {_, term}, n ->
          :wxListBox.setClientData(listBox, n, term)
          n + 1
        end,
        0,
        filteredData
      )

    filteredData
  end

  defp get_modules(node) do
    :lists.sort(
      for {module, _} <- :observer_wx.try_rpc(node, :code, :all_loaded, []) do
        module
      end
    )
  end

  defp optionpage_top_right(panel, topRightSz, options, text) do
    sizer = :wxBoxSizer.new(8)
    chkBox = :wxCheckBox.new(panel, -1, 'Inherit on ' ++ text, [])
    radioSz = :wxBoxSizer.new(8)
    radio1 = :wxRadioButton.new(panel, -1, 'All ' ++ text, [{:style, 4}])
    radio2 = :wxRadioButton.new(panel, -1, 'First ' ++ text ++ ' only', [])
    :wxSizer.add(sizer, chkBox, [])
    :wxSizer.add(radioSz, radio1, [])
    :wxSizer.add(radioSz, radio2, [])
    :wxSizer.add(sizer, radioSz, [{:flag, 16}, {:border, 20}])
    :wxSizer.add(topRightSz, sizer, options)
    {chkBox, radio1, radio2}
  end

  defp create_styled_txtctrl(parent) do
    fixedFont = :observer_wx.get_attrib({:font, :fixed})
    ed = :wxStyledTextCtrl.new(parent)
    :wxStyledTextCtrl.styleClearAll(ed)
    :wxStyledTextCtrl.styleSetFont(ed, 32, fixedFont)
    :wxStyledTextCtrl.setLexer(ed, 53)
    :wxStyledTextCtrl.setMarginType(ed, 1, 1)
    :wxStyledTextCtrl.setSelectionMode(ed, 2)
    :wxStyledTextCtrl.setUseHorizontalScrollBar(ed, false)

    styles = [
      {0, {0, 0, 0}},
      {1, {160, 53, 35}},
      {2, {150, 100, 40}},
      {3, {5, 5, 100}},
      {4, {130, 40, 172}},
      {5, {170, 45, 132}},
      {6, {30, 0, 0}},
      {7, {0, 0, 0}},
      {8, {64, 102, 244}},
      {9, {236, 155, 172}},
      {10, {40, 144, 170}},
      {11, {40, 100, 20}},
      {12, {0, 0, 0}},
      {13, {0, 0, 0}}
    ]

    setStyle = fn {style, color} ->
      :wxStyledTextCtrl.styleSetFont(ed, style, fixedFont)
      :wxStyledTextCtrl.styleSetForeground(ed, style, color)
    end

    for style <- styles do
      setStyle.(style)
    end

    :wxStyledTextCtrl.setKeyWords(ed, 0, keyWords())
    ed
  end

  defp keyWords() do
    l = [
      'after',
      'begin',
      'case',
      'try',
      'cond',
      'catch',
      'andalso',
      'orelse',
      'end',
      'fun',
      'if',
      'let',
      'of',
      'receive',
      'when',
      'bnot',
      'not',
      'div',
      'rem',
      'band',
      'and',
      'bor',
      'bxor',
      'bsl',
      'bsr',
      'or',
      'xor'
    ]

    :lists.flatten(
      for k <- l do
        k ++ ' '
      end ++ [0]
    )
  end

  defp enable(checkBox, radio) do
    case :wxCheckBox.isChecked(checkBox) do
      false ->
        for r <- radio do
          :wxWindow.disable(r)
        end

      true ->
        for r <- radio do
          :wxWindow.enable(r)
        end
    end
  end

  defp check_box(chkBox, bool) do
    case bool do
      true ->
        :wxCheckBox.set3StateValue(chkBox, 1)

      false ->
        :ignore
    end
  end

  defp parse_function_names(choices) do
    strList =
      for term = {name, arity} <- choices do
        {:erlang.atom_to_list(name) ++ '/' ++ :erlang.integer_to_list(arity), term}
      end

    parse_function_names(strList, [])
  end

  defp parse_function_names([], acc) do
    :lists.reverse(acc)
  end

  defp parse_function_names([{h, term} | t], acc) do
    isFun = :re.run(h, '.*-fun-\\d*?-', [:unicode, :ucp])
    isLc = :re.run(h, '.*-lc\\$\\^\\d*?/\\d*?-\\d*?-', [:unicode, :ucp])
    isLbc = :re.run(h, '.*-lbc\\$\\^\\d*?/\\d*?-\\d*?-', [:unicode, :ucp])

    parsed =
      cond do
        isFun !== :nomatch ->
          'Fun: ' ++ h

        isLc !== :nomatch ->
          'List comprehension: ' ++ h

        isLbc !== :nomatch ->
          'Bit comprehension: ' ++ h

        true ->
          h
      end

    parse_function_names(t, [{parsed, term} | acc])
  end

  defp ms_names(matchSpecList) do
    msOrAlias = fn r_match_spec(name: a, str: m) ->
      case a do
        '' ->
          m

        _ ->
          a ++ '    ' ++ m
      end
    end

    for x <- matchSpecList do
      {msOrAlias.(x), x}
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

  defp ttb_file_options(panel, sizer, default) do
    top = :wxBoxSizer.new(8)
    nameS = :wxBoxSizer.new(4)
    fileBox = :wxCheckBox.new(panel, -1, 'File (Using ttb file tracer)', [])

    check_box(
      fileBox,
      :proplists.get_value(:file, default, false)
    )

    :wxSizer.add(sizer, fileBox)
    desc = :wxStaticText.new(panel, -1, 'File')
    fileName = :proplists.get_value(:filename, default, 'ttb')
    fileT = :wxTextCtrl.new(panel, -1, [{:size, {150, -1}}, {:value, fileName}])
    fileB = :wxButton.new(panel, -1, [{:label, 'Browse'}])
    :wxSizer.add(nameS, desc, [{:proportion, 0}, {:flag, 2048}])
    :wxSizer.add(nameS, fileT, [{:proportion, 1}, {:flag, 8192 ||| 2048}])
    :wxSizer.add(nameS, fileB, [{:proportion, 0}, {:flag, 2048}])
    wrapB = :wxCheckBox.new(panel, -1, 'Wrap logs')

    wrapSz =
      :wxSlider.new(panel, -1, :proplists.get_value(:wrap_sz, default, 128), 64, 10 * 1024, [
        {:style, 4 ||| :wxe_util.get_const(:wxSL_LABELS)}
      ])

    wrapC =
      :wxSlider.new(panel, -1, :proplists.get_value(:wrap_c, default, 8), 2, 100, [
        {:style, 4 ||| :wxe_util.get_const(:wxSL_LABELS)}
      ])

    :wxSizer.add(top, nameS, [{:flag, 8192}])
    :wxSizer.add(top, wrapB, [])
    :wxSizer.add(top, wrapSz, [{:flag, 8192}])
    :wxSizer.add(top, wrapC, [{:flag, 8192}])
    :wxSizer.add(sizer, top, [{:flag, 8192 ||| 16}, {:border, 20}])

    enable = fn useFile, useWrap0 ->
      useWrap = useFile and useWrap0

      for w <- [desc, fileT, fileB, wrapB] do
        :wxWindow.enable(w, [{:enable, useFile}])
      end

      for w <- [wrapSz, wrapC] do
        :wxWindow.enable(w, [{:enable, useWrap}])
      end

      check_box(wrapB, useWrap0)
    end

    enable.(
      :proplists.get_value(:file, default, false),
      :proplists.get_value(:wrap, default, false)
    )

    :wxPanel.connect(fileBox, :command_checkbox_clicked, [
      {:callback,
       fn _, _ ->
         enable.(
           :wxCheckBox.getValue(fileBox),
           :wxCheckBox.getValue(wrapB)
         )
       end}
    ])

    :wxPanel.connect(wrapB, :command_checkbox_clicked, [
      {:callback,
       fn _, _ ->
         enable.(true, :wxCheckBox.getValue(wrapB))
       end}
    ])

    :wxPanel.connect(fileB, :command_button_clicked, [
      {:callback,
       fn _, _ ->
         get_file(fileT)
       end}
    ])

    fn ->
      [
        {:file, :wxCheckBox.getValue(fileBox)},
        {:filename, :wxTextCtrl.getValue(fileT)},
        {:wrap, :wxCheckBox.getValue(wrapB)},
        {:wrap_sz, :wxSlider.getValue(wrapSz)},
        {:wrap_c, :wxSlider.getValue(wrapC)}
      ]
    end
  end

  defp get_file(text) do
    str = :wxTextCtrl.getValue(text)

    dialog =
      :wxFileDialog.new(
        text,
        [{:message, 'Select a file'}, {:defaultFile, str}]
      )

    case :wxDialog.showModal(dialog) do
      5100 ->
        dir = :wxFileDialog.getDirectory(dialog)
        file = :wxFileDialog.getFilename(dialog)
        :wxTextCtrl.setValue(text, :filename.join(dir, file))

      _ ->
        :ok
    end

    :wxFileDialog.destroy(dialog)
  end
end
