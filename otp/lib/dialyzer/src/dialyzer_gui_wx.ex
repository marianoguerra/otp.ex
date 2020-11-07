defmodule :m_dialyzer_gui_wx do
  use Bitwise
  require Record

  Record.defrecord(:r_analysis, :analysis,
    analysis_pid: :undefined,
    type: :succ_typings,
    defines: [],
    doc_plt: :undefined,
    files: [],
    include_dirs: [],
    start_from: :byte_code,
    plt: :undefined,
    use_contracts: true,
    race_detection: false,
    behaviours_chk: false,
    timing: false,
    timing_server: :none,
    callgraph_file: '',
    solvers: :undefined
  )

  Record.defrecord(:r_options, :options,
    files: [],
    files_rec: [],
    analysis_type: :succ_typings,
    timing: false,
    defines: [],
    from: :byte_code,
    get_warnings: :maybe,
    init_plts: [],
    include_dirs: [],
    output_plt: :none,
    legal_warnings: :ordsets.new(),
    report_mode: :normal,
    erlang_mode: false,
    use_contracts: true,
    output_file: :none,
    output_format: :formatted,
    filename_opt: :basename,
    indent_opt: true,
    callgraph_file: '',
    check_plt: true,
    solvers: [],
    native: :maybe,
    native_cache: true
  )

  Record.defrecord(:r_contract, :contract, contracts: [], args: [], forms: [])

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

  Record.defrecord(:r_menu, :menu,
    file: :undefined,
    warnings: :undefined,
    plt: :undefined,
    options: :undefined,
    help: :undefined
  )

  Record.defrecord(:r_gui_state, :gui_state,
    add: :undefined,
    add_dir: :undefined,
    add_rec: :undefined,
    chosen_box: :undefined,
    del_file: :undefined,
    doc_plt: :undefined,
    clear_chosen: :undefined,
    clear_log: :undefined,
    explain_warn: :undefined,
    clear_warn: :undefined,
    init_plt: :undefined,
    dir_entry: :undefined,
    file_box: :undefined,
    files_to_analyze: :undefined,
    gui: :undefined,
    log: :undefined,
    menu: :undefined,
    mode: :undefined,
    options: :undefined,
    run: :undefined,
    stop: :undefined,
    frame: :undefined,
    warnings_box: :undefined,
    explanation_box: :undefined,
    wantedWarnings: :undefined,
    rawWarnings: :undefined,
    backend_pid: :undefined,
    expl_pid: :undefined
  )

  def start(dialyzerOptions) do
    :erlang.process_flag(:trap_exit, true)
    wx = :wx.new()

    state =
      :wx.batch(fn ->
        create_window(wx, dialyzerOptions)
      end)

    gui_loop(state)
  end

  defp create_window(
         wx,
         r_options(init_plts: initPltFiles) = dialyzerOptions
       ) do
    {:ok, host} = :inet.gethostname()
    frame = :wxFrame.new(wx, -1, 'Dialyzer ' ++ :EFE_TODO_VSN_MACRO ++ ' @ ' ++ host)
    :wxFrame.connect(frame, :close_window)
    fileMenu = createFileMenu()
    warningsMenu = createWarningsMenu()
    pltMenu = createPltMenu()
    optionsMenu = createOptionsMenu()
    helpMenu = createHelpMenu()
    menuBar = :wxMenuBar.new()
    :wxMenuBar.append(menuBar, fileMenu, 'File')
    :wxMenuBar.append(menuBar, warningsMenu, 'Warnings')
    :wxMenuBar.append(menuBar, pltMenu, 'Plt')
    :wxMenuBar.append(menuBar, optionsMenu, 'Options')
    :wxMenuBar.append(menuBar, helpMenu, 'Help')
    :wxFrame.setMenuBar(frame, menuBar)
    :ok = :wxFrame.connect(frame, :command_menu_selected)
    lab1 = :wxStaticText.new(frame, 502, 'Directories or modules to analyze')
    optionsLabel = :wxStaticText.new(frame, 503, 'Analysis Options')
    logLabel = :wxStaticText.new(frame, 504, 'Log')
    fileLabel = :wxStaticText.new(frame, 505, 'File: ')
    dirLabel = :wxStaticText.new(frame, 505, 'Dir: ')
    warningsLabel = :wxStaticText.new(frame, 506, 'Warnings')

    chosenBox =
      :wxListBox.new(frame, 510, [{:size, {250, 200}}, {:style, 128 ||| 1_073_741_824 ||| 512}])

    logBox =
      :wxTextCtrl.new(frame, 511, [{:size, {530, 200}}, {:style, 32 ||| 16 ||| 1_073_741_824}])

    defaultPath = :code.root_dir()

    filePicker =
      :wxFilePickerCtrl.new(frame, 512, [
        {:path, defaultPath},
        {:message, 'Choose File to Analyse'},
        {:style, 8192 ||| 2}
      ])

    :wxPickerBase.setTextCtrlProportion(filePicker, 3)
    :wxPickerBase.setPickerCtrlProportion(filePicker, 2)

    dirPicker =
      :wxDirPickerCtrl.new(frame, 513, [
        {:path, defaultPath},
        {:message, 'Choose Directory to Analyze'},
        {:style, 8 ||| 2}
      ])

    warningsBox =
      :wxListBox.new(frame, 521, [{:size, {700, 200}}, {:style, 1_073_741_824 ||| 512}])

    deleteButton = :wxButton.new(frame, 514, [{:label, 'Delete'}])
    deleteAllButton = :wxButton.new(frame, 515, [{:label, 'Delete All'}])

    fileType =
      :wxRadioBox.new(frame, 524, ' File Type: ', {1, 1}, {150, 90}, [
        ['BeamFiles'],
        ['SourceFiles']
      ])

    clearLogButton = :wxButton.new(frame, 516, [{:label, 'Clear Log'}])
    addButton = :wxButton.new(frame, 517, [{:label, 'Add'}])
    addDirButton = :wxButton.new(frame, 532, [{:label, 'Add Dir'}])
    addRecButton = :wxButton.new(frame, 518, [{:label, 'Add Recursively'}])
    explainWarnButton = :wxButton.new(frame, 523, [{:label, 'Explain Warning'}])
    clearWarningsButton = :wxButton.new(frame, 519, [{:label, 'Clear Warnings'}])
    runButton = :wxButton.new(frame, 520, [{:label, 'Run'}])
    stopButton = :wxButton.new(frame, 522, [{:label, 'Stop'}])
    :wxWindow.disable(stopButton)
    :wxButton.connect(deleteButton, :command_button_clicked)

    :wxButton.connect(
      deleteAllButton,
      :command_button_clicked
    )

    :wxButton.connect(
      clearLogButton,
      :command_button_clicked
    )

    :wxButton.connect(addButton, :command_button_clicked)
    :wxButton.connect(addDirButton, :command_button_clicked)
    :wxButton.connect(addRecButton, :command_button_clicked)

    :wxButton.connect(
      explainWarnButton,
      :command_button_clicked
    )

    :wxButton.connect(
      clearWarningsButton,
      :command_button_clicked
    )

    :wxButton.connect(runButton, :command_button_clicked)
    :wxButton.connect(stopButton, :command_button_clicked)
    all = :wxBoxSizer.new(8)
    top = :wxBoxSizer.new(4)
    left = :wxBoxSizer.new(8)
    right = :wxBoxSizer.new(8)
    rightUp = :wxBoxSizer.new(4)
    opts = [{:flag, 8192 ||| (64 ||| 128 ||| 32 ||| 16)}, {:proportion, 1}, {:border, 1}]
    opts3 = [{:flag, 8192 ||| (64 ||| 128 ||| 32 ||| 16)}, {:proportion, 3}, {:border, 1}]
    center = [{:flag, 256}]
    chooseItem = :wxBoxSizer.new(8)
    fileTypeItem = :wxBoxSizer.new(8)
    logItem = :wxBoxSizer.new(8)
    fileDirItem = :wxBoxSizer.new(8)
    fileItem = :wxBoxSizer.new(4)
    dirItem = :wxBoxSizer.new(4)
    addDirButtons = :wxBoxSizer.new(4)
    warningsItem = :wxBoxSizer.new(8)
    chooseButtons = :wxBoxSizer.new(4)
    warnButtons = :wxBoxSizer.new(4)
    runButtons = :wxBoxSizer.new(4)
    buttons = :wxFlexGridSizer.new(3)

    _ =
      :wxSizer.add(chooseButtons, deleteButton, [{:flag, 64 ||| 128 ||| 32 ||| 16}, {:border, 2}])

    _ =
      :wxSizer.add(chooseButtons, deleteAllButton, [
        {:flag, 64 ||| 128 ||| 32 ||| 16},
        {:border, 2}
      ])

    _ = :wxSizer.add(chooseItem, lab1, center)
    _ = :wxSizer.add(chooseItem, chosenBox, opts)
    _ = :wxSizer.add(chooseItem, chooseButtons, [{:flag, 64 ||| 128 ||| 32 ||| 16}, {:border, 2}])
    _ = :wxSizer.add(fileTypeItem, optionsLabel)
    _ = :wxSizer.add(fileTypeItem, fileType, [{:border, 5}, {:flag, 64 ||| 128 ||| 32 ||| 16}])
    _ = :wxSizer.add(logItem, logLabel, center)
    _ = :wxSizer.add(logItem, logBox, opts3)
    _ = :wxSizer.add(logItem, clearLogButton, [{:flag, 64 ||| 128 ||| 32 ||| 16}, {:border, 2}])
    _ = :wxSizer.add(fileItem, fileLabel)
    _ = :wxSizer.add(fileItem, filePicker)
    _ = :wxSizer.add(dirItem, dirLabel)
    _ = :wxSizer.add(dirItem, dirPicker)

    _ =
      :wxSizer.add(addDirButtons, addDirButton, [{:flag, 64 ||| 128 ||| 32 ||| 16}, {:border, 2}])

    _ =
      :wxSizer.add(addDirButtons, addRecButton, [{:flag, 64 ||| 128 ||| 32 ||| 16}, {:border, 2}])

    _ = :wxSizer.add(fileDirItem, fileItem)
    _ = :wxSizer.add(fileDirItem, addButton, [{:flag, 64 ||| 128 ||| 32 ||| 16}, {:border, 2}])
    _ = :wxSizer.add(fileDirItem, dirItem, [{:flag, 64 ||| 128 ||| 32 ||| 16}, {:border, 2}])

    _ =
      :wxSizer.add(fileDirItem, addDirButtons, [{:flag, 64 ||| 128 ||| 32 ||| 16}, {:border, 2}])

    _ =
      :wxSizer.add(warnButtons, explainWarnButton, [
        {:flag, 64 ||| 128 ||| 32 ||| 16},
        {:border, 2}
      ])

    _ =
      :wxSizer.add(warnButtons, clearWarningsButton, [
        {:flag, 64 ||| 128 ||| 32 ||| 16},
        {:border, 2}
      ])

    _ = :wxSizer.add(runButtons, runButton, [{:flag, 64 ||| 128 ||| 32 ||| 16}, {:border, 2}])
    _ = :wxSizer.add(runButtons, stopButton, [{:flag, 64 ||| 128 ||| 32 ||| 16}, {:border, 2}])
    _ = :wxSizer.add(buttons, warnButtons)
    _ = :wxSizer.add(buttons, :wxStaticText.new(frame, 507, ''), [{:flag, 8192}])
    _ = :wxSizer.add(buttons, runButtons)
    _ = :wxFlexGridSizer.addGrowableCol(buttons, 1)
    _ = :wxSizer.add(warningsItem, warningsLabel, center)
    _ = :wxSizer.add(warningsItem, warningsBox, opts3)

    _ =
      :wxSizer.add(warningsItem, buttons, [
        {:flag, 8192 ||| (64 ||| 128 ||| 32 ||| 16)},
        {:border, 2}
      ])

    _ = :wxSizer.add(left, chooseItem, opts)
    _ = :wxSizer.add(left, fileDirItem, [{:proportion, 1}, {:border, 60}, {:flag, 64}])
    _ = :wxSizer.add(rightUp, fileTypeItem, [{:flag, 64 ||| 128 ||| 32 ||| 16}, {:border, 2}])
    _ = :wxSizer.add(rightUp, logItem, opts3)
    _ = :wxSizer.add(right, rightUp, opts3)
    _ = :wxSizer.add(right, warningsItem, opts3)
    _ = :wxSizer.add(top, left, opts)
    _ = :wxSizer.add(top, right, opts3)
    _ = :wxSizer.add(all, top, opts)
    :wxWindow.setSizer(frame, all)
    :wxWindow.setSizeHints(frame, {1150, 600})
    :wxWindow.show(frame)

    warnings = [
      {:warn_return_no_exit, 207},
      {:warn_return_only_exit, 206},
      {:warn_not_called, 205},
      {:warn_non_proper_list, 204},
      {:warn_fun_app, 202},
      {:warn_matching, 200},
      {:warn_opaque, 203},
      {:warn_failing_call, 201},
      {:warn_callgraph, 208},
      {:warn_race_condition, 209},
      {:warn_contract_types, 210},
      {:warn_contract_syntax, 211}
    ]

    menu =
      r_menu(
        file: fileMenu,
        warnings: warningsMenu,
        plt: pltMenu,
        options: optionsMenu,
        help: helpMenu
      )

    initPlt =
      case initPltFiles do
        [] ->
          :dialyzer_plt.new()

        _ ->
          plts =
            for f <- initPltFiles do
              :dialyzer_plt.from_file(f)
            end

          :dialyzer_plt.merge_plts_or_report_conflicts(
            initPltFiles,
            plts
          )
      end

    r_gui_state(
      add: addButton,
      add_dir: addDirButton,
      add_rec: addRecButton,
      chosen_box: chosenBox,
      clear_chosen: deleteAllButton,
      clear_log: clearLogButton,
      explain_warn: explainWarnButton,
      clear_warn: clearWarningsButton,
      del_file: deleteButton,
      doc_plt: :dialyzer_plt.new(),
      dir_entry: dirPicker,
      file_box: filePicker,
      files_to_analyze: :ordsets.new(),
      gui: wx,
      init_plt: initPlt,
      log: logBox,
      menu: menu,
      mode: fileType,
      options: dialyzerOptions,
      run: runButton,
      stop: stopButton,
      frame: frame,
      warnings_box: warningsBox,
      wantedWarnings: warnings,
      rawWarnings: []
    )
  end

  defp createFileMenu() do
    fileMenu = :wxMenu.new()

    _ =
      :wxMenu.append(
        fileMenu,
        :wxMenuItem.new([{:id, 100}, {:text, 'Save &Warnings'}])
      )

    _ =
      :wxMenu.append(
        fileMenu,
        :wxMenuItem.new([{:id, 101}, {:text, 'Save &Log'}])
      )

    _ =
      :wxMenu.append(
        fileMenu,
        :wxMenuItem.new([{:id, 102}, {:text, 'E&xit\tAlt-X'}])
      )

    fileMenu
  end

  defp createWarningsMenu() do
    warningsMenu = :wxMenu.new()
    addCheckedItem(warningsMenu, 200, 'Match failures')
    addCheckedItem(warningsMenu, 201, 'Failing function calls')
    addCheckedItem(warningsMenu, 202, 'Bad fun applications')
    addCheckedItem(warningsMenu, 203, 'Opacity violations')
    addCheckedItem(warningsMenu, 204, 'Improper list constructions')
    addCheckedItem(warningsMenu, 205, 'Unused functions')
    _ = :wxMenu.appendCheckItem(warningsMenu, 206, 'Error handling functions')
    addCheckedItem(warningsMenu, 207, 'Functions of no return')
    addCheckedItem(warningsMenu, 208, 'Call to unexported function')
    _ = :wxMenu.appendCheckItem(warningsMenu, 209, 'Possible race conditions')
    addCheckedItem(warningsMenu, 210, 'Wrong contracts')
    addCheckedItem(warningsMenu, 211, 'Wrong contract syntax')
    warningsMenu
  end

  defp addCheckedItem(menu, itemId, str) do
    _ = :wxMenu.appendCheckItem(menu, itemId, str)
    :wxMenu.check(menu, itemId, true)
  end

  defp createPltMenu() do
    pltMenu = :wxMenu.new()
    _ = :wxMenu.appendCheckItem(pltMenu, 300, 'Init with empty PLT')

    _ =
      :wxMenu.append(
        pltMenu,
        :wxMenuItem.new([{:id, 301}, {:text, 'Show contents'}])
      )

    _ =
      :wxMenu.append(
        pltMenu,
        :wxMenuItem.new([{:id, 302}, {:text, 'Search contents'}])
      )

    pltMenu
  end

  defp createOptionsMenu() do
    optsMenu = :wxMenu.new()

    _ =
      :wxMenu.append(
        optsMenu,
        :wxMenuItem.new([{:id, 400}, {:text, 'Manage Macro Definitions'}])
      )

    _ =
      :wxMenu.append(
        optsMenu,
        :wxMenuItem.new([{:id, 401}, {:text, 'Manage Include Directories'}])
      )

    optsMenu
  end

  defp createHelpMenu() do
    helpMenu = :wxMenu.new()

    _ =
      :wxMenu.append(
        helpMenu,
        :wxMenuItem.new([{:id, 500}, {:text, 'Manual'}])
      )

    _ =
      :wxMenu.append(
        helpMenu,
        :wxMenuItem.new([{:id, 501}, {:text, 'Warning Options'}])
      )

    _ =
      :wxMenu.append(
        helpMenu,
        :wxMenuItem.new([{:id, 499}, {:text, 'About'}])
      )

    helpMenu
  end

  defp gui_loop(
         r_gui_state(
           backend_pid: backendPid,
           doc_plt: docPlt,
           log: log,
           frame: frame,
           warnings_box: warningsBox
         ) = state
       ) do
    receive do
      r_wx(event: r_wxClose()) ->
        :ok = :wxFrame.setStatusText(frame, 'Closing...', [])
        :wxWindow.destroy(frame)
        0

      r_wx(id: 101, obj: ^frame, event: r_wxCommand(type: :command_menu_selected)) ->
        save_file(state, :log)
        gui_loop(state)

      r_wx(id: 100, obj: ^frame, event: r_wxCommand(type: :command_menu_selected)) ->
        save_file(state, :warnings)
        gui_loop(state)

      r_wx(id: 102, obj: ^frame, event: r_wxCommand(type: :command_menu_selected)) ->
        case maybe_quit(state) do
          true ->
            0

          false ->
            gui_loop(state)
        end

      r_wx(id: 301, obj: ^frame, event: r_wxCommand(type: :command_menu_selected)) ->
        show_doc_plt(state)
        gui_loop(state)

      r_wx(id: 302, obj: ^frame, event: r_wxCommand(type: :command_menu_selected)) ->
        case :dialyzer_plt.get_specs(docPlt) do
          '' ->
            error_sms(state, 'No analysis has been made yet!\n')

          _ ->
            search_doc_plt(state)
        end

        gui_loop(state)

      r_wx(id: 401, obj: ^frame, event: r_wxCommand(type: :command_menu_selected)) ->
        newOptions = include_dialog(state)
        newState = r_gui_state(state, options: newOptions)
        gui_loop(newState)

      r_wx(id: 400, obj: ^frame, event: r_wxCommand(type: :command_menu_selected)) ->
        newOptions = macro_dialog(state)
        newState = r_gui_state(state, options: newOptions)
        gui_loop(newState)

      r_wx(id: 500, obj: ^frame, event: r_wxCommand(type: :command_menu_selected)) ->
        handle_help(state, 'Dialyzer Manual', 'manual.txt')
        gui_loop(state)

      r_wx(id: 501, obj: ^frame, event: r_wxCommand(type: :command_menu_selected)) ->
        handle_help(state, 'Dialyzer Warnings', 'warnings.txt')
        gui_loop(state)

      r_wx(id: 499, obj: ^frame, event: r_wxCommand(type: :command_menu_selected)) ->
        message =
          '\t       This is DIALYZER version ' ++
            :EFE_TODO_VSN_MACRO ++
            ' \n' ++
            'DIALYZER is a DIscrepancy AnaLYZer for ERlang programs.\n\n' ++
            '     Copyright (C) Tobias Lindahl <tobiasl@it.uu.se>\n' ++
            '                   Kostis Sagonas <kostis@it.uu.se>\n\n'

        output_sms(state, 'About Dialyzer', message, :info)
        gui_loop(state)

      r_wx(id: 517, event: r_wxCommand(type: :command_button_clicked)) ->
        state1 = handle_add_files(state)
        gui_loop(state1)

      r_wx(id: 532, event: r_wxCommand(type: :command_button_clicked)) ->
        state1 = handle_add_dir(state)
        gui_loop(state1)

      r_wx(id: 518, event: r_wxCommand(type: :command_button_clicked)) ->
        state1 = handle_add_rec(state)
        gui_loop(state1)

      r_wx(id: 514, event: r_wxCommand(type: :command_button_clicked)) ->
        state1 = handle_file_delete(state)
        gui_loop(state1)

      r_wx(id: 515, event: r_wxCommand(type: :command_button_clicked)) ->
        state1 = handle_file_delete_all(state)
        gui_loop(state1)

      r_wx(id: 516, event: r_wxCommand(type: :command_button_clicked)) ->
        :wxTextCtrl.clear(r_gui_state(state, :log))
        gui_loop(state)

      r_wx(id: 523, event: r_wxCommand(type: :command_button_clicked)) ->
        handle_explanation(state)
        gui_loop(state)

      r_wx(id: 519, event: r_wxCommand(type: :command_button_clicked)) ->
        :wxListBox.clear(warningsBox)
        newState = r_gui_state(state, rawWarnings: [])
        gui_loop(newState)

      r_wx(id: 520, event: r_wxCommand(type: :command_button_clicked)) ->
        newState = start_analysis(state)
        gui_loop(newState)

      r_wx(id: 522, event: r_wxCommand(type: :command_button_clicked)) ->
        send(backendPid, {self(), :stop})
        config_gui_stop(state)
        update_editor(log, '\n***** Analysis stopped ****\n')
        gui_loop(state)

      {^backendPid, :ext_calls, extCalls} ->
        msg =
          :io_lib.format(
            'The following functions are called but type information about them is not available.\nThe analysis might get more precise by including the modules containing these functions:\n\n\t~tp\n',
            [extCalls]
          )

        free_editor(state, 'Analysis Done', msg)
        gui_loop(state)

      {^backendPid, :ext_types, extTypes} ->
        map = fn {m, f, a} ->
          :io_lib.format('~tp:~tp/~p', [m, f, a])
        end

        extTypeString =
          :lists.join(
            '\n',
            :lists.map(map, extTypes)
          )

        msg =
          :io_lib.format(
            'The following remote types are being used but information about them is not available.\nThe analysis might get more precise by including the modules containing these types and making sure that they are exported:\n~ts\n',
            [extTypeString]
          )

        free_editor(state, 'Analysis done', msg)
        gui_loop(state)

      {^backendPid, :log, logMsg} ->
        update_editor(log, logMsg)
        gui_loop(state)

      {^backendPid, :warnings, warns} ->
        sortedWarns = :lists.keysort(2, warns)
        newState = add_warnings(state, sortedWarns)
        gui_loop(newState)

      {^backendPid, :cserver, cServer, plt} ->
        self = self()

        fun = fn ->
          :dialyzer_explanation.expl_loop(self, cServer, plt)
        end

        explanationPid = spawn_link(fun)
        gui_loop(r_gui_state(state, expl_pid: explanationPid))

      {^backendPid, :done, newPlt, newDocPlt} ->
        message(state, 'Analysis done')
        :dialyzer_plt.delete(newPlt)
        config_gui_stop(state)
        gui_loop(r_gui_state(state, doc_plt: newDocPlt))

      {:EXIT, ^backendPid, {:error, reason}} ->
        free_editor(state, 'Dialyzer Error', reason)
        config_gui_stop(state)
        gui_loop(state)

      {:EXIT, ^backendPid, reason} when reason !== :normal ->
        free_editor(state, 'Dialyzer Error', :io_lib.format('~tp', [reason]))
        config_gui_stop(state)
        gui_loop(state)
    end
  end

  defp maybe_quit(r_gui_state(frame: frame) = state) do
    case dialog(state, 'Do you really want to quit?', 'Dialyzer Message') do
      true ->
        :wxWindow.destroy(frame)
        true

      false ->
        false
    end
  end

  defp dialog(r_gui_state(frame: frame), message, title) do
    messageWin =
      :wxMessageDialog.new(frame, message, [{:caption, title}, {:style, 2 ||| 8 ||| 1024 ||| 128}])

    case :wxDialog.showModal(messageWin) do
      5103 ->
        true

      5104 ->
        false

      5101 ->
        false
    end
  end

  defp search_doc_plt(r_gui_state(gui: wx) = state) do
    dialog = :wxFrame.new(wx, 535, 'Search the PLT', [{:size, {400, 100}}, {:style, 32768}])
    size = {:size, {120, 30}}
    modLabel = :wxStaticText.new(dialog, 536, 'Module')
    modText = :wxTextCtrl.new(dialog, 539, [size])
    funLabel = :wxStaticText.new(dialog, 537, 'Function')
    funText = :wxTextCtrl.new(dialog, 540, [size])
    arLabel = :wxStaticText.new(dialog, 538, 'Arity')
    arText = :wxTextCtrl.new(dialog, 541, [size])
    searchButton = :wxButton.new(dialog, 542, [{:label, 'Search'}])
    :wxButton.connect(searchButton, :command_button_clicked)
    cancel = :wxButton.new(dialog, 543, [{:label, 'Cancel'}])
    :wxButton.connect(cancel, :command_button_clicked)
    layout = :wxBoxSizer.new(8)
    top = :wxBoxSizer.new(4)
    modLayout = :wxBoxSizer.new(8)
    funLayout = :wxBoxSizer.new(8)
    arLayout = :wxBoxSizer.new(8)
    buttons = :wxBoxSizer.new(4)
    _ = :wxSizer.add(modLayout, modLabel, [{:flag, 64 ||| 128 ||| 32 ||| 16}, {:border, 2}])
    _ = :wxSizer.add(modLayout, modText, [{:flag, 64 ||| 128 ||| 32 ||| 16}, {:border, 2}])
    _ = :wxSizer.add(funLayout, funLabel, [{:flag, 64 ||| 128 ||| 32 ||| 16}, {:border, 2}])
    _ = :wxSizer.add(funLayout, funText, [{:flag, 64 ||| 128 ||| 32 ||| 16}, {:border, 2}])
    _ = :wxSizer.add(arLayout, arLabel, [{:flag, 64 ||| 128 ||| 32 ||| 16}, {:border, 2}])
    _ = :wxSizer.add(arLayout, arText, [{:flag, 64 ||| 128 ||| 32 ||| 16}, {:border, 2}])
    _ = :wxSizer.add(buttons, searchButton, [{:flag, 64 ||| 128 ||| 32 ||| 16}, {:border, 2}])
    _ = :wxSizer.add(buttons, cancel, [{:flag, 64 ||| 128 ||| 32 ||| 16}, {:border, 2}])
    _ = :wxSizer.add(top, modLayout)
    _ = :wxSizer.add(top, funLayout)
    _ = :wxSizer.add(top, arLayout)
    _ = :wxSizer.add(layout, top, [{:flag, 256 ||| 2048}])
    _ = :wxSizer.add(layout, buttons, [{:flag, 256 ||| 2048 ||| 128}])
    :wxFrame.connect(dialog, :close_window)
    :wxWindow.setSizer(dialog, layout)
    :wxFrame.show(dialog)
    search_plt_loop(state, dialog, modText, funText, arText, searchButton, cancel)
  end

  defp search_plt_loop(
         state = r_gui_state(doc_plt: docPlt, frame: frame),
         win,
         modText,
         funText,
         arText,
         search,
         cancel
       ) do
    receive do
      r_wx(id: 543, event: r_wxCommand(type: :command_button_clicked)) ->
        :wxWindow.destroy(win)

      r_wx(id: 535, event: r_wxClose(type: :close_window)) ->
        :wxWindow.destroy(win)

      r_wx(event: r_wxClose(type: :close_window)) ->
        :wxWindow.destroy(win)
        :wxWindow.destroy(frame)

      r_wx(id: 542, event: r_wxCommand(type: :command_button_clicked)) ->
        m = format_search(:wxTextCtrl.getValue(modText))
        f = format_search(:wxTextCtrl.getValue(funText))
        a = format_search(:wxTextCtrl.getValue(arText))

        cond do
          m === :_ or f === :_ or a === :_ ->
            error_sms(state, 'Please give:\n Module (atom)\n Function (atom)\n Arity (integer)\n')
            search_plt_loop(state, win, modText, funText, arText, search, cancel)

          true ->
            case :dialyzer_plt.get_specs(docPlt, m, f, a) do
              :none ->
                error_sms(state, 'No such function')
                search_plt_loop(state, win, modText, funText, arText, search, cancel)

              nonEmptyString ->
                :wxWindow.destroy(win)
                free_editor(state, 'Content of PLT', nonEmptyString)
            end
        end
    end
  end

  defp format_search([]) do
    :_
  end

  defp format_search(string) do
    try do
      :erlang.list_to_integer(string)
    catch
      :error, _ ->
        :erlang.list_to_atom(string)
    end
  end

  defp show_doc_plt(r_gui_state(doc_plt: docPLT) = state) do
    case :dialyzer_plt.get_specs(docPLT) do
      '' ->
        error_sms(state, 'No analysis has been made yet!\n')

      nonEmptyString ->
        free_editor(state, 'Content of PLT', nonEmptyString)
    end
  end

  defp message(state, message) do
    output_sms(state, 'Dialyzer Message', message, :info)
  end

  defp error_sms(state, message) do
    output_sms(state, 'Dialyzer Error', message, :error)
  end

  defp output_sms(r_gui_state(frame: frame), title, message, type) do
    style =
      case type do
        :error ->
          4 ||| 512

        :info ->
          4 ||| 2048
      end

    options = [{:caption, title}, {:style, style}]
    messageWin = :wxMessageDialog.new(frame, message, options)
    :wxWindow.setSizeHints(messageWin, {350, 100})
    :wxDialog.showModal(messageWin)
    :ok
  end

  defp free_editor(r_gui_state(gui: wx, frame: frame), title, contents0) do
    contents = :lists.flatten(contents0)
    tokens = :string.lexemes(contents, '\n')
    nofLines = length(tokens)

    longestLine =
      :lists.max(
        for x <- tokens do
          length(x)
        end
      )

    height0 = nofLines * 25 + 80

    height =
      cond do
        height0 > 500 ->
          500

        true ->
          height0
      end

    width0 = longestLine * 7 + 60

    width =
      cond do
        width0 > 800 ->
          800

        true ->
          width0
      end

    size = {:size, {width, height}}
    win = :wxFrame.new(wx, 534, title, [{:size, {width + 4, height + 50}}])
    editor = :wxTextCtrl.new(win, 530, [size, {:style, 32 ||| 16 ||| 2_147_483_648 ||| 8192}])
    :wxTextCtrl.appendText(editor, contents)
    :wxFrame.connect(win, :close_window)
    ok = :wxButton.new(win, 531, [{:label, 'OK'}])
    :wxButton.connect(ok, :command_button_clicked)
    layout = :wxBoxSizer.new(8)
    _ = :wxSizer.add(layout, editor, [{:flag, 64 ||| 128 ||| 32 ||| 16}, {:border, 2}])
    flag = 256 ||| 2048 ||| 128 ||| (64 ||| 128 ||| 32 ||| 16)
    _ = :wxSizer.add(layout, ok, [{:flag, flag}, {:border, 2}])
    :wxWindow.setSizer(win, layout)
    :wxWindow.show(win)
    show_info_loop(frame, win)
  end

  defp show_info_loop(frame, win) do
    receive do
      r_wx(id: 531, event: r_wxCommand(type: :command_button_clicked)) ->
        :wxWindow.destroy(win)

      r_wx(id: 534, event: r_wxClose(type: :close_window)) ->
        :wxWindow.destroy(win)

      r_wx(event: r_wxClose(type: :close_window)) ->
        :wxWindow.destroy(frame)
    end
  end

  defp handle_add_files(
         r_gui_state(
           chosen_box: chosenBox,
           file_box: fileBox,
           files_to_analyze: fileList,
           mode: mode
         ) = state
       ) do
    case :wxFilePickerCtrl.getPath(fileBox) do
      '' ->
        state

      file ->
        newFile = :ordsets.new()
        newFile1 = :ordsets.add_element(file, newFile)

        ext =
          case :wxRadioBox.getSelection(mode) do
            0 ->
              '.beam'

            1 ->
              '.erl'
          end

        r_gui_state(state,
          files_to_analyze:
            add_files(
              filter_mods(
                newFile1,
                ext
              ),
              fileList,
              chosenBox,
              ext
            )
        )
    end
  end

  defp handle_add_dir(
         r_gui_state(
           chosen_box: chosenBox,
           dir_entry: dirBox,
           files_to_analyze: fileList,
           mode: mode
         ) = state
       ) do
    case :wxDirPickerCtrl.getPath(dirBox) do
      '' ->
        state

      dir ->
        newDir = :ordsets.new()
        newDir1 = :ordsets.add_element(dir, newDir)

        ext =
          case :wxRadioBox.getSelection(mode) do
            0 ->
              '.beam'

            1 ->
              '.erl'
          end

        r_gui_state(state,
          files_to_analyze:
            add_files(
              filter_mods(
                newDir1,
                ext
              ),
              fileList,
              chosenBox,
              ext
            )
        )
    end
  end

  defp handle_add_rec(
         r_gui_state(
           chosen_box: chosenBox,
           dir_entry: dirBox,
           files_to_analyze: fileList,
           mode: mode
         ) = state
       ) do
    case :wxDirPickerCtrl.getPath(dirBox) do
      '' ->
        state

      dir ->
        newDir = :ordsets.new()
        newDir1 = :ordsets.add_element(dir, newDir)

        targetDirs =
          :ordsets.union(
            newDir1,
            all_subdirs(newDir1)
          )

        ext =
          case :wxRadioBox.getSelection(mode) do
            0 ->
              '.beam'

            1 ->
              '.erl'
          end

        r_gui_state(state,
          files_to_analyze:
            add_files(
              filter_mods(
                targetDirs,
                ext
              ),
              fileList,
              chosenBox,
              ext
            )
        )
    end
  end

  defp handle_file_delete(
         r_gui_state(
           chosen_box: chosenBox,
           files_to_analyze: fileList
         ) = state
       ) do
    {_, list} = :wxListBox.getSelections(chosenBox)

    set =
      :ordsets.from_list(
        for x <- list do
          :wxControlWithItems.getString(chosenBox, x)
        end
      )

    fileList1 = :ordsets.subtract(fileList, set)

    :lists.foreach(
      fn x ->
        :wxListBox.delete(chosenBox, x)
      end,
      list
    )

    r_gui_state(state, files_to_analyze: fileList1)
  end

  defp handle_file_delete_all(r_gui_state(chosen_box: chosenBox) = state) do
    :wxListBox.clear(chosenBox)
    r_gui_state(state, files_to_analyze: :ordsets.new())
  end

  defp add_files(file, fileList, chosenBox, ext) do
    set = filter_mods(fileList, ext)
    files = :ordsets.union(file, set)
    files1 = :ordsets.to_list(files)
    :wxListBox.set(chosenBox, files1)
    files
  end

  defp filter_mods(mods, extension) do
    fun = fn x ->
      :filename.extension(x) === extension or
        (:filelib.is_dir(x) and
           contains_files(
             x,
             extension
           ))
    end

    :ordsets.filter(fun, mods)
  end

  defp contains_files(dir, extension) do
    {:ok, files} = :file.list_dir(dir)

    :lists.any(
      fn x ->
        :filename.extension(x) === extension
      end,
      files
    )
  end

  defp all_subdirs(dirs) do
    all_subdirs(dirs, [])
  end

  defp all_subdirs([dir | t], acc) do
    {:ok, files} = :file.list_dir(dir)

    subDirs =
      :lists.zf(
        fn f ->
          subDir = :filename.join(dir, f)

          case :filelib.is_dir(subDir) do
            true ->
              {true, subDir}

            false ->
              false
          end
        end,
        files
      )

    newAcc =
      :ordsets.union(
        :ordsets.from_list(subDirs),
        acc
      )

    all_subdirs(t ++ subDirs, newAcc)
  end

  defp all_subdirs([], acc) do
    acc
  end

  defp start_analysis(state) do
    analysis = build_analysis_record(state)

    case get_anal_files(
           state,
           r_analysis(analysis, :start_from)
         ) do
      :error ->
        msg = 'You must choose one or more files or dirs\nbefore starting the analysis!'
        error_sms(state, msg)
        config_gui_stop(state)
        state

      {:ok, files} ->
        msg = '\n========== Starting Analysis ==========\n\n'
        update_editor(r_gui_state(state, :log), msg)
        newAnalysis = r_analysis(analysis, files: files)
        run_analysis(state, newAnalysis)
    end
  end

  defp build_analysis_record(
         r_gui_state(mode: mode, menu: menu, options: options, init_plt: initPlt0)
       ) do
    startFrom =
      case :wxRadioBox.getSelection(mode) do
        0 ->
          :byte_code

        1 ->
          :src_code
      end

    initPlt =
      case :wxMenu.isChecked(
             r_menu(menu, :plt),
             300
           ) do
        true ->
          :dialyzer_plt.new()

        false ->
          initPlt0
      end

    r_analysis(
      defines: r_options(options, :defines),
      include_dirs: r_options(options, :include_dirs),
      plt: initPlt,
      start_from: startFrom,
      solvers: r_options(options, :solvers)
    )
  end

  defp get_anal_files(r_gui_state(files_to_analyze: files), startFrom) do
    filteredMods =
      case startFrom do
        :src_code ->
          filter_mods(files, '.erl')

        :byte_code ->
          filter_mods(files, '.beam')
      end

    filteredDirs =
      for x <- files, :filelib.is_dir(x) do
        x
      end

    case :ordsets.union(filteredMods, filteredDirs) do
      [] ->
        :error

      set ->
        {:ok, set}
    end
  end

  defp run_analysis(state, analysis) do
    config_gui_start(state)
    self = self()
    newAnalysis = r_analysis(analysis, doc_plt: :dialyzer_plt.new())
    legalWarnings = find_legal_warnings(state)

    fun = fn ->
      :dialyzer_analysis_callgraph.start(self, legalWarnings, newAnalysis)
    end

    backendPid = spawn_link(fun)
    r_gui_state(state, backend_pid: backendPid)
  end

  defp find_legal_warnings(
         r_gui_state(
           menu: r_menu(warnings: menuWarnings),
           wantedWarnings: warnings
         )
       ) do
    :ordsets.from_list(
      for {tag, menuItem} <- warnings,
          :wxMenu.isChecked(menuWarnings, menuItem) do
        tag
      end
    )
  end

  defp update_editor(editor, msg) do
    :wxTextCtrl.appendText(editor, msg)
  end

  defp config_gui_stop(state) do
    :wxWindow.disable(r_gui_state(state, :stop))
    :wxWindow.enable(r_gui_state(state, :run))
    :wxWindow.enable(r_gui_state(state, :del_file))
    :wxWindow.enable(r_gui_state(state, :clear_chosen))
    :wxWindow.enable(r_gui_state(state, :add))
    :wxWindow.enable(r_gui_state(state, :add_dir))
    :wxWindow.enable(r_gui_state(state, :add_rec))
    :wxWindow.enable(r_gui_state(state, :clear_warn))
    :wxWindow.enable(r_gui_state(state, :clear_log))
    menu = r_gui_state(state, :menu)
    :wxMenu.enable(r_menu(menu, :file), 100, true)
    :wxMenu.enable(r_menu(menu, :file), 101, true)
    :wxMenu.enable(r_menu(menu, :options), 400, true)
    :wxMenu.enable(r_menu(menu, :options), 401, true)
    :wxMenu.enable(r_menu(menu, :plt), 300, true)
    :wxMenu.enable(r_menu(menu, :plt), 301, true)
    :wxMenu.enable(r_menu(menu, :plt), 302, true)
    :wxRadioBox.enable(r_gui_state(state, :mode))
  end

  defp config_gui_start(state) do
    :wxWindow.enable(r_gui_state(state, :stop))
    :wxWindow.disable(r_gui_state(state, :run))
    :wxWindow.disable(r_gui_state(state, :del_file))
    :wxWindow.disable(r_gui_state(state, :clear_chosen))
    :wxWindow.disable(r_gui_state(state, :add))
    :wxWindow.disable(r_gui_state(state, :add_dir))
    :wxWindow.disable(r_gui_state(state, :add_rec))
    :wxWindow.disable(r_gui_state(state, :clear_warn))
    :wxWindow.disable(r_gui_state(state, :clear_log))
    menu = r_gui_state(state, :menu)
    :wxMenu.enable(r_menu(menu, :file), 100, false)
    :wxMenu.enable(r_menu(menu, :file), 101, false)
    :wxMenu.enable(r_menu(menu, :options), 400, false)
    :wxMenu.enable(r_menu(menu, :options), 401, false)
    :wxMenu.enable(r_menu(menu, :plt), 300, false)
    :wxMenu.enable(r_menu(menu, :plt), 301, false)
    :wxMenu.enable(r_menu(menu, :plt), 302, false)
    :wxRadioBox.disable(r_gui_state(state, :mode))
  end

  defp save_file(
         r_gui_state(frame: frame, warnings_box: wBox, log: log) = state,
         type
       ) do
    {message, box} =
      case type do
        :warnings ->
          {'Save Warnings', wBox}

        :log ->
          {'Save Log', log}
      end

    case :wxTextCtrl.getValue(box) do
      '' ->
        error_sms(state, 'There is nothing to save...\n')

      _ ->
        defaultPath = :code.root_dir()

        fileDialog =
          :wxFileDialog.new(
            frame,
            [{:defaultDir, defaultPath}, {:message, message}, {:style, 2 ||| 4}]
          )

        case :wxFileDialog.showModal(fileDialog) do
          5100 ->
            path = :wxFileDialog.getPath(fileDialog)

            case :wxTextCtrl.saveFile(box, [{:file, path}]) do
              true ->
                :ok

              false ->
                error_sms(state, 'Could not write to file:\n' ++ path)
            end

          5101 ->
            :wxWindow.destroy(fileDialog)

          _ ->
            error_sms(state, 'Could not write to file:\n')
        end
    end
  end

  defp include_dialog(r_gui_state(gui: wx, frame: frame, options: options)) do
    size = {:size, {300, 480}}
    dialog = :wxFrame.new(wx, 544, 'Include Directories', [size])
    dirLabel = :wxStaticText.new(dialog, 545, 'Directory: ')
    defaultPath = :code.root_dir()

    dirPicker =
      :wxDirPickerCtrl.new(dialog, 546, [
        {:path, defaultPath},
        {:message, 'Choose Directory to Include'},
        {:style, 8 ||| 2}
      ])

    box =
      :wxListBox.new(dialog, 547, [{:size, {200, 300}}, {:style, 128 ||| 1_073_741_824 ||| 512}])

    addButton = :wxButton.new(dialog, 548, [{:label, 'Add'}])
    deleteButton = :wxButton.new(dialog, 549, [{:label, 'Delete'}])
    deleteAllButton = :wxButton.new(dialog, 550, [{:label, 'Delete All'}])
    ok = :wxButton.new(dialog, 551, [{:label, 'OK'}])
    cancel = :wxButton.new(dialog, 552, [{:label, 'Cancel'}])
    :wxButton.connect(addButton, :command_button_clicked)
    :wxButton.connect(deleteButton, :command_button_clicked)

    :wxButton.connect(
      deleteAllButton,
      :command_button_clicked
    )

    :wxButton.connect(ok, :command_button_clicked)
    :wxButton.connect(cancel, :command_button_clicked)

    dirs =
      for x <- r_options(options, :include_dirs) do
        :io_lib.format('~ts', [x])
      end

    :wxListBox.set(box, dirs)
    layout = :wxBoxSizer.new(8)
    buttons = :wxBoxSizer.new(4)
    buttons1 = :wxBoxSizer.new(4)
    _ = :wxSizer.add(layout, dirLabel, [{:flag, 256}])
    _ = :wxSizer.add(layout, dirPicker, [{:flag, 256}])

    _ =
      :wxSizer.add(layout, addButton, [{:flag, 256 ||| (64 ||| 128 ||| 32 ||| 16)}, {:border, 2}])

    _ = :wxSizer.add(layout, box, [{:flag, 256 ||| (64 ||| 128 ||| 32 ||| 16)}, {:border, 2}])
    _ = :wxSizer.add(buttons, deleteButton, [{:flag, 64 ||| 128 ||| 32 ||| 16}, {:border, 2}])
    _ = :wxSizer.add(buttons, deleteAllButton, [{:flag, 64 ||| 128 ||| 32 ||| 16}, {:border, 2}])
    _ = :wxSizer.add(layout, buttons, [{:flag, 256}])
    _ = :wxSizer.add(buttons1, ok, [{:flag, 64 ||| 128 ||| 32 ||| 16}, {:border, 2}])
    _ = :wxSizer.add(buttons1, cancel, [{:flag, 64 ||| 128 ||| 32 ||| 16}, {:border, 2}])
    _ = :wxSizer.add(layout, buttons1, [{:flag, 512 ||| 128}])
    :wxFrame.connect(dialog, :close_window)
    :wxWindow.setSizer(dialog, layout)
    :wxFrame.show(dialog)
    include_loop(options, dialog, box, dirPicker, frame)
  end

  defp include_loop(options, win, box, dirPicker, frame) do
    receive do
      r_wx(id: 552, event: r_wxCommand(type: :command_button_clicked)) ->
        :wxWindow.destroy(win)
        options

      r_wx(id: 544, event: r_wxClose(type: :close_window)) ->
        :wxWindow.destroy(win)
        options

      r_wx(event: r_wxClose(type: :close_window)) ->
        :wxWindow.destroy(win)
        :wxWindow.destroy(frame)

      r_wx(id: 551, event: r_wxCommand(type: :command_button_clicked)) ->
        :wxWindow.destroy(win)
        options

      r_wx(id: 548, event: r_wxCommand(type: :command_button_clicked)) ->
        dirs = r_options(options, :include_dirs)

        newDirs =
          case :wxDirPickerCtrl.getPath(dirPicker) do
            '' ->
              dirs

            add ->
              [add | dirs]
          end

        newOptions = r_options(options, include_dirs: newDirs)
        :wxListBox.set(box, newDirs)
        include_loop(newOptions, win, box, dirPicker, frame)

      r_wx(id: 549, event: r_wxCommand(type: :command_button_clicked)) ->
        newOptions =
          case :wxListBox.getSelections(box) do
            {0, _} ->
              options

            {_, list} ->
              delList =
                for x <- list do
                  :wxControlWithItems.getString(box, x)
                end

              newDirs = r_options(options, :include_dirs) -- delList

              :lists.foreach(
                fn x ->
                  :wxListBox.delete(box, x)
                end,
                list
              )

              r_options(options, include_dirs: newDirs)
          end

        include_loop(newOptions, win, box, dirPicker, frame)

      r_wx(id: 550, event: r_wxCommand(type: :command_button_clicked)) ->
        :wxListBox.clear(box)
        newOptions = r_options(options, include_dirs: [])
        include_loop(newOptions, win, box, dirPicker, frame)
    end
  end

  defp macro_dialog(r_gui_state(gui: wx, frame: frame, options: options)) do
    size = {:size, {300, 480}}
    size1 = {:size, {120, 30}}
    dialog = :wxFrame.new(wx, 553, 'Macro Definitions', [size])
    macroLabel = :wxStaticText.new(dialog, 554, 'Macro')
    termLabel = :wxStaticText.new(dialog, 556, 'Term')
    macroText = :wxTextCtrl.new(dialog, 555, [size1])
    termText = :wxTextCtrl.new(dialog, 557, [size1])

    box =
      :wxListBox.new(dialog, 558, [{:size, {250, 300}}, {:style, 128 ||| 1_073_741_824 ||| 512}])

    addButton = :wxButton.new(dialog, 559, [{:label, 'Add'}])
    deleteButton = :wxButton.new(dialog, 560, [{:label, 'Delete'}])
    deleteAllButton = :wxButton.new(dialog, 561, [{:label, 'Delete All'}])
    ok = :wxButton.new(dialog, 562, [{:label, 'OK'}])
    cancel = :wxButton.new(dialog, 563, [{:label, 'Cancel'}])
    :wxButton.connect(addButton, :command_button_clicked)
    :wxButton.connect(deleteButton, :command_button_clicked)

    :wxButton.connect(
      deleteAllButton,
      :command_button_clicked
    )

    :wxButton.connect(ok, :command_button_clicked)
    :wxButton.connect(cancel, :command_button_clicked)

    macros =
      for {x, y} <- r_options(options, :defines) do
        :io_lib.format('~p = ~p', [x, y])
      end

    :wxListBox.set(box, macros)
    layout = :wxBoxSizer.new(8)
    item = :wxBoxSizer.new(4)
    macroItem = :wxBoxSizer.new(8)
    termItem = :wxBoxSizer.new(8)
    buttons = :wxBoxSizer.new(4)
    buttons1 = :wxBoxSizer.new(4)
    _ = :wxSizer.add(macroItem, macroLabel, [{:flag, 64 ||| 128 ||| 32 ||| 16}, {:border, 2}])
    _ = :wxSizer.add(macroItem, macroText, [{:flag, 64 ||| 128 ||| 32 ||| 16}, {:border, 2}])
    _ = :wxSizer.add(termItem, termLabel, [{:flag, 64 ||| 128 ||| 32 ||| 16}, {:border, 2}])
    _ = :wxSizer.add(termItem, termText, [{:flag, 64 ||| 128 ||| 32 ||| 16}, {:border, 2}])
    _ = :wxSizer.add(item, macroItem)
    _ = :wxSizer.add(item, termItem)
    _ = :wxSizer.add(layout, item, [{:flag, 256}])

    _ =
      :wxSizer.add(layout, addButton, [{:flag, 256 ||| (64 ||| 128 ||| 32 ||| 16)}, {:border, 2}])

    _ = :wxSizer.add(layout, box, [{:flag, 256 ||| (64 ||| 128 ||| 32 ||| 16)}, {:border, 2}])
    _ = :wxSizer.add(buttons, deleteButton, [{:flag, 64 ||| 128 ||| 32 ||| 16}, {:border, 2}])
    _ = :wxSizer.add(buttons, deleteAllButton, [{:flag, 64 ||| 128 ||| 32 ||| 16}, {:border, 2}])
    _ = :wxSizer.add(layout, buttons, [{:flag, 256}])
    _ = :wxSizer.add(buttons1, ok, [{:flag, 64 ||| 128 ||| 32 ||| 16}, {:border, 2}])
    _ = :wxSizer.add(buttons1, cancel, [{:flag, 64 ||| 128 ||| 32 ||| 16}, {:border, 2}])
    _ = :wxSizer.add(layout, buttons1, [{:flag, 512 ||| 128}])
    :wxFrame.connect(dialog, :close_window)
    :wxWindow.setSizer(dialog, layout)
    :wxFrame.show(dialog)
    macro_loop(options, dialog, box, macroText, termText, frame)
  end

  defp macro_loop(options, win, box, macroText, termText, frame) do
    receive do
      r_wx(id: 563, event: r_wxCommand(type: :command_button_clicked)) ->
        :wxWindow.destroy(win)
        options

      r_wx(id: 553, event: r_wxClose(type: :close_window)) ->
        :wxWindow.destroy(win)
        options

      r_wx(event: r_wxClose(type: :close_window)) ->
        :wxWindow.destroy(win)
        :wxWindow.destroy(frame)

      r_wx(id: 562, event: r_wxCommand(type: :command_button_clicked)) ->
        :wxWindow.destroy(win)
        options

      r_wx(id: 559, event: r_wxCommand(type: :command_button_clicked)) ->
        defines = r_options(options, :defines)

        newDefines =
          case :wxTextCtrl.getValue(macroText) do
            '' ->
              defines

            macro ->
              case :wxTextCtrl.getValue(termText) do
                '' ->
                  :orddict.store(:erlang.list_to_atom(macro), true, defines)

                string ->
                  :orddict.store(:erlang.list_to_atom(macro), string, defines)
              end
          end

        newOptions = r_options(options, defines: newDefines)

        newEntries =
          for {x, y} <- newDefines do
            :io_lib.format('~p = ~p', [x, y])
          end

        :wxListBox.set(box, newEntries)
        macro_loop(newOptions, win, box, macroText, termText, frame)

      r_wx(id: 560, event: r_wxCommand(type: :command_button_clicked)) ->
        newOptions =
          case :wxListBox.getSelections(box) do
            {0, _} ->
              options

            {_, list} ->
              fun = fn x ->
                val = :wxControlWithItems.getString(box, x)
                [macroName | _] = :re.split(val, ' ', [{:return, :list}, :unicode])
                :erlang.list_to_atom(macroName)
              end

              delete =
                for x <- list do
                  fun.(x)
                end

              :lists.foreach(
                fn x ->
                  :wxListBox.delete(box, x)
                end,
                list
              )

              defines = r_options(options, :defines)

              newDefines =
                :lists.foldl(
                  fn x, acc ->
                    :orddict.erase(x, acc)
                  end,
                  defines,
                  delete
                )

              r_options(options, defines: newDefines)
          end

        macro_loop(newOptions, win, box, macroText, termText, frame)

      r_wx(id: 561, event: r_wxCommand(type: :command_button_clicked)) ->
        :wxListBox.clear(box)
        newOptions = r_options(options, defines: [])
        macro_loop(newOptions, win, box, macroText, termText, frame)
    end
  end

  defp handle_help(state, title, txt) do
    fileName = :filename.join([:code.lib_dir(:dialyzer), 'doc', txt])

    case :file.open(fileName, [:read]) do
      {:error, reason} ->
        error_sms(state, :io_lib.format('Could not find doc/~ts file!\n\n ~tp', [txt, reason]))

      {:ok, _Handle} ->
        case :file.read_file(fileName) do
          {:error, reason} ->
            error_sms(
              state,
              :io_lib.format('Could not read doc/~ts file!\n\n ~tp', [txt, reason])
            )

          {:ok, binary} ->
            contents = :erlang.binary_to_list(binary)
            free_editor(state, title, contents)
        end
    end
  end

  defp add_warnings(
         r_gui_state(
           warnings_box: warnBox,
           rawWarnings: rawWarns
         ) = state,
         warnings
       ) do
    newRawWarns = rawWarns ++ warnings

    warnList =
      for w <- newRawWarns do
        :string.trim(:dialyzer.format_warning(w), :trailing)
      end

    :wxListBox.set(warnBox, warnList)
    r_gui_state(state, rawWarnings: newRawWarns)
  end

  defp handle_explanation(
         r_gui_state(rawWarnings: rawWarns, warnings_box: warnBox, expl_pid: explPid) = state
       ) do
    case :wxListBox.isEmpty(warnBox) do
      true ->
        error_sms(state, '\nThere are no warnings.\nRun the dialyzer first.')

      false ->
        case :wxListBox.getSelections(warnBox) do
          {0, []} ->
            error_sms(state, '\nYou must choose a warning to be explained\n')

          {_, [warnNumber]} ->
            warn = :lists.nth(warnNumber + 1, rawWarns)
            self = self()
            send(explPid, {self, :warning, warn})
            explanation_loop(state)
        end
    end
  end

  defp explanation_loop(r_gui_state(expl_pid: explPid) = state) do
    receive do
      {^explPid, :explanation, explanation} ->
        show_explanation(state, explanation)

      _ ->
        :io.format('Unknown message\n')
        explanation_loop(state)
    end
  end

  defp show_explanation(r_gui_state(gui: wx) = state, explanation) do
    case explanation do
      :none ->
        output_sms(
          state,
          'Dialyzer Message',
          'There is not any explanation for this error!\n',
          :info
        )

      expl ->
        explString = format_explanation(expl)
        size = {:size, {700, 300}}
        win = :wxFrame.new(wx, 564, 'Dialyzer Explanation', [{:size, {740, 350}}])
        editor = :wxTextCtrl.new(win, 565, [size, {:style, 32 ||| 16 ||| 2_147_483_648 ||| 8192}])
        :wxTextCtrl.appendText(editor, explString)
        :wxFrame.connect(win, :close_window)
        explButton = :wxButton.new(win, 566, [{:label, 'Further Explain'}])
        :wxButton.connect(explButton, :command_button_clicked)
        ok = :wxButton.new(win, 567, [{:label, 'OK'}])
        :wxButton.connect(ok, :command_button_clicked)
        layout = :wxBoxSizer.new(8)
        buttons = :wxBoxSizer.new(4)
        _ = :wxSizer.add(buttons, explButton, [{:flag, 64 ||| 128 ||| 32 ||| 16}, {:border, 2}])
        _ = :wxSizer.add(buttons, ok, [{:flag, 64 ||| 128 ||| 32 ||| 16}, {:border, 2}])

        _ =
          :wxSizer.add(layout, editor, [{:flag, 256 ||| (64 ||| 128 ||| 32 ||| 16)}, {:border, 2}])

        _ = :wxSizer.add(layout, buttons, [{:flag, 256}])
        :wxWindow.setSizer(win, layout)
        :wxWindow.show(win)
        newState = r_gui_state(state, explanation_box: editor)
        show_explanation_loop(newState, win, explanation)
    end
  end

  defp show_explanation_loop(
         r_gui_state(frame: frame, expl_pid: explPid) = state,
         win,
         explanation
       ) do
    receive do
      {^explPid, :none, _} ->
        output_sms(
          state,
          'Dialyzer Message',
          'There is not any other explanation for this error!\n',
          :info
        )

        show_explanation_loop(state, win, explanation)

      {^explPid, :further, newExplanation} ->
        update_explanation(state, newExplanation)
        show_explanation_loop(state, win, newExplanation)

      r_wx(id: 566, event: r_wxCommand(type: :command_button_clicked)) ->
        send(explPid, {self(), :further, explanation})
        show_explanation_loop(state, win, explanation)

      r_wx(id: 567, event: r_wxCommand(type: :command_button_clicked)) ->
        :wxWindow.destroy(win)

      r_wx(id: 564, event: r_wxClose(type: :close_window)) ->
        :wxWindow.destroy(win)

      r_wx(event: r_wxClose(type: :close_window)) ->
        :wxWindow.destroy(frame)
    end
  end

  defp update_explanation(r_gui_state(explanation_box: box), explanation) do
    explString = format_explanation(explanation)
    :wxTextCtrl.appendText(box, '\n --------------------------- \n')
    :wxTextCtrl.appendText(box, explString)
  end

  defp format_explanation({:function_return, {m, f, a}, newList}) do
    :io_lib.format(
      'The function ~w:~tw/~w returns ~ts\n',
      [m, f, a, :erl_types.t_to_string(newList)]
    )
  end

  defp format_explanation(explanation) do
    :io_lib.format('~p\n', [explanation])
  end
end
