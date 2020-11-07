defmodule :m_reltool_app_win do
  use Bitwise
  import Kernel, except: [raise: 1]
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

  Record.defrecord(:r_common, :common,
    sys_debug: :undefined,
    wx_debug: :undefined,
    trap_exit: :undefined
  )

  Record.defrecord(:r_mod, :mod,
    name: :undefined,
    app_name: :undefined,
    incl_cond: :undefined,
    debug_info: :undefined,
    is_app_mod: :undefined,
    is_ebin_mod: :undefined,
    uses_mods: :undefined,
    exists: :undefined,
    status: :ok,
    used_by_mods: [],
    is_pre_included: :undefined,
    is_included: :undefined
  )

  Record.defrecord(:r_app_info, :app_info,
    description: '',
    id: '',
    vsn: '',
    modules: [],
    maxP: :infinity,
    maxT: :infinity,
    registered: [],
    incl_apps: [],
    applications: [],
    env: [],
    mod: :undefined,
    start_phases: :undefined,
    runtime_dependencies: []
  )

  Record.defrecord(:r_regexp, :regexp,
    source: :undefined,
    compiled: :undefined
  )

  Record.defrecord(:r_app, :app,
    name: :undefined,
    is_escript: :undefined,
    use_selected_vsn: :undefined,
    active_dir: :undefined,
    sorted_dirs: :undefined,
    vsn: :undefined,
    label: :undefined,
    info: :undefined,
    mods: :undefined,
    mod_cond: :undefined,
    incl_cond: :undefined,
    debug_info: :undefined,
    app_file: :undefined,
    app_type: :undefined,
    incl_app_filters: :undefined,
    excl_app_filters: :undefined,
    incl_archive_filters: :undefined,
    excl_archive_filters: :undefined,
    archive_opts: :undefined,
    status: :undefined,
    uses_mods: :undefined,
    used_by_mods: :undefined,
    uses_apps: :undefined,
    used_by_apps: :undefined,
    is_pre_included: :undefined,
    is_included: :undefined,
    rels: :undefined
  )

  Record.defrecord(:r_rel_app, :rel_app,
    name: :undefined,
    app_type: :undefined,
    incl_apps: :undefined
  )

  Record.defrecord(:r_rel, :rel,
    name: :undefined,
    vsn: :undefined,
    rel_apps: :undefined,
    load_dot_erlang: true
  )

  Record.defrecord(:r_sys, :sys,
    root_dir: :undefined,
    lib_dirs: :undefined,
    escripts: :undefined,
    mod_cond: :undefined,
    incl_cond: :undefined,
    apps: :undefined,
    boot_rel: :undefined,
    rels: :undefined,
    emu_name: :undefined,
    profile: :undefined,
    excl_lib: :undefined,
    incl_sys_filters: :undefined,
    excl_sys_filters: :undefined,
    incl_app_filters: :undefined,
    excl_app_filters: :undefined,
    incl_archive_filters: :undefined,
    excl_archive_filters: :undefined,
    archive_opts: :undefined,
    relocatable: :undefined,
    rel_app_type: :undefined,
    embedded_app_type: :undefined,
    app_file: :undefined,
    debug_info: :undefined
  )

  Record.defrecord(:r_state, :state,
    parent_pid: :undefined,
    xref_pid: :undefined,
    mod_wins: :undefined,
    sys: :undefined,
    common: :undefined,
    app: :undefined,
    frame: :undefined,
    panel: :undefined,
    book: :undefined,
    status_bar: :undefined,
    config_app_global: :undefined,
    config_app_local: :undefined,
    config_app_local_box: :undefined,
    config_mod_global: :undefined,
    config_mod_local: :undefined,
    config_mod_local_box: :undefined,
    config_latest: :undefined,
    config_selected: :undefined,
    config_source_box: :undefined,
    app_used_by_ctrl: :undefined,
    app_required_ctrl: :undefined,
    app_incl_ctrl: :undefined,
    app_uses_ctrl: :undefined,
    mods_source_ctrl: :undefined,
    mods_white_ctrl: :undefined,
    mods_black_ctrl: :undefined,
    mods_derived_ctrl: :undefined,
    deps_used_by_ctrl: :undefined,
    deps_uses_ctrl: :undefined,
    popup_menu: :undefined
  )

  Record.defrecord(:r_mod_win, :mod_win,
    name: :undefined,
    pid: :undefined
  )

  def start_link(wxEnv, xref, common, appName) do
    :proc_lib.start_link(
      :reltool_app_win,
      :init,
      [self(), wxEnv, xref, common, appName],
      :infinity,
      []
    )
  end

  def raise(pid) do
    :reltool_utils.cast(pid, :raise)
  end

  def refresh(pid) do
    :reltool_utils.cast(pid, :refresh)
  end

  def open_mod(pid, modName) do
    :reltool_utils.call(pid, {:open_mod, modName})
  end

  def init(parent, wxEnv, xref, c, appName) do
    try do
      do_init(parent, wxEnv, xref, c, appName)
    catch
      :error, reason ->
        exit({reason, __STACKTRACE__})
    end
  end

  defp do_init(parent, wxEnv, xref, c, appName) do
    :erlang.process_flag(:trap_exit, r_common(c, :trap_exit))
    {:ok, app} = :reltool_server.get_app(xref, appName)
    {:ok, sys} = :reltool_server.get_sys(xref)
    s = r_state(parent_pid: parent, xref_pid: xref, mod_wins: [], sys: sys, common: c, app: app)
    :proc_lib.init_ack(parent, {:ok, self()})
    :wx.set_env(wxEnv)
    :wx.debug(r_common(c, :wx_debug))

    s2 =
      :wx.batch(fn ->
        create_window(s)
      end)

    loop(s2)
  end

  def loop(r_state(xref_pid: xref, common: c, app: app) = s) do
    receive do
      {:system, from, msg} ->
        dbg = r_common(c, :sys_debug)
        :sys.handle_system_msg(msg, from, r_state(s, :parent_pid), :reltool_app_win, dbg, s)

      {:cast, _From, :raise} ->
        :wxFrame.raise(r_state(s, :frame))
        :wxFrame.setFocus(r_state(s, :frame))
        :reltool_app_win.loop(s)

      {:cast, _From, :refresh} ->
        case :reltool_server.get_app(xref, r_app(app, :name)) do
          {:ok, app2} ->
            {:ok, sys} = :reltool_server.get_sys(xref)
            s2 = redraw_window(r_state(s, sys: sys, app: app2))

            for mW <- r_state(s2, :mod_wins) do
              :ok = :reltool_mod_win.refresh(r_mod_win(mW, :pid))
            end

            :reltool_app_win.loop(s2)

          {:error, _Reason} ->
            :wxFrame.destroy(r_state(s, :frame))
            exit(:shutdown)
        end

      {:call, replyTo, ref, {:open_mod, modName}} ->
        s2 = create_mod_window(s, modName)

        {:value, r_mod_win(pid: modPid)} =
          :lists.keysearch(
            modName,
            r_mod_win(:name),
            r_state(s2, :mod_wins)
          )

        :reltool_utils.reply(replyTo, ref, {:ok, modPid})
        :reltool_app_win.loop(s2)

      r_wx(event: r_wxSize()) = wx ->
        wx2 = :reltool_utils.get_latest_resize(wx)
        s2 = handle_event(s, wx2)
        :reltool_app_win.loop(s2)

      r_wx(obj: objRef, event: r_wxClose(type: :close_window)) ->
        :wxFrame.destroy(objRef)
        exit(:shutdown)

      r_wx() = wx ->
        s2 = handle_event(s, wx)
        :reltool_app_win.loop(s2)

      {:EXIT, pid, reason} when pid === r_state(s, :parent_pid) ->
        exit(reason)

      {:EXIT, pid, _Reason} = exit ->
        exit_warning(exit)
        s2 = r_state(s, mod_wins: :lists.keydelete(pid, r_mod_win(:pid), r_state(s, :mod_wins)))
        :reltool_app_win.loop(s2)

      msg ->
        :error_logger.format('~w~w got unexpected message:\n\t~tp\n', [
          :reltool_app_win,
          self(),
          msg
        ])

        :reltool_app_win.loop(s)
    end
  end

  defp exit_warning({:EXIT, _Pid, :shutdown}) do
    :ok
  end

  defp exit_warning({:EXIT, _Pid, _Reason} = msg) do
    :error_logger.format('~w~w got unexpected message:\n\t~tp\n', [:reltool_app_win, self(), msg])
  end

  defp create_window(r_state(app: app) = s) do
    title = app_title(app)
    frame = :wxFrame.new(:wx.null(), -1, title, [])
    panel = :wxPanel.new(frame, [])
    statusBar = :wxFrame.createStatusBar(frame, [])
    book = :wxNotebook.new(panel, -1, [])
    s2 = r_state(s, frame: frame, panel: panel, book: book, status_bar: statusBar)
    derived = app_to_mods(s2)
    s3 = create_mods_page(s2, derived)
    s4 = create_apps_page(s3, derived)
    s5 = create_deps_page(s4, derived)
    s6 = create_config_page(s5)
    sizer = :wxBoxSizer.new(8)
    :wxSizer.add(sizer, book, [{:flag, 8192}, {:proportion, 1}])
    :wxPanel.setSizer(panel, sizer)
    :wxSizer.fit(sizer, frame)
    :wxSizer.setSizeHints(sizer, frame)
    :wxFrame.show(frame)
    :wxFrame.connect(frame, :close_window)
    s6
  end

  defp app_title(app) do
    :lists.concat([:reltool, ' - ', r_app(app, :label)])
  end

  defp create_apps_page(s, derived) do
    panel = :wxPanel.new(r_state(s, :book), [])
    main = :wxBoxSizer.new(8)
    upper = :wxBoxSizer.new(4)
    lower = :wxBoxSizer.new(4)
    usedByCtrl = create_apps_list_ctrl(panel, upper, 'Used by')

    :wxSizer.add(
      upper,
      :wxStaticLine.new(panel, [{:style, 8}]),
      [{:border, 2}, {:flag, 64 ||| 128 ||| 32 ||| 16 ||| 8192}]
    )

    requiredCtrl = create_apps_list_ctrl(panel, upper, 'Required')

    :wxSizer.add(
      upper,
      :wxStaticLine.new(panel, [{:style, 8}]),
      [{:border, 2}, {:flag, 64 ||| 128 ||| 32 ||| 16 ||| 8192}]
    )

    inclCtrl = create_apps_list_ctrl(panel, upper, 'Included')

    :wxSizer.add(
      upper,
      :wxStaticLine.new(panel, [{:style, 8}]),
      [{:border, 2}, {:flag, 64 ||| 128 ||| 32 ||| 16 ||| 8192}]
    )

    usesCtrl = create_apps_list_ctrl(panel, upper, 'Uses')

    s2 =
      r_state(s,
        app_required_ctrl: requiredCtrl,
        app_used_by_ctrl: usedByCtrl,
        app_incl_ctrl: inclCtrl,
        app_uses_ctrl: usesCtrl
      )

    redraw_apps(s2, derived)

    :wxSizer.add(main, upper, [
      {:border, 2},
      {:flag, 64 ||| 128 ||| 32 ||| 16 ||| 8192},
      {:proportion, 1}
    ])

    :wxSizer.add(main, lower, [{:border, 2}, {:flag, 64 ||| 128 ||| 32 ||| 16 ||| 8192}])
    :wxPanel.setSizer(panel, main)
    :wxNotebook.addPage(r_state(s2, :book), panel, 'Application dependencies', [])
    s2
  end

  defp create_apps_list_ctrl(panel, sizer, text) do
    width = div(:lists.max([100, 800 - 40]), 4)
    height = :lists.max([100, 600 - 100])

    listCtrl =
      :wxListCtrl.new(
        panel,
        [{:style, 32 ||| 8192 ||| 1_073_741_824 ||| 2_147_483_648}, {:size, {width, height}}]
      )

    :reltool_utils.assign_image_list(listCtrl)
    listItem = :wxListItem.new()
    :wxListItem.setAlign(listItem, 0)
    :wxListItem.setText(listItem, text)

    :wxListItem.setWidth(
      listItem,
      :reltool_utils.get_column_width(listCtrl)
    )

    :wxListCtrl.insertColumn(listCtrl, 0, listItem)
    :wxListItem.destroy(listItem)

    :wxSizer.add(sizer, listCtrl, [
      {:border, 2},
      {:flag, 64 ||| 128 ||| 32 ||| 16 ||| 8192},
      {:proportion, 1}
    ])

    :wxEvtHandler.connect(listCtrl, :size, [{:skip, true}, {:userData, :apps_list_ctrl}])

    :wxListCtrl.connect(
      listCtrl,
      :command_list_item_activated,
      [{:userData, :open_app}]
    )

    :wxWindow.connect(listCtrl, :enter_window)
    listCtrl
  end

  defp create_deps_page(s, derived) do
    panel = :wxPanel.new(r_state(s, :book), [])
    main = :wxBoxSizer.new(4)

    usedByCtrl =
      create_mods_list_ctrl(
        panel,
        main,
        'Modules using this',
        ' and their applications',
        :undefined,
        :undefined
      )

    :wxSizer.add(
      main,
      :wxStaticLine.new(panel, [{:style, 8}]),
      [{:border, 2}, {:flag, 64 ||| 128 ||| 32 ||| 16 ||| 8192}]
    )

    usesCtrl =
      create_mods_list_ctrl(
        panel,
        main,
        'Used modules',
        ' and their applications',
        :undefined,
        :undefined
      )

    s2 =
      r_state(s,
        deps_used_by_ctrl: usedByCtrl,
        deps_uses_ctrl: usesCtrl
      )

    redraw_mods(s2, derived)
    :wxPanel.setSizer(panel, main)
    :wxNotebook.addPage(r_state(s2, :book), panel, 'Module dependencies', [])
    s2
  end

  defp create_mods_page(s, derived) do
    panel = :wxPanel.new(r_state(s, :book), [])
    mainSz = :wxBoxSizer.new(4)

    sourceCtrl =
      create_mods_list_ctrl(panel, mainSz, 'Available', '', :whitelist_add, :blacklist_add)

    :wxSizer.add(
      mainSz,
      :wxStaticLine.new(panel, [{:style, 8}]),
      [{:border, 2}, {:flag, 64 ||| 128 ||| 32 ||| 16 ||| 8192}]
    )

    whiteCtrl =
      create_mods_list_ctrl(panel, mainSz, 'Included', '', :whitelist_del, :blacklist_add)

    :wxSizer.add(
      mainSz,
      :wxStaticLine.new(panel, [{:style, 8}]),
      [{:border, 2}, {:flag, 64 ||| 128 ||| 32 ||| 16 ||| 8192}]
    )

    blackCtrl =
      create_mods_list_ctrl(panel, mainSz, 'Excluded', '', :whitelist_add, :blacklist_del)

    :wxSizer.add(
      mainSz,
      :wxStaticLine.new(panel, [{:style, 8}]),
      [{:border, 2}, {:flag, 64 ||| 128 ||| 32 ||| 16 ||| 8192}]
    )

    derivedCtrl =
      create_mods_list_ctrl(panel, mainSz, 'Derived', '', :whitelist_add, :blacklist_add)

    s2 =
      r_state(s,
        mods_source_ctrl: sourceCtrl,
        mods_white_ctrl: whiteCtrl,
        mods_black_ctrl: blackCtrl,
        mods_derived_ctrl: derivedCtrl
      )

    redraw_mods(s2, derived)
    :wxPanel.setSizer(panel, mainSz)
    :wxNotebook.addPage(r_state(s2, :book), panel, 'Modules', [])
    s2
  end

  defp create_mods_list_ctrl(panel, outerSz, title, appText, tick, cross) do
    listCtrl =
      :wxListCtrl.new(
        panel,
        [{:style, 32 ||| 1_073_741_824 ||| 2_147_483_648}]
      )

    toolTip = 'Select module(s) or open separate module window with a double click.'
    :wxListCtrl.setToolTip(listCtrl, toolTip)
    :reltool_utils.assign_image_list(listCtrl)
    listItem = :wxListItem.new()
    :wxListItem.setAlign(listItem, 0)
    :wxListItem.setText(listItem, title)
    :wxListCtrl.insertColumn(listCtrl, 0, listItem)

    prop =
      case appText !== '' do
        true ->
          :wxListItem.setText(listItem, appText)
          :wxListCtrl.insertColumn(listCtrl, 1, listItem)
          2

        false ->
          1
      end

    :wxListItem.destroy(listItem)
    buttonSz = :wxBoxSizer.new(4)
    create_button(panel, buttonSz, listCtrl, title, 'wxART_TICK_MARK', tick)
    create_button(panel, buttonSz, listCtrl, title, 'wxART_CROSS_MARK', cross)
    :wxEvtHandler.connect(listCtrl, :size, [{:skip, true}, {:userData, :mods_list_ctrl}])

    :wxListCtrl.connect(
      listCtrl,
      :command_list_item_activated,
      [{:userData, :open_mod}]
    )

    :wxWindow.connect(listCtrl, :enter_window)
    innerSz = :wxBoxSizer.new(8)

    :wxSizer.add(innerSz, listCtrl, [
      {:border, 2},
      {:flag, 64 ||| 128 ||| 32 ||| 16 ||| 8192},
      {:proportion, 1}
    ])

    :wxSizer.add(innerSz, buttonSz, [{:flag, 64 ||| 128 ||| 32 ||| 16 ||| 8192}])

    :wxSizer.add(outerSz, innerSz, [
      {:flag, 64 ||| 128 ||| 32 ||| 16 ||| 8192},
      {:proportion, prop}
    ])

    listCtrl
  end

  defp create_button(_Panel, sizer, _ListCtrl, _Title, _BitMapName, :undefined) do
    :wxSizer.addStretchSpacer(sizer)
  end

  defp create_button(panel, sizer, listCtrl, title, bitMapName, action) do
    bitMap = :wxArtProvider.getBitmap(bitMapName)
    button = :wxBitmapButton.new(panel, -1, bitMap, [])
    toolTip = action_to_tool_tip(title, action)
    :wxBitmapButton.setToolTip(button, toolTip)
    opts = [{:userData, {:mod_button, action, listCtrl}}]
    :wxEvtHandler.connect(button, :command_button_clicked, opts)

    :wxSizer.add(sizer, button, [
      {:border, 2},
      {:flag, 64 ||| 128 ||| 32 ||| 16 ||| 256},
      {:proportion, 1}
    ])
  end

  defp action_to_tool_tip(label, action) do
    case action do
      :whitelist_add when label === 'Included' ->
        'Remove selected module(s) from whitelist.'

      :whitelist_add ->
        'Add selected module(s) to whitelist.'

      :whitelist_del ->
        'Remove selected module(s)from whitelist.'

      :blacklist_add when label === 'Excluded' ->
        'Remove selected module(s) from blacklist.'

      :blacklist_add ->
        'Add selected module(s) to blacklist.'

      :blacklist_del ->
        'Remove selected module(s) from blacklist.'
    end
  end

  defp create_config_page(r_state(app: app) = s) do
    panel = :wxPanel.new(r_state(s, :book), [])
    topSizer = :wxBoxSizer.new(8)

    {latestRadio, selectedRadio, sourceBox} =
      create_double_box(
        panel,
        topSizer,
        'Source selection policy',
        'Use latest version',
        :use_latest_vsn,
        'Use selected version',
        :use_selected_vsn,
        'Directories',
        r_app(app, :sorted_dirs),
        :version
      )

    inclSizer = :wxBoxSizer.new(4)

    {appGlobalRadio, appLocalRadio, appLocalBox} =
      create_double_box(
        panel,
        inclSizer,
        'Application inclusion policy',
        'Use global config',
        :global_incl_cond,
        'Use application specific config',
        :local_incl_cond,
        'Application specific',
        :reltool_utils.incl_conds(),
        :incl_cond
      )

    {modGlobalRadio, modLocalRadio, modLocalBox} =
      create_double_box(
        panel,
        inclSizer,
        'Module inclusion policy',
        'Use global config',
        :global_mod_cond,
        'Use application specific config',
        :local_mod_cond,
        'Application specific',
        :reltool_utils.mod_conds(),
        :mod_cond
      )

    :wxSizer.add(topSizer, inclSizer, [
      {:border, 2},
      {:flag, 64 ||| 128 ||| 32 ||| 16 ||| 8192},
      {:proportion, 1}
    ])

    s2 =
      r_state(s,
        config_app_global: appGlobalRadio,
        config_app_local: appLocalRadio,
        config_app_local_box: appLocalBox,
        config_mod_global: modGlobalRadio,
        config_mod_local: modLocalRadio,
        config_mod_local_box: modLocalBox,
        config_latest: latestRadio,
        config_selected: selectedRadio,
        config_source_box: sourceBox
      )

    redraw_config(s2)
    :wxPanel.setSizer(panel, topSizer)
    :wxNotebook.addPage(r_state(s2, :book), panel, 'Application settings', [])
    s2
  end

  defp create_double_box(
         panel,
         sizer,
         topLabel,
         outerText,
         outerData,
         innerText,
         innerData,
         internalLabel,
         internalChoices,
         internalChoiceData
       ) do
    topSizer = :wxStaticBoxSizer.new(8, panel, [{:label, topLabel}])
    outerSizer = :wxBoxSizer.new(8)
    outerRadio = :wxRadioButton.new(panel, -1, outerText, [{:style, 4}])

    :wxEvtHandler.connect(
      outerRadio,
      :command_radiobutton_selected,
      [{:userData, outerData}]
    )

    innerRadio = :wxRadioButton.new(panel, -1, innerText)

    :wxEvtHandler.connect(
      innerRadio,
      :command_radiobutton_selected,
      [{:userData, innerData}]
    )

    innerBox = :wxRadioBox.new(panel, -1, internalLabel, {-1, -1}, {-1, -1}, internalChoices, [])

    :wxEvtHandler.connect(
      innerBox,
      :command_radiobox_selected,
      [{:userData, internalChoiceData}]
    )

    :wxSizer.add(outerSizer, outerRadio, [
      {:border, 2},
      {:flag, 64 ||| 128 ||| 32 ||| 16 ||| 8192}
    ])

    :wxSizer.add(outerSizer, innerRadio, [
      {:border, 2},
      {:flag, 64 ||| 128 ||| 32 ||| 16 ||| 8192}
    ])

    :wxSizer.add(topSizer, outerSizer, [{:border, 2}, {:flag, 64 ||| 128 ||| 32 ||| 16 ||| 8192}])

    :wxSizer.add(topSizer, innerBox, [
      {:border, 2},
      {:flag, 64 ||| 128 ||| 32 ||| 16 ||| 8192},
      {:proportion, 1}
    ])

    :wxSizer.add(sizer, topSizer, [
      {:border, 2},
      {:flag, 64 ||| 128 ||| 32 ||| 16 ||| 8192},
      {:proportion, 1}
    ])

    {outerRadio, innerRadio, innerBox}
  end

  defp handle_event(r_state(sys: sys, app: app) = s, wx) do
    case wx do
      r_wx(obj: objRef, event: r_wxMouse(type: :enter_window)) ->
        :wxWindow.setFocus(objRef)
        s

      r_wx(obj: listCtrl, userData: :mods_list_ctrl, event: r_wxSize(type: :size, size: {w, _H})) ->
        hasApps = :wxListCtrl.getColumnCount(listCtrl) > 1

        case hasApps do
          false ->
            :wxListCtrl.setColumnWidth(listCtrl, 0, w)

          true ->
            :wxListCtrl.setColumnWidth(listCtrl, 0, div(2 * w, 3))
            :wxListCtrl.setColumnWidth(listCtrl, 1, div(w, 3))
        end

        s

      r_wx(obj: listCtrl, userData: :apps_list_ctrl, event: r_wxSize(type: :size, size: {w, _H})) ->
        :wxListCtrl.setColumnWidth(listCtrl, 0, w)
        s

      r_wx(
        userData: :open_app,
        obj: listCtrl,
        event:
          r_wxList(
            type: :command_list_item_activated,
            itemIndex: pos
          )
      ) ->
        appBase = :wxListCtrl.getItemText(listCtrl, pos)
        {appName, _AppVsn} = :reltool_utils.split_app_name(appBase)

        {:ok, _AppPid} =
          :reltool_sys_win.open_app(
            r_state(s, :parent_pid),
            appName
          )

        s

      r_wx(
        userData: :open_mod,
        obj: listCtrl,
        event:
          r_wxList(
            type: :command_list_item_activated,
            itemIndex: pos
          )
      ) ->
        modName =
          :erlang.list_to_atom(
            :wxListCtrl.getItemText(
              listCtrl,
              pos
            )
          )

        create_mod_window(s, modName)

      r_wx(userData: :global_incl_cond) ->
        change_incl_cond(s, app, :undefined)

      r_wx(userData: :local_incl_cond) ->
        change_incl_cond(s, app, r_sys(sys, :incl_cond))

      r_wx(
        userData: :incl_cond,
        event:
          r_wxCommand(
            type: :command_radiobox_selected,
            cmdString: sel
          )
      ) ->
        appCond = :reltool_utils.list_to_incl_cond(sel)
        change_incl_cond(s, app, appCond)

      r_wx(userData: :global_mod_cond) ->
        change_mod_cond(s, app, :undefined)

      r_wx(userData: :local_mod_cond) ->
        change_mod_cond(s, app, r_sys(sys, :mod_cond))

      r_wx(
        userData: :mod_cond,
        event:
          r_wxCommand(
            type: :command_radiobox_selected,
            cmdString: sel
          )
      ) ->
        modCond = :reltool_utils.list_to_mod_cond(sel)
        change_mod_cond(s, app, modCond)

      r_wx(userData: :use_latest_vsn) ->
        app2 = r_app(app, use_selected_vsn: :undefined)
        s2 = change_version(s, app2, r_app(app, :active_dir))
        redraw_window(s2)

      r_wx(userData: :use_selected_vsn) ->
        app2 = r_app(app, use_selected_vsn: :dir)

        {:ok, app3} =
          :reltool_sys_win.set_app(
            r_state(s, :parent_pid),
            app2
          )

        s2 = r_state(s, app: app3)
        redraw_window(s2)

      r_wx(
        userData: :version,
        event:
          r_wxCommand(
            type: :command_radiobox_selected,
            cmdString: activeDir
          )
      ) ->
        app2 = r_app(app, use_selected_vsn: :dir)
        s2 = change_version(s, app2, activeDir)
        redraw_window(s2)

      r_wx(
        userData: {:mod_button, action, listCtrl},
        event: r_wxCommand(type: :command_button_clicked)
      ) ->
        items = :reltool_utils.get_items(listCtrl)
        handle_mod_button(s, items, action)

      _ ->
        :error_logger.format('~w~w got unexpected app event from wx:\n\t~tp\n', [
          :reltool_app_win,
          self(),
          wx
        ])

        s
    end
  end

  defp create_mod_window(
         r_state(parent_pid: relPid, xref_pid: xref, common: c) = s,
         modName
       ) do
    case :lists.keysearch(modName, r_mod_win(:name), r_state(s, :mod_wins)) do
      false ->
        wxEnv = :wx.get_env()
        {:ok, pid} = :reltool_mod_win.start_link(wxEnv, xref, relPid, c, modName)
        mW = r_mod_win(name: modName, pid: pid)
        r_state(s, mod_wins: [mW | r_state(s, :mod_wins)])

      {:value, mW} ->
        :reltool_app_win.raise(r_mod_win(mW, :pid))
        s
    end
  end

  defp handle_mod_button(r_state(app: app) = s, items, action) do
    app2 =
      :lists.foldl(
        fn item, a ->
          move_mod(a, item, action)
        end,
        app,
        items
      )

    {:ok, app3} =
      :reltool_sys_win.set_app(
        r_state(s, :parent_pid),
        app2
      )

    s2 = r_state(s, app: app3)
    redraw_window(s2)
  end

  defp move_mod(app, {_ItemNo, modStr}, action) do
    modName = :erlang.list_to_atom(modStr)
    mods = r_app(app, :mods)
    {:value, m} = :lists.keysearch(modName, r_mod(:name), mods)

    appCond =
      case action do
        :whitelist_add ->
          case r_mod(m, :incl_cond) do
            :include ->
              :undefined

            :exclude ->
              :include

            :undefined ->
              :include
          end

        :whitelist_del ->
          :undefined

        :blacklist_add ->
          :exclude

        :blacklist_del ->
          :undefined

        _ ->
          :error_logger.format(
            '~w~w got unexpected mod button event: ~w\n\t ~tp\n',
            [:reltool_app_win, self(), modName, action]
          )

          r_mod(m, :incl_cond)
      end

    m2 = r_mod(m, incl_cond: appCond)
    mods2 = :lists.keystore(modName, r_mod(:name), mods, m2)
    r_app(app, mods: mods2)
  end

  defp change_incl_cond(s, app, newAppCond) do
    app2 = r_app(app, incl_cond: newAppCond)

    {:ok, app3} =
      :reltool_sys_win.set_app(
        r_state(s, :parent_pid),
        app2
      )

    s2 = r_state(s, app: app3)
    redraw_window(s2)
  end

  defp change_mod_cond(s, app, newModCond) do
    app2 = r_app(app, mod_cond: newModCond)

    {:ok, app3} =
      :reltool_sys_win.set_app(
        r_state(s, :parent_pid),
        app2
      )

    s2 = r_state(s, app: app3)
    redraw_window(s2)
  end

  defp change_version(s, app, newDir) do
    app2 = r_app(app, active_dir: newDir, label: :undefined, vsn: :undefined, info: :undefined)

    {:ok, app3} =
      :reltool_sys_win.set_app(
        r_state(s, :parent_pid),
        app2
      )

    title = app_title(app3)
    :wxFrame.setTitle(r_state(s, :frame), title)
    r_state(s, app: app3)
  end

  defp redraw_apps(
         r_state(
           app: r_app(info: appInfo),
           app_used_by_ctrl: usedByCtrl,
           app_required_ctrl: requiredCtrl,
           app_incl_ctrl: inclCtrl,
           app_uses_ctrl: usesCtrl,
           xref_pid: xref
         ),
         {_SourceMods, _WhiteMods, _BlackMods, _DerivedMods, usedByMods, usesMods}
       ) do
    usedByApps =
      :lists.usort(
        for {image, _, m} <- usedByMods do
          {r_mod(m, :app_name), image}
        end
      )

    select = fn appName ->
      {:ok, app} = :reltool_server.get_app(xref, appName)

      case r_app(app, :status) do
        :missing ->
          {appName, 0}

        :ok ->
          {appName, 3}
      end
    end

    requiredApps =
      :lists.sort(
        :lists.map(
          select,
          r_app_info(appInfo, :applications)
        )
      )

    inclApps = :lists.map(select, r_app_info(appInfo, :incl_apps))

    usesApps =
      :lists.usort(
        for {image, _, m} <- usesMods do
          {r_mod(m, :app_name), image}
        end
      )

    do_redraw_apps(usedByCtrl, usedByApps)
    do_redraw_apps(requiredCtrl, requiredApps)
    do_redraw_apps(inclCtrl, inclApps)
    do_redraw_apps(usesCtrl, usesApps)
    :ok
  end

  defp do_redraw_apps(listCtrl, []) do
    :wxListCtrl.deleteAllItems(listCtrl)
  end

  defp do_redraw_apps(listCtrl, appImages) do
    :wxListCtrl.deleteAllItems(listCtrl)

    add = fn
      {appName, imageId}, {row, prev}
      when appName !== prev ->
        :wxListCtrl.insertItem(listCtrl, row, '')

        cond do
          rem(row, 2) === 0 ->
            :wxListCtrl.setItemBackgroundColour(listCtrl, row, {240, 240, 255})

          true ->
            :ignore
        end

        str = :erlang.atom_to_list(appName)
        :wxListCtrl.setItem(listCtrl, row, 0, str, [{:imageId, imageId}])
        {row + 1, appName}

      {_, _}, acc ->
        acc
    end

    :wx.foldl(add, {0, :undefined}, appImages)
  end

  defp redraw_mods(
         r_state(
           mods_source_ctrl: sourceCtrl,
           mods_white_ctrl: whiteCtrl,
           mods_black_ctrl: blackCtrl,
           mods_derived_ctrl: derivedCtrl,
           deps_used_by_ctrl: usedByCtrl,
           deps_uses_ctrl: usesCtrl,
           app: r_app(is_pre_included: isPre, is_included: isIncl),
           status_bar: bar
         ),
         {sourceMods, whiteMods, blackMods, derivedMods, usedByMods, usesMods}
       ) do
    inclStatus =
      case isIncl do
        true when isPre === true ->
          'Whitelist - '

        true ->
          'Derived - '

        false ->
          'Blacklist - '

        :undefined ->
          'Source - '
      end

    status =
      :lists.concat([
        inclStatus,
        length(whiteMods),
        ' whitelisted modules and ',
        length(derivedMods),
        ' derived modules.'
      ])

    :wxStatusBar.setStatusText(bar, status)
    opt_redraw_mods(sourceCtrl, sourceMods)
    opt_redraw_mods(whiteCtrl, whiteMods)
    opt_redraw_mods(blackCtrl, blackMods)
    opt_redraw_mods(derivedCtrl, derivedMods)
    opt_redraw_mods(usedByCtrl, usedByMods)
    opt_redraw_mods(usesCtrl, usesMods)
  end

  defp app_to_mods(r_state(xref_pid: xref, app: app)) do
    sourceMods =
      for m <- r_app(app, :mods),
          r_mod(m, :is_included) !== true,
          r_mod(m, :is_pre_included) !== false do
        m
      end

    whiteMods =
      for m <- r_app(app, :mods),
          r_mod(m, :is_pre_included) === true do
        m
      end

    blackMods =
      for m <- r_app(app, :mods),
          r_mod(m, :is_pre_included) === false do
        m
      end

    derivedMods =
      for m <- r_app(app, :mods),
          r_mod(m, :is_included) === true,
          r_mod(m, :is_pre_included) !== true do
        m
      end

    getMod = fn modName when is_atom(modName) ->
      {:ok, m} = :reltool_server.get_mod(xref, modName)

      cond do
        r_mod(m, :app_name) === r_app(app, :name) and
            r_mod(m, :is_included) === true ->
          false

        true ->
          {true, m}
      end
    end

    usedByMods = :lists.zf(getMod, r_app(app, :used_by_mods))
    usesMods = :lists.zf(getMod, r_app(app, :uses_mods))

    {for m <- sourceMods do
       select_image(:source, m)
     end,
     for m <- whiteMods do
       select_image(:whitelist, m)
     end,
     for m <- blackMods do
       select_image(:blacklist, m)
     end,
     for m <- derivedMods do
       select_image(:derived, m)
     end,
     for m <- usedByMods do
       select_image(:used_by, m)
     end,
     for m <- usesMods do
       select_image(:uses, m)
     end}
  end

  defp select_image(kind, m) do
    image =
      case kind do
        :blacklist when r_mod(m, :status) === :missing ->
          1

        :source when r_mod(m, :status) === :missing ->
          1

        _ when r_mod(m, :status) === :missing ->
          0

        :blacklist when r_mod(m, :incl_cond) === :exclude ->
          4

        :blacklist ->
          5

        :source ->
          4

        :whitelist when r_mod(m, :incl_cond) === :include ->
          3

        :whitelist ->
          5

        :derived ->
          3

        :used_by when r_mod(m, :is_included) === true ->
          3

        :used_by when r_mod(m, :is_included) === false ->
          1

        :used_by ->
          0

        :uses when r_mod(m, :is_included) === true ->
          3

        :uses when r_mod(m, :is_included) === false ->
          1

        :uses ->
          0
      end

    {image, r_mod(m, :app_name), m}
  end

  defp opt_redraw_mods(:undefined, _ImageMods) do
    :ok
  end

  defp opt_redraw_mods(listCtrl, imageMods) do
    hasApps = :wxListCtrl.getColumnCount(listCtrl) > 1
    do_redraw_mods(listCtrl, imageMods, hasApps)
  end

  defp do_redraw_mods(listCtrl, [], _HasApps) do
    :wxListCtrl.deleteAllItems(listCtrl)
  end

  defp do_redraw_mods(listCtrl, imageMods, hasApps) do
    :wxListCtrl.deleteAllItems(listCtrl)

    add = fn {imageId, appName, r_mod(name: modName)}, row ->
      :wxListCtrl.insertItem(listCtrl, row, '')

      cond do
        rem(row, 2) === 0 ->
          :wxListCtrl.setItemBackgroundColour(listCtrl, row, {240, 240, 255})

        true ->
          :ignore
      end

      :wxListCtrl.setItem(listCtrl, row, 0, :erlang.atom_to_list(modName), [{:imageId, imageId}])

      case hasApps do
        false ->
          :ok

        true ->
          :wxListCtrl.setItem(listCtrl, row, 1, :erlang.atom_to_list(appName), [
            {:imageId, imageId}
          ])
      end

      row + 1
    end

    :wx.foldl(add, 0, :lists.sort(imageMods))
  end

  defp redraw_config(
         r_state(
           sys:
             r_sys(
               incl_cond: globalIncl,
               mod_cond: globalSource
             ),
           app:
             r_app(
               incl_cond: localIncl,
               mod_cond: localSource,
               use_selected_vsn: useSelected,
               active_dir: activeDir,
               sorted_dirs: sortedDirs
             ),
           config_app_global: appGlobalRadio,
           config_app_local: appLocalRadio,
           config_app_local_box: appLocalBox,
           config_mod_global: modGlobalRadio,
           config_mod_local: modLocalRadio,
           config_mod_local_box: modLocalBox,
           config_latest: latestRadio,
           config_selected: selectedRadio,
           config_source_box: sourceBox
         )
       ) do
    redraw_double_box(
      globalIncl,
      localIncl,
      appGlobalRadio,
      appLocalRadio,
      appLocalBox,
      &:reltool_utils.incl_cond_to_index/1
    )

    redraw_double_box(
      globalSource,
      localSource,
      modGlobalRadio,
      modLocalRadio,
      modLocalBox,
      &:reltool_utils.mod_cond_to_index/1
    )

    redraw_double_box(false, useSelected, latestRadio, selectedRadio, sourceBox, fn
      false ->
        0

      _ ->
        :reltool_utils.elem_to_index(
          activeDir,
          sortedDirs
        ) - 1
    end)
  end

  defp redraw_double_box(global, local, globalRadio, localRadio, localBox, getChoice) do
    appCond =
      case local do
        :undefined ->
          :wxRadioButton.setValue(globalRadio, true)
          :wxRadioButton.setValue(localRadio, false)
          :wxRadioBox.disable(localBox)
          global

        _ ->
          :wxRadioButton.setValue(globalRadio, false)
          :wxRadioButton.setValue(localRadio, true)
          :wxRadioBox.enable(localBox)
          local
      end

    choice = getChoice.(appCond)
    :wxRadioBox.setSelection(localBox, choice)
  end

  defp redraw_window(s) do
    derived = app_to_mods(s)
    redraw_config(s)
    redraw_mods(s, derived)
    redraw_apps(s, derived)
    s
  end

  def system_continue(_Parent, _Debug, s) do
    :reltool_app_win.loop(s)
  end

  def system_terminate(reason, _Parent, _Debug, _S) do
    exit(reason)
  end

  def system_code_change(s, _Module, _OldVsn, _Extra) do
    {:ok, s}
  end
end
