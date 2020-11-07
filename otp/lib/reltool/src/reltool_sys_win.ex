defmodule :m_reltool_sys_win do
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
    server_pid: :undefined,
    app_wins: :undefined,
    sys: :undefined,
    common: :undefined,
    config_file: :undefined,
    target_dir: :undefined,
    boot_dir: :undefined,
    frame: :undefined,
    panel: :undefined,
    book: :undefined,
    rel_book: :undefined,
    lib_tree: :undefined,
    status_bar: :undefined,
    source: :undefined,
    whitelist: :undefined,
    blacklist: :undefined,
    derived: :undefined,
    fgraph_wins: :undefined,
    app_box: :undefined,
    mod_box: :undefined,
    warning_list: :undefined,
    warning_wins: :undefined
  )

  Record.defrecord(:r_root_data, :root_data, dir: :undefined)
  Record.defrecord(:r_lib_data, :lib_data, dir: :undefined, tree: :undefined, item: :undefined)

  Record.defrecord(:r_escript_data, :escript_data,
    file: :undefined,
    tree: :undefined,
    item: :undefined
  )

  Record.defrecord(:r_app_data, :app_data,
    name: :undefined,
    dir: :undefined
  )

  Record.defrecord(:r_app_win, :app_win,
    name: :undefined,
    pid: :undefined
  )

  Record.defrecord(:r_fgraph_win, :fgraph_win,
    frame: :undefined,
    pid: :undefined
  )

  Record.defrecord(:r_root_popup, :root_popup,
    dir: :undefined,
    choices: :undefined,
    tree: :undefined,
    item: :undefined
  )

  Record.defrecord(:r_lib_popup, :lib_popup,
    dir: :undefined,
    choices: :undefined,
    tree: :undefined,
    item: :undefined
  )

  Record.defrecord(:r_escript_popup, :escript_popup,
    file: :undefined,
    choices: :undefined,
    tree: :undefined,
    item: :undefined
  )

  def start_link(opts) do
    :proc_lib.start_link(
      :reltool_sys_win,
      :init,
      [
        [
          [{:safe_config, false}, {:parent, self()}]
          | opts
        ]
      ],
      :infinity,
      []
    )
  end

  def get_server(pid) do
    :reltool_utils.call(pid, :get_server)
  end

  def set_app(pid, app) do
    :reltool_utils.call(pid, {:set_app, app})
  end

  def open_app(pid, appName) do
    :reltool_utils.call(pid, {:open_app, appName})
  end

  def init(options) do
    try do
      do_init(options)
    catch
      :error, reason ->
        :io.format('~tp: ~tp~n', [reason, __STACKTRACE__])
        exit({reason, __STACKTRACE__})
    end
  end

  defp do_init([
         [{:safe_config, safe}, {:parent, parent}]
         | options
       ]) do
    case :reltool_server.start_link(options) do
      {:ok, serverPid, c, sys} ->
        :erlang.process_flag(:trap_exit, r_common(c, :trap_exit))
        :wx.new()
        :wx.debug(r_common(c, :wx_debug))
        {:ok, warnings} = :reltool_server.get_status(serverPid)
        exit_dialog(warnings)

        s =
          r_state(
            parent_pid: parent,
            server_pid: serverPid,
            common: c,
            config_file: :filename.absname('config.reltool'),
            target_dir: :filename.absname('reltool_target_dir'),
            app_wins: [],
            sys: sys,
            fgraph_wins: [],
            warning_wins: []
          )

        s2 = create_window(s)

        s5 =
          :wx.batch(fn ->
            title = :erlang.atom_to_list(:reltool)
            :wxFrame.setTitle(r_state(s2, :frame), title)
            :wxStatusBar.setStatusText(r_state(s2, :status_bar), 'Done.')
            s3 = redraw_apps(s2)
            s4 = redraw_libs(s3)
            redraw_config_page(s4)
          end)

        :proc_lib.init_ack(r_state(s, :parent_pid), {:ok, self()})
        loop(s5)

      {:error, reason} ->
        restart_server_safe_config(safe, parent, reason)
    end
  end

  defp restart_server_safe_config(true, parent, reason) do
    :io.format('~w(~w): <ERROR> ~tp\n', [:reltool_sys_win, 185, reason])
    :proc_lib.init_ack(parent, {:error, reason})
  end

  defp restart_server_safe_config(false, parent, reason) do
    :wx.new()

    strings = [
      {:wxe_util.get_const(:wxBLACK), 'Could not start reltool server:\n\n'},
      {:wxe_util.get_const(:wxRED), reason ++ '\n\n'},
      {:wxe_util.get_const(:wxBLACK),
       :io_lib.format(
         'Resetting the configuration to:~n~n  ~p~n~nDo you want to continue with this configuration?',
         [
           {:sys,
            [
              {:incl_cond, :exclude},
              {:app, :kernel, [{:incl_cond, :include}]},
              {:app, :stdlib, [{:incl_cond, :include}]},
              {:app, :sasl, [{:incl_cond, :include}]}
            ]}
         ]
       )}
    ]

    case question_dialog_2('Reltool server start error', strings) do
      5100 ->
        do_init([
          {:safe_config, true},
          {:parent, parent},
          {:sys,
           [
             {:incl_cond, :exclude},
             {:app, :kernel, [{:incl_cond, :include}]},
             {:app, :stdlib, [{:incl_cond, :include}]},
             {:app, :sasl, [{:incl_cond, :include}]}
           ]}
        ])

      5101 ->
        :io.format('~w(~w): <ERROR> ~tp\n', [:reltool_sys_win, 202, reason])
        :proc_lib.init_ack(parent, {:error, reason})
    end
  end

  defp exit_dialog([]) do
    :ok
  end

  defp exit_dialog(warnings) do
    question = 'Do you want to continue despite these warnings?'

    details =
      :lists.flatten(
        for w <- warnings do
          [w, ?\n]
        end
      )

    case question_dialog(question, details) do
      5100 ->
        :ok

      5101 ->
        :io.format('~w(~w): <ERROR> ~ts\n', [:reltool_sys_win, 215, details])
        exit(details)
    end
  end

  def loop(s) do
    receive do
      {:system, from, msg} ->
        common = r_state(s, :common)

        :sys.handle_system_msg(
          msg,
          from,
          r_state(s, :parent_pid),
          :reltool_sys_win,
          r_common(common, :sys_debug),
          s
        )

      r_wx(obj: objRef, event: r_wxClose(type: :close_window)) = msg ->
        cond do
          objRef === r_state(s, :frame) ->
            :wxFrame.destroy(objRef)
            exit(:shutdown)

          true ->
            fWs = r_state(s, :fgraph_wins)

            case :lists.keysearch(objRef, r_fgraph_win(:frame), fWs) do
              {:value, fW} ->
                :reltool_fgraph_win.stop(r_fgraph_win(fW, :pid), :shutdown)
                :wxFrame.destroy(objRef)
                fWs2 = :lists.keydelete(objRef, r_fgraph_win(:frame), fWs)
                :reltool_sys_win.loop(r_state(s, fgraph_wins: fWs2))

              false ->
                wWs = r_state(s, :warning_wins)

                case :lists.member(objRef, wWs) do
                  true ->
                    :wxFrame.destroy(objRef)
                    wWs2 = :lists.delete(objRef, wWs)
                    :reltool_sys_win.loop(r_state(s, warning_wins: wWs2))

                  false ->
                    :error_logger.format('~w~w got unexpected message:\n\t~tp\n', [
                      :reltool_sys_win,
                      self(),
                      msg
                    ])

                    :reltool_sys_win.loop(s)
                end
            end
        end

      r_wx(id: 5006, event: r_wxCommand(type: :command_menu_selected), userData: :main_window) ->
        :wxFrame.destroy(r_state(s, :frame))
        exit(:shutdown)

      r_wx(event: r_wxSize()) = wx ->
        wx2 = :reltool_utils.get_latest_resize(wx)
        s2 = handle_event(s, wx2)
        :reltool_sys_win.loop(s2)

      r_wx() = wx ->
        s2 = handle_event(s, wx)
        :reltool_sys_win.loop(s2)

      {:call, replyTo, ref, :get_server} ->
        :reltool_utils.reply(replyTo, ref, {:ok, r_state(s, :server_pid)})
        :reltool_sys_win.loop(s)

      {:call, replyTo, ref, {:set_app, newApp}} ->
        {:ok, analysedApp, s2} = do_set_app(s, newApp)
        :reltool_utils.reply(replyTo, ref, {:ok, analysedApp})
        :reltool_sys_win.loop(s2)

      {:call, replyTo, ref, {:open_app, appName}} ->
        s2 = do_open_app(s, appName)

        {:value, r_app_win(pid: appPid)} =
          :lists.keysearch(
            appName,
            r_app_win(:name),
            r_state(s2, :app_wins)
          )

        :reltool_utils.reply(replyTo, ref, {:ok, appPid})
        :reltool_sys_win.loop(s2)

      {:EXIT, pid, reason} when pid === r_state(s, :parent_pid) ->
        for fW <- r_state(s, :fgraph_wins) do
          :reltool_fgraph_win.stop(r_fgraph_win(fW, :pid), reason)
        end

        exit(reason)

      {:EXIT, _Pid, _Reason} = exit ->
        {fWs, aWs} = handle_child_exit(exit, r_state(s, :fgraph_wins), r_state(s, :app_wins))

        :reltool_sys_win.loop(
          r_state(s,
            fgraph_wins: fWs,
            app_wins: aWs
          )
        )

      msg ->
        :error_logger.format('~w~w got unexpected message:\n\t~tp\n', [
          :reltool_sys_win,
          self(),
          msg
        ])

        :reltool_sys_win.loop(s)
    end
  end

  defp handle_child_exit({:EXIT, pid, _Reason} = exit, fWs, aWs) do
    case :lists.keymember(pid, r_fgraph_win(:pid), fWs) do
      true ->
        msg_warning(exit, :forcegraph_window)
        {:lists.keydelete(pid, r_fgraph_win(:pid), fWs), aWs}

      false ->
        case :lists.keymember(pid, r_app_win(:pid), aWs) do
          true ->
            msg_warning(exit, :application_window)
            {fWs, :lists.keydelete(pid, r_app_win(:pid), aWs)}

          false ->
            msg_warning(exit, :unknown)
            {fWs, aWs}
        end
    end
  end

  defp msg_warning({:EXIT, _Pid, :shutdown}, type)
       when type !== :unknown do
    :ok
  end

  defp msg_warning(exit, type) do
    :error_logger.format(
      '~w~w got unexpected message (~w):\n\t~tp\n',
      [:reltool_sys_win, self(), type, exit]
    )
  end

  defp create_window(s) do
    title = :lists.concat([:reltool, ' - starting up'])
    frame = :wxFrame.new(:wx.null(), -1, title, [{:size, {800, 600}}])
    bar = :wxFrame.createStatusBar(frame, [])
    :wxStatusBar.setStatusText(bar, 'Processing libraries...')
    :wxToolTip.setDelay(3000)
    panel = :wxPanel.new(frame, [])
    create_menubar(frame)
    book = :wxNotebook.new(panel, -1, [])
    s2 = r_state(s, frame: frame, panel: panel, book: book, status_bar: bar)

    s3 =
      :lists.foldl(
        fn fun, acc ->
          fun.(acc)
        end,
        s2,
        [
          &create_app_page/1,
          &create_lib_page/1,
          &create_main_release_page/1,
          &create_config_page/1
        ]
      )

    s4 = create_warning_list(s3)
    sizer = :wxBoxSizer.new(8)
    :wxSizer.add(sizer, book, [{:flag, 8192}, {:proportion, 1}])
    :wxSizer.add(sizer, r_state(s4, :warning_list), [{:flag, 8192}])
    :wxPanel.setSizer(panel, sizer)
    :wxSizer.fit(sizer, frame)
    :wxSizer.setSizeHints(sizer, frame)
    :wxFrame.connect(frame, :close_window)
    :wxFrame.show(frame)
    s4
  end

  defp create_menubar(frame) do
    menuBar = :wxMenuBar.new()
    file = :wxMenu.new([])
    help = :wxMenu.new([])
    :wxMenuBar.append(menuBar, file, 'File')
    :wxMenu.append(file, 301, 'Display application dependency graph')
    :wxMenu.append(file, 302, 'Display module dependency graph')
    :wxMenu.appendSeparator(file)
    :wxMenu.append(file, 309, 'Reset configuration to default')
    :wxMenu.append(file, 308, 'Undo configuration (toggle)')
    :wxMenu.append(file, 303, 'Load configuration')
    save = :wxMenu.new()
    :wxMenu.append(save, 304, 'Save explicit configuration  (neither defaults nor derivates)')
    :wxMenu.append(save, 306, 'Save configuration defaults  (defaults only)')
    :wxMenu.append(save, 305, 'Save configuration derivates (derivates only))')
    :wxMenu.append(save, 307, 'Save extended configuration  (both defaults and derivates)')
    :wxMenu.append(file, -1, 'Save configuration', save)
    :wxMenu.appendSeparator(file)
    :wxMenu.append(file, 310, 'Generate rel, script and boot files')
    :wxMenu.append(file, 311, 'Generate target system')
    :wxMenu.appendSeparator(file)
    :wxMenu.append(file, 5006, 'Close')
    :wxMenuBar.append(menuBar, help, 'Help')
    :wxMenu.append(help, 300, 'Contents')
    :wxMenu.append(help, 5014, 'About')
    :wxFrame.setMenuBar(frame, menuBar)
    :wxEvtHandler.connect(frame, :command_menu_selected, [{:userData, :main_window}])
    menuBar
  end

  defp create_app_page(r_state(book: book) = s) do
    panel = :wxPanel.new(book, [])
    sizer = :wxBoxSizer.new(4)
    sourceCtrl = create_app_list_ctrl(panel, sizer, 'Available', :whitelist_add, :blacklist_add)

    :wxSizer.add(
      sizer,
      :wxStaticLine.new(panel, [{:style, 8}]),
      [{:border, 2}, {:flag, 64 ||| 128 ||| 32 ||| 16 ||| 8192}]
    )

    whiteCtrl = create_app_list_ctrl(panel, sizer, 'Included', :whitelist_del, :blacklist_add)

    :wxSizer.add(
      sizer,
      :wxStaticLine.new(panel, [{:style, 8}]),
      [{:border, 2}, {:flag, 64 ||| 128 ||| 32 ||| 16 ||| 8192}]
    )

    blackCtrl = create_app_list_ctrl(panel, sizer, 'Excluded', :whitelist_add, :blacklist_del)

    :wxSizer.add(
      sizer,
      :wxStaticLine.new(panel, [{:style, 8}]),
      [{:border, 2}, {:flag, 64 ||| 128 ||| 32 ||| 16 ||| 8192}]
    )

    derivedCtrl = create_app_list_ctrl(panel, sizer, 'Derived', :whitelist_add, :blacklist_add)
    :wxPanel.setSizer(panel, sizer)
    :wxNotebook.addPage(book, panel, 'Applications', [])

    r_state(s,
      source: sourceCtrl,
      whitelist: whiteCtrl,
      blacklist: blackCtrl,
      derived: derivedCtrl
    )
  end

  defp create_app_list_ctrl(panel, outerSz, title, tick, cross) do
    width = div(:lists.max([100, 800 - 40]), 4)
    height = :lists.max([100, 600 - 100])

    listCtrl =
      :wxListCtrl.new(
        panel,
        [{:style, 32 ||| 2_147_483_648}, {:size, {width, height}}]
      )

    toolTip = 'Select application(s) or open separate application window with a double click.'
    :wxListCtrl.setToolTip(listCtrl, toolTip)
    :reltool_utils.assign_image_list(listCtrl)
    listItem = :wxListItem.new()
    :wxListItem.setAlign(listItem, 0)
    :wxListItem.setText(listItem, title)

    :wxListItem.setWidth(
      listItem,
      :reltool_utils.get_column_width(listCtrl)
    )

    :wxListCtrl.insertColumn(listCtrl, 0, listItem)
    :wxListItem.destroy(listItem)
    buttonSz = :wxBoxSizer.new(4)
    create_button(panel, buttonSz, listCtrl, title, 'wxART_TICK_MARK', tick)
    create_button(panel, buttonSz, listCtrl, title, 'wxART_CROSS_MARK', cross)
    innerSz = :wxBoxSizer.new(8)

    :wxSizer.add(innerSz, listCtrl, [
      {:border, 2},
      {:flag, 64 ||| 128 ||| 32 ||| 16 ||| 8192},
      {:proportion, 1}
    ])

    :wxSizer.add(innerSz, buttonSz, [{:flag, 8192}])
    :wxSizer.add(outerSz, innerSz, [{:flag, 8192}, {:proportion, 1}])
    :wxEvtHandler.connect(listCtrl, :size, [{:skip, true}, {:userData, :app_list_ctrl}])

    :wxEvtHandler.connect(
      listCtrl,
      :command_list_item_activated
    )

    :wxWindow.connect(listCtrl, :enter_window)
    listCtrl
  end

  defp create_button(panel, sizer, listCtrl, title, bitMapName, action) do
    bitMap = :wxArtProvider.getBitmap(bitMapName)
    button = :wxBitmapButton.new(panel, -1, bitMap, [])
    toolTip = action_to_tool_tip(title, action)
    :wxBitmapButton.setToolTip(button, toolTip)
    options = [{:userData, {:app_button, action, listCtrl}}]
    :wxEvtHandler.connect(button, :command_button_clicked, options)

    :wxSizer.add(sizer, button, [
      {:border, 2},
      {:flag, 64 ||| 128 ||| 32 ||| 16},
      {:proportion, 1}
    ])
  end

  defp action_to_tool_tip(label, action) do
    case action do
      :whitelist_add when label === 'Included' ->
        'Remove selected application(s) from whitelist.'

      :whitelist_add ->
        'Add selected application(s) to whitelist.'

      :whitelist_del ->
        'Remove selected application(s)from whitelist.'

      :blacklist_add when label === 'Excluded' ->
        'Remove selected application(s) from blacklist.'

      :blacklist_add ->
        'Add selected application(s) to blacklist.'

      :blacklist_del ->
        'Remove selected application(s) from blacklist.'
    end
  end

  defp create_lib_page(r_state(book: book) = s) do
    panel = :wxPanel.new(book, [])
    sizer = :wxBoxSizer.new(4)
    tree = :wxTreeCtrl.new(panel, [{:style, 1 ||| 2048}])
    toolTip = 'Edit application sources.'
    :wxBitmapButton.setToolTip(tree, toolTip)
    :wxFrame.connect(tree, :command_tree_item_activated)
    :wxFrame.connect(tree, :command_tree_item_right_click)

    :wxSizer.add(sizer, tree, [
      {:border, 2},
      {:flag, 64 ||| 128 ||| 32 ||| 16 ||| 8192},
      {:proportion, 1}
    ])

    :wxPanel.setSizer(panel, sizer)
    :wxNotebook.addPage(book, panel, 'Libraries', [])
    r_state(s, lib_tree: tree)
  end

  defp redraw_libs(r_state(lib_tree: tree, sys: sys) = s) do
    :wxTreeCtrl.deleteAllItems(tree)
    top = :wxTreeCtrl.addRoot(tree, 'Sources', [])

    {:ok, erts} =
      :reltool_server.get_app(
        r_state(s, :server_pid),
        :erts
      )

    append_root(tree, top, r_sys(sys, :root_dir), erts)
    libItem = :wxTreeCtrl.appendItem(tree, top, 'Library directories', [])
    libData = r_lib_data(dir: :undefined, tree: tree, item: libItem)
    :wxTreeCtrl.setItemData(tree, libItem, libData)

    for dir <- r_sys(sys, :lib_dirs) do
      append_lib(tree, libItem, dir)
    end

    escriptItem = append_item(tree, top, 'Escript files', :undefined)
    escriptData = r_escript_data(file: :undefined, tree: tree, item: escriptItem)
    :wxTreeCtrl.setItemData(tree, escriptItem, escriptData)

    for file <- r_sys(sys, :escripts) do
      append_escript(tree, escriptItem, file)
    end

    :wxTreeCtrl.expand(tree, libItem)
    :wxTreeCtrl.expand(tree, escriptItem)
    s
  end

  defp append_root(tree, parent, dir, erts) do
    top = append_item(tree, parent, 'Root directory', :undefined)
    data = r_root_data(dir: dir)
    rootItem = append_item(tree, top, dir, data)
    ertsItem = append_item(tree, rootItem, 'erts', :undefined)

    for d <- r_app(erts, :sorted_dirs) do
      append_app(tree, ertsItem, :filename.basename(:filename.dirname(d)), d)
    end

    libItem = append_item(tree, rootItem, 'lib', :undefined)
    libDir = :filename.join([dir, 'lib'])
    libDirs = :reltool_utils.lib_dirs(libDir)

    appDirs =
      :lists.sort(
        &:reltool_utils.app_dir_test/2,
        libDirs
      )

    for d <- appDirs do
      append_app(tree, libItem, d, libDir)
    end

    :wxTreeCtrl.expand(tree, top)
    rootItem
  end

  defp append_lib(tree, parent, dir) do
    item = :wxTreeCtrl.appendItem(tree, parent, dir, [])
    data = r_lib_data(dir: dir, tree: tree, item: item)
    :wxTreeCtrl.setItemData(tree, item, data)
    append_apps(tree, item, dir)
  end

  defp append_apps(tree, item, dir) do
    appDirs =
      :lists.sort(
        &:reltool_utils.app_dir_test/2,
        :reltool_utils.lib_dirs(dir)
      )

    for d <- appDirs do
      append_app(tree, item, d, dir)
    end

    item
  end

  defp append_app(tree, parent, base, dir) do
    data = r_app_data(name: base, dir: dir)
    append_item(tree, parent, base, data)
  end

  defp append_escript(tree, parent, file) do
    data = r_escript_data(file: file)
    append_item(tree, parent, file, data)
  end

  defp append_item(tree, parent, label, data) do
    item = :wxTreeCtrl.appendItem(tree, parent, label, [])
    :wxTreeCtrl.setItemData(tree, item, data)
    item
  end

  defp create_config_page(r_state(sys: sys, book: book) = s) do
    panel = :wxPanel.new(book, [])
    sizer = :wxBoxSizer.new(4)
    appConds = :reltool_utils.incl_conds()

    appBox =
      :wxRadioBox.new(panel, -1, 'Application inclusion policy', {-1, -1}, {-1, -1}, appConds, [])

    appToolTip = 'Choose default policy for inclusion of applications. '
    :wxBitmapButton.setToolTip(appBox, appToolTip)
    appChoice = :reltool_utils.incl_cond_to_index(r_sys(sys, :incl_cond))
    :wxRadioBox.setSelection(appBox, appChoice)

    :wxEvtHandler.connect(
      appBox,
      :command_radiobox_selected,
      [{:userData, :config_incl_cond}]
    )

    modConds = :reltool_utils.mod_conds()

    modBox =
      :wxRadioBox.new(panel, -1, 'Module inclusion policy', {-1, -1}, {-1, -1}, modConds, [])

    modToolTip = 'Choose default policy for module inclusion.'
    :wxBitmapButton.setToolTip(modBox, modToolTip)
    modChoice = :reltool_utils.mod_cond_to_index(r_sys(sys, :mod_cond))
    :wxRadioBox.setSelection(modBox, modChoice)

    :wxEvtHandler.connect(
      modBox,
      :command_radiobox_selected,
      [{:userData, :config_mod_cond}]
    )

    :wxSizer.add(sizer, appBox, [
      {:border, 2},
      {:flag, 64 ||| 128 ||| 32 ||| 16 ||| 8192},
      {:proportion, 1}
    ])

    :wxSizer.add(sizer, modBox, [
      {:border, 2},
      {:flag, 64 ||| 128 ||| 32 ||| 16 ||| 8192},
      {:proportion, 1}
    ])

    :wxPanel.setSizer(panel, sizer)
    :wxNotebook.addPage(book, panel, 'System settings', [])
    r_state(s, app_box: appBox, mod_box: modBox)
  end

  defp redraw_config_page(r_state(sys: sys, app_box: appBox, mod_box: modBox) = s) do
    appChoice = :reltool_utils.incl_cond_to_index(r_sys(sys, :incl_cond))
    :wxRadioBox.setSelection(appBox, appChoice)
    modChoice = :reltool_utils.mod_cond_to_index(r_sys(sys, :mod_cond))
    :wxRadioBox.setSelection(modBox, modChoice)
    s
  end

  defp create_warning_list(r_state(panel: panel) = s) do
    listCtrl =
      :wxListCtrl.new(
        panel,
        [{:style, 32 ||| 2 ||| 2_147_483_648}, {:size, {800, 80}}]
      )

    :reltool_utils.assign_image_list(listCtrl)

    :wxListCtrl.insertColumn(listCtrl, 0, 'Warnings', [
      {:format, 0},
      {:width, :reltool_utils.get_column_width(listCtrl)}
    ])

    :wxListCtrl.setToolTip(listCtrl, 'Warnings are listed in this window')
    :wxEvtHandler.connect(listCtrl, :size, [{:skip, true}, {:userData, :warnings}])

    :wxEvtHandler.connect(
      listCtrl,
      :command_list_item_activated,
      [{:userData, :warnings}]
    )

    :wxEvtHandler.connect(listCtrl, :motion, [{:userData, :warnings}])
    :wxEvtHandler.connect(listCtrl, :enter_window)
    r_state(s, warning_list: listCtrl)
  end

  defp redraw_warnings(s) do
    {:ok, warnings} = :reltool_server.get_status(r_state(s, :server_pid))
    redraw_warnings(r_state(s, :warning_list), warnings)
    length(warnings)
  end

  defp redraw_warnings(listCtrl, []) do
    :wxListCtrl.deleteAllItems(listCtrl)
    :ok
  end

  defp redraw_warnings(listCtrl, warnings) do
    :wxListCtrl.deleteAllItems(listCtrl)

    show = fn warning, row ->
      :wxListCtrl.insertItem(listCtrl, row, '')
      :wxListCtrl.setItem(listCtrl, row, 0, warning, [{:imageId, 1}])
      row + 1
    end

    :wx.foldl(show, 0, warnings)
    :ok
  end

  defp create_main_release_page(r_state(book: book) = s) do
    panel = :wxPanel.new(book, [])
    relBook = :wxNotebook.new(panel, -1, [])
    sizer = :wxBoxSizer.new(8)
    buttonSizer = :wxBoxSizer.new(4)
    create = :wxButton.new(panel, -1, [{:label, 'Create'}])
    :wxButton.setToolTip(create, 'Create a new release.')
    :wxButton.connect(create, :command_button_clicked, [{:userData, :create_rel}])
    :wxSizer.add(buttonSizer, create)
    delete = :wxButton.new(panel, -1, [{:label, 'Delete'}])
    :wxButton.setToolTip(delete, 'Delete a release.')
    :wxButton.connect(delete, :command_button_clicked, [{:userData, :delete_rel}])
    :wxSizer.add(buttonSizer, delete)
    view = :wxButton.new(panel, -1, [{:label, 'View script'}])
    :wxButton.setToolTip(view, 'View generated script file.')
    :wxButton.connect(view, :command_button_clicked, [{:userData, :view_script}])
    :wxSizer.add(buttonSizer, view)

    for rel <- r_sys(r_state(s, :sys), :rels) do
      add_release_page(relBook, rel)
    end

    :wxSizer.add(sizer, relBook, [{:flag, 8192}, {:proportion, 1}])
    :wxSizer.add(sizer, buttonSizer, [{:flag, 8192}])
    :wxPanel.setSizer(panel, sizer)
    :wxNotebook.addPage(book, panel, 'Releases', [])
    r_state(s, rel_book: relBook)
  end

  defp add_release_page(book, r_rel(name: relName, rel_apps: relApps)) do
    panel = :wxPanel.new(book, [])
    sizer = :wxBoxSizer.new(4)

    appNames = [
      [:kernel, :stdlib]
      | for rA <- relApps do
          r_rel_app(rA, :name)
        end -- [:kernel, :stdlib]
    ]

    relBox =
      :wxListBox.new(panel, -1, [
        {:pos, {-1, -1}},
        {:size, {-1, -1}},
        {:choices,
         for appName <- appNames do
           [:erlang.atom_to_list(appName)]
         end},
        {:style, 128}
      ])

    :wxEvtHandler.connect(relBox, :command_listbox_selected, [
      {:userData, {:config_rel_cond, relName}}
    ])

    relToolTip = 'Choose which applications that shall be included in the release resource file.'
    :wxBitmapButton.setToolTip(relBox, relToolTip)

    :wxSizer.add(sizer, relBox, [
      {:border, 2},
      {:flag, 64 ||| 128 ||| 32 ||| 16 ||| 8192},
      {:proportion, 1}
    ])

    :wxPanel.setSizer(panel, sizer)
    :wxNotebook.addPage(book, panel, relName, [])
  end

  defp do_open_app(s, appBase) when is_list(appBase) do
    {appName, _AppVsn} = :reltool_utils.split_app_name(appBase)
    do_open_app(s, appName)
  end

  defp do_open_app(s, :"") do
    s
  end

  defp do_open_app(
         r_state(server_pid: serverPid, common: c, app_wins: appWins) = s,
         appName
       )
       when is_atom(appName) do
    case :lists.keysearch(appName, r_app_win(:name), appWins) do
      false ->
        wxEnv = :wx.get_env()
        {:ok, pid} = :reltool_app_win.start_link(wxEnv, serverPid, c, appName)
        aW = r_app_win(name: appName, pid: pid)
        r_state(s, app_wins: [aW | appWins])

      {:value, aW} ->
        :reltool_app_win.raise(r_app_win(aW, :pid))
        s
    end
  end

  defp root_popup(s, root, tree, item) do
    popupMenu = :wxMenu.new()
    :wxMenu.append(popupMenu, 0, 'Root dir')
    :wxMenu.appendSeparator(popupMenu)
    :wxMenu.append(popupMenu, 1, 'Edit')
    choices = [:edit]
    popup = r_root_popup(dir: root, choices: choices, tree: tree, item: item)
    :wxEvtHandler.connect(popupMenu, :command_menu_selected, [{:userData, {:popup, popup}}])
    :wxWindow.popupMenu(r_state(s, :frame), popupMenu)
    s
  end

  defp lib_popup(s, lib, tree, item) do
    popupMenu = :wxMenu.new()
    :wxMenu.append(popupMenu, 0, 'Library dir')
    :wxMenu.appendSeparator(popupMenu)
    :wxMenu.append(popupMenu, 1, 'Add')

    choices =
      case :wxTreeCtrl.getItemData(tree, item) do
        r_lib_data(dir: :undefined) ->
          [:add]

        r_lib_data() ->
          :wxMenu.append(popupMenu, 2, 'Edit')
          :wxMenu.append(popupMenu, 3, 'Delete')
          [:add, :edit, :delete]
      end

    popup = r_lib_popup(dir: lib, choices: choices, tree: tree, item: item)
    :wxEvtHandler.connect(popupMenu, :command_menu_selected, [{:userData, {:popup, popup}}])
    :wxWindow.popupMenu(r_state(s, :frame), popupMenu)
    s
  end

  defp escript_popup(s, file, tree, item) do
    popupMenu = :wxMenu.new()
    :wxMenu.append(popupMenu, 0, 'Escript file')
    :wxMenu.appendSeparator(popupMenu)
    :wxMenu.append(popupMenu, 1, 'Add')

    choices =
      case :wxTreeCtrl.getItemData(tree, item) do
        r_escript_data(file: :undefined) ->
          [:add]

        r_escript_data() ->
          :wxMenu.append(popupMenu, 2, 'Edit')
          :wxMenu.append(popupMenu, 3, 'Delete')
          [:add, :edit, :delete]
      end

    popup = r_escript_popup(file: file, choices: choices, tree: tree, item: item)
    :wxEvtHandler.connect(popupMenu, :command_menu_selected, [{:userData, {:popup, popup}}])
    :wxWindow.popupMenu(r_state(s, :frame), popupMenu)
    s
  end

  defp handle_event(
         s,
         r_wx(id: id, obj: objRef, userData: userData, event: event) = _Wx
       ) do
    case event do
      _
      when userData === :warnings or
             (is_tuple(userData) and
                :erlang.element(1, userData) === :warning) ->
        handle_warning_event(s, objRef, userData, event)

      r_wxSize(type: :size, size: {w, _H})
      when userData === :app_list_ctrl ->
        :wxListCtrl.setColumnWidth(objRef, 0, w)
        s

      r_wxCommand(type: :command_menu_selected) when id === 301 ->
        update_app_graph(s)

      r_wxCommand(type: :command_menu_selected) when id === 302 ->
        update_mod_graph(s)

      r_wxCommand(type: :command_menu_selected) when id === 309 ->
        reset_config(s)

      r_wxCommand(type: :command_menu_selected) when id === 308 ->
        undo_config(s)

      r_wxCommand(type: :command_menu_selected) when id === 303 ->
        load_config(s)

      r_wxCommand(type: :command_menu_selected) when id === 304 ->
        save_config(s, false, false)

      r_wxCommand(type: :command_menu_selected) when id === 305 ->
        save_config(s, false, true)

      r_wxCommand(type: :command_menu_selected) when id === 306 ->
        save_config(s, true, false)

      r_wxCommand(type: :command_menu_selected) when id === 307 ->
        save_config(s, true, true)

      r_wxCommand(type: :command_menu_selected) when id === 310 ->
        gen_rel_files(s)

      r_wxCommand(type: :command_menu_selected) when id === 311 ->
        gen_target(s)

      r_wxCommand(type: :command_menu_selected)
      when userData === :main_window and id === 300 ->
        {:file, beamFile} = :code.is_loaded(:reltool_sys_win)
        ebinDir = :filename.dirname(beamFile)
        appDir = :filename.dirname(ebinDir)
        helpFile = :filename.join([appDir, 'doc', 'html', 'index.html'])
        url = 'file://' ++ :filename.absname(helpFile)
        :wx_misc.launchDefaultBrowser(url)
        s

      r_wxCommand(type: :command_menu_selected)
      when userData === :main_window and id === 5014 ->
        aboutStr =
          'Reltool is a release management tool. It analyses a  given Erlang/OTP installation and determines various  dependencies between applications. The graphical frontend  depicts the dependencies and enables interactive  customization of a target system. The backend provides a  batch interface for generation of customized target systems.'

        mD =
          :wxMessageDialog.new(r_state(s, :frame), aboutStr, [
            {:style, 4 ||| 2048},
            {:caption, 'About Reltool'}
          ])

        :wxMessageDialog.showModal(mD)
        :wxMessageDialog.destroy(mD)
        s

      r_wxCommand(
        type: :command_menu_selected = type,
        cmdString: str
      ) ->
        case userData do
          {:popup, popup} ->
            handle_popup_event(s, type, id, objRef, popup, str)

          true ->
            s
        end

      r_wxMouse(type: :enter_window) ->
        s

      _ ->
        case :wxNotebook.getPageText(
               r_state(s, :book),
               :wxNotebook.getSelection(r_state(s, :book))
             ) do
          'Applications' ->
            handle_app_event(s, event, objRef, userData)

          'Libraries' ->
            handle_source_event(s, event, objRef, userData)

          'System settings' ->
            handle_system_event(s, event, objRef, userData)

          'Releases' ->
            handle_release_event(s, event, objRef, userData)
        end
    end
  end

  defp handle_warning_event(s, objRef, _, r_wxSize(type: :size)) do
    columnWidth = :reltool_utils.get_column_width(objRef)
    :wxListCtrl.setColumnWidth(objRef, 0, columnWidth)
    s
  end

  defp handle_warning_event(s, objRef, _, r_wxMouse(type: :motion, x: x, y: y)) do
    pos =
      :reltool_utils.wait_for_stop_motion(
        objRef,
        {x, y}
      )

    warning_list_set_tool_tip(:os.type(), objRef, pos)
    s
  end

  defp handle_warning_event(
         s,
         objRef,
         _,
         r_wxList(
           type: :command_list_item_activated,
           itemIndex: pos
         )
       ) do
    text = :wxListCtrl.getItemText(objRef, pos)

    r_state(s,
      warning_wins: [
        display_warning(s, text)
        | r_state(s, :warning_wins)
      ]
    )
  end

  defp handle_warning_event(
         s,
         _ObjRef,
         {:warning, frame},
         r_wxCommand(type: :command_button_clicked)
       ) do
    :wxFrame.destroy(frame)

    r_state(s,
      warning_wins:
        :lists.delete(
          frame,
          r_state(s, :warning_wins)
        )
    )
  end

  defp warning_list_set_tool_tip({:win32, _}, listCtrl, {_X, y}) do
    case win_find_item(listCtrl, y, 0) do
      -1 ->
        :wxListCtrl.setToolTip(listCtrl, 'Warnings are listed in this window')

      _Index ->
        :ok
    end
  end

  defp warning_list_set_tool_tip(_, listCtrl, pos) do
    case :wxListCtrl.findItem(listCtrl, -1, pos, 0) do
      index when index >= 0 ->
        tip =
          case :wxListCtrl.getItemText(
                 listCtrl,
                 index
               ) do
            '' ->
              'Warnings are listed in this window'

            text ->
              'WARNING:\n' ++ text
          end

        :wxListCtrl.setToolTip(listCtrl, tip)

      _ ->
        :ok
    end
  end

  defp win_find_item(listCtrl, yPos, index) do
    case :wxListCtrl.getItemRect(listCtrl, index) do
      {true, {_, y, _, h}} when yPos >= y and yPos <= y + h ->
        index

      {true, _} ->
        win_find_item(listCtrl, yPos, index + 1)

      {false, _} ->
        -1
    end
  end

  defp display_warning(s, warning) do
    pos = warning_popup_position(s, {400, 150})
    frame = :wxFrame.new(:wx.null(), -1, 'Warning', [{:pos, pos}])
    panel = :wxPanel.new(frame, [])
    textStyle = 16 ||| 1 ||| 32

    text =
      :wxTextCtrl.new(panel, -1, [{:value, warning}, {:style, textStyle}, {:size, {400, 150}}])

    attr = :wxTextAttr.new()
    :wxTextAttr.setLeftIndent(attr, 10)
    :wxTextAttr.setRightIndent(attr, 10)
    true = :wxTextCtrl.setDefaultStyle(text, attr)
    :wxTextAttr.destroy(attr)
    sizer = :wxBoxSizer.new(8)

    :wxSizer.add(sizer, text, [
      {:border, 2},
      {:flag, 8192 ||| (64 ||| 128 ||| 32 ||| 16)},
      {:proportion, 1}
    ])

    close = :wxButton.new(panel, -1, [{:label, 'Close'}])
    :wxButton.setToolTip(close, 'Close window.')
    :wxButton.connect(close, :command_button_clicked, [{:userData, {:warning, frame}}])
    :wxSizer.add(sizer, close, [{:flag, 256}])
    :wxPanel.setSizer(panel, sizer)
    :wxSizer.fit(sizer, frame)
    :wxSizer.setSizeHints(sizer, frame)
    :wxFrame.connect(frame, :close_window)
    :wxFrame.show(frame)
    frame
  end

  defp warning_popup_position(r_state(frame: mF, warning_list: wL), {wFW, wFH}) do
    {mFX, mFY} = :wxWindow.getPosition(mF)
    {mFW, mFH} = :wxWindow.getSize(mF)
    {_WLW, wLH} = :wxWindow.getSize(wL)
    offset = 50
    x = mFX + div(mFW - wFW, 2) - offset
    y = mFY + div(mFH - wLH - wFH, 2) - offset
    {x, y}
  end

  defp handle_popup_event(s, _Type, 0, _ObjRef, _UserData, _Str) do
    s
  end

  defp handle_popup_event(
         r_state(sys: sys) = s,
         _Type,
         pos,
         _ObjRef,
         r_root_popup(dir: oldDir, choices: choices),
         _Str
       ) do
    case :lists.nth(pos, choices) do
      :edit ->
        style = 1 ||| 16

        case select_dir(r_state(s, :frame), 'Change root directory', oldDir, style) do
          {:ok, newDir} when newDir === oldDir ->
            s

          {:ok, newDir} ->
            sys2 = r_sys(sys, root_dir: newDir)
            do_set_sys(r_state(s, sys: sys2))

          :cancel ->
            s
        end
    end
  end

  defp handle_popup_event(
         r_state(sys: sys) = s,
         _Type,
         pos,
         _ObjRef,
         r_lib_popup(dir: oldDir, choices: choices),
         _Str
       ) do
    case :lists.nth(pos, choices) do
      :add ->
        {:ok, cwd} = :file.get_cwd()
        style = 1 ||| 16

        case select_dir(r_state(s, :frame), 'Select a library directory to add', cwd, style) do
          {:ok, newDir} ->
            case :lists.member(newDir, r_sys(sys, :lib_dirs)) do
              true ->
                s

              false ->
                libDirs = r_sys(sys, :lib_dirs) ++ [newDir]
                sys2 = r_sys(sys, lib_dirs: libDirs)
                do_set_sys(r_state(s, sys: sys2))
            end

          :cancel ->
            s
        end

      :edit ->
        style = 1 ||| 16

        case select_dir(r_state(s, :frame), 'Change library directory', oldDir, style) do
          {:ok, newDir} ->
            case :lists.member(newDir, r_sys(sys, :lib_dirs)) do
              true ->
                s

              false ->
                pred = fn e ->
                  e !== oldDir
                end

                {before, [_ | after__]} =
                  :lists.splitwith(
                    pred,
                    r_sys(sys, :lib_dirs)
                  )

                libDirs2 = before ++ [newDir | after__]
                sys2 = r_sys(sys, lib_dirs: libDirs2)
                do_set_sys(r_state(s, sys: sys2))
            end

          :cancel ->
            s
        end

      :delete ->
        libDirs = r_sys(sys, :lib_dirs) -- [oldDir]
        sys2 = r_sys(sys, lib_dirs: libDirs)
        do_set_sys(r_state(s, sys: sys2))
    end
  end

  defp handle_popup_event(
         r_state(sys: sys) = s,
         _Type,
         pos,
         _ObjRef,
         r_escript_popup(file: oldFile, choices: choices),
         _Str
       ) do
    case :lists.nth(pos, choices) do
      :add ->
        oldFile2 =
          case oldFile do
            :undefined ->
              {:ok, cwd} = :file.get_cwd()
              :filename.join([cwd, 'myEscript'])

            _ ->
              oldFile
          end

        style = 1 ||| 16

        case select_file(r_state(s, :frame), 'Select an escript file to add', oldFile2, style) do
          {:ok, newFile} ->
            case :lists.member(newFile, r_sys(sys, :escripts)) do
              true ->
                s

              false ->
                escripts = r_sys(sys, :escripts) ++ [newFile]
                sys2 = r_sys(sys, escripts: escripts)
                do_set_sys(r_state(s, sys: sys2))
            end

          :cancel ->
            s
        end

      :edit ->
        style = 1 ||| 16

        case select_file(r_state(s, :frame), 'Change escript file name', oldFile, style) do
          {:ok, newFile} ->
            case :lists.member(newFile, r_sys(sys, :escripts)) do
              true ->
                s

              false ->
                pred = fn e ->
                  e !== oldFile
                end

                {before, [_ | after__]} =
                  :lists.splitwith(
                    pred,
                    r_sys(sys, :escripts)
                  )

                escripts2 = before ++ [newFile | after__]
                sys2 = r_sys(sys, escripts: escripts2)
                do_set_sys(r_state(s, sys: sys2))
            end

          :cancel ->
            s
        end

      :delete ->
        escripts = r_sys(sys, :escripts) -- [oldFile]
        sys2 = r_sys(sys, escripts: escripts)
        do_set_sys(r_state(s, sys: sys2))
    end
  end

  defp handle_system_event(
         r_state(sys: sys) = s,
         r_wxCommand(type: :command_radiobox_selected, cmdString: choice),
         _ObjRef,
         :config_mod_cond
       ) do
    modCond = :reltool_utils.list_to_mod_cond(choice)
    sys2 = r_sys(sys, mod_cond: modCond)
    do_set_sys(r_state(s, sys: sys2))
  end

  defp handle_system_event(
         r_state(sys: sys) = s,
         r_wxCommand(type: :command_radiobox_selected, cmdString: choice),
         _ObjRef,
         :config_incl_cond
       ) do
    appCond = :reltool_utils.list_to_incl_cond(choice)
    sys2 = r_sys(sys, incl_cond: appCond)
    do_set_sys(r_state(s, sys: sys2))
  end

  defp handle_system_event(s, event, objRef, userData) do
    :error_logger.format(
      '~w~w got unexpected wx sys event to ~p with user data: ~tp\n\t ~tp\n',
      [:reltool_sys_win, self(), objRef, userData, event]
    )

    s
  end

  defp handle_release_event(s, _Event, _ObjRef, userData) do
    :io.format('Release data: ~tp\n', [userData])
    s
  end

  defp handle_source_event(
         s,
         r_wxTree(type: :command_tree_item_activated, item: item),
         objRef,
         _UserData
       ) do
    case :wxTreeCtrl.getItemData(objRef, item) do
      r_root_data(dir: _Dir) ->
        s

      r_lib_data(dir: _Dir) ->
        s

      r_escript_data(file: _File) ->
        s

      r_app_data(name: name) ->
        do_open_app(s, name)

      :undefined ->
        s
    end
  end

  defp handle_source_event(
         s,
         r_wxTree(type: :command_tree_item_right_click, item: item),
         tree,
         _UserData
       ) do
    case :wxTreeCtrl.getItemData(tree, item) do
      r_root_data(dir: dir) ->
        :wx.batch(fn ->
          root_popup(s, dir, tree, item)
        end)

      r_lib_data(dir: dir) ->
        :wx.batch(fn ->
          lib_popup(s, dir, tree, item)
        end)

      r_escript_data(file: file) ->
        :wx.batch(fn ->
          escript_popup(s, file, tree, item)
        end)

      r_app_data(name: name) ->
        :io.format('App menu: ~tp\n', [name])
        s

      :undefined ->
        s
    end
  end

  defp handle_app_event(
         s,
         r_wxList(type: :command_list_item_activated, itemIndex: pos),
         listCtrl,
         _UserData
       ) do
    appName = :wxListCtrl.getItemText(listCtrl, pos)
    do_open_app(s, appName)
  end

  defp handle_app_event(
         s,
         r_wxCommand(type: :command_button_clicked),
         _ObjRef,
         {:app_button, action, listCtrl}
       ) do
    items = :reltool_utils.get_items(listCtrl)
    handle_app_button(s, items, action)
  end

  defp handle_app_event(s, event, objRef, userData) do
    :error_logger.format(
      '~w~w got unexpected wx app event to ~p with user data: ~tp\n\t ~tp\n',
      [:reltool_sys_win, self(), objRef, userData, event]
    )

    s
  end

  defp handle_app_button(
         r_state(server_pid: serverPid, status_bar: bar, app_wins: appWins) = s,
         items,
         action
       ) do
    :wxStatusBar.setStatusText(bar, 'Processing libraries...')

    newApps =
      for item <- items do
        move_app(s, item, action)
      end

    case :reltool_server.set_apps(serverPid, newApps) do
      {:ok, _Warnings} ->
        :ok

      {:error, reason} ->
        display_message(reason, 512)
    end

    for aW <- appWins do
      :ok = :reltool_app_win.refresh(r_app_win(aW, :pid))
    end

    redraw_apps(s)
  end

  defp do_set_sys(r_state(sys: sys, server_pid: serverPid, status_bar: bar) = s) do
    :wxStatusBar.setStatusText(bar, 'Processing libraries...')
    status = :reltool_server.set_sys(serverPid, sys)
    check_and_refresh(s, status)
  end

  defp move_app(s, {_ItemNo, appBase}, action) do
    {appName, _Vsn} = :reltool_utils.split_app_name(appBase)

    {:ok, oldApp} =
      :reltool_server.get_app(
        r_state(s, :server_pid),
        appName
      )

    appCond =
      case action do
        :whitelist_add ->
          case r_app(oldApp, :incl_cond) do
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
            '~w~w got unexpected app button event: ~tp ~tp\n',
            [:reltool_sys_win, self(), action, appBase]
          )

          r_app(oldApp, :incl_cond)
      end

    r_app(oldApp, incl_cond: appCond)
  end

  defp do_set_app(
         r_state(server_pid: serverPid, status_bar: bar, app_wins: appWins) = s,
         newApp
       ) do
    :wxStatusBar.setStatusText(bar, 'Processing libraries...')
    result = :reltool_server.set_app(serverPid, newApp)

    returnApp =
      case result do
        {:ok, analysedApp, _Warnings} ->
          analysedApp

        {:error, reason} ->
          display_message(reason, 512)

          {:ok, oldApp} =
            :reltool_server.get_app(
              serverPid,
              r_app(newApp, :name)
            )

          oldApp
      end

    for aW <- appWins do
      :ok = :reltool_app_win.refresh(r_app_win(aW, :pid))
    end

    s2 = redraw_apps(s)
    {:ok, returnApp, s2}
  end

  defp redraw_apps(
         r_state(
           server_pid: serverPid,
           source: sourceCtrl,
           whitelist: whiteCtrl,
           blacklist: blackCtrl,
           derived: derivedCtrl
         ) = s
       ) do
    {:ok, sourceApps} =
      :reltool_server.get_apps(
        serverPid,
        :source
      )

    {:ok, whiteApps} =
      :reltool_server.get_apps(
        serverPid,
        :whitelist
      )

    {:ok, blackApps} =
      :reltool_server.get_apps(
        serverPid,
        :blacklist
      )

    {:ok, derivedApps} =
      :reltool_server.get_apps(
        serverPid,
        :derived
      )

    badApps = fn
      r_app(used_by_apps: usedBy) = a
      when usedBy !== [] ->
        r_app(a, status: :missing)

      a ->
        a
    end

    blackApps2 = :lists.map(badApps, blackApps)
    redraw_apps(sourceApps, sourceCtrl, 4, 1)
    whiteN = redraw_apps(whiteApps, whiteCtrl, 3, 0)
    redraw_apps(blackApps2, blackCtrl, 4, 1)
    derivedN = redraw_apps(derivedApps, derivedCtrl, 3, 0)
    warningsN = redraw_warnings(s)

    warningText =
      cond do
        warningsN == 1 ->
          'warning'

        true ->
          'warnings'
      end

    status =
      :lists.concat([
        whiteN,
        ' whitelisted modules and ',
        derivedN,
        ' derived modules, ',
        warningsN,
        ' ',
        warningText,
        '.'
      ])

    :wxStatusBar.setStatusText(r_state(s, :status_bar), status)
    s
  end

  defp redraw_apps(apps, listCtrl, okImage, errImage) do
    do_redraw_apps(listCtrl, apps, okImage, errImage)
  end

  defp do_redraw_apps(listCtrl, [], _OkImage, _ErrImage) do
    :wxListCtrl.deleteAllItems(listCtrl)
    0
  end

  defp do_redraw_apps(listCtrl, apps, okImage, errImage) do
    oldItems = :reltool_utils.get_items(listCtrl)
    :wxListCtrl.deleteAllItems(listCtrl)

    addImage = fn app ->
      case r_app(app, :status) do
        :ok ->
          {okImage, r_app(app, :label), app}

        :missing ->
          {errImage, r_app(app, :label), app}
      end
    end

    imageApps = :lists.map(addImage, apps)

    show = fn {imageId, text, app}, {row, modCount, items} ->
      :wxListCtrl.insertItem(listCtrl, row, '')

      cond do
        rem(row, 2) === 0 ->
          :wxListCtrl.setItemBackgroundColour(listCtrl, row, {240, 240, 255})

        true ->
          :ignore
      end

      :wxListCtrl.setItem(listCtrl, row, 0, text, [{:imageId, imageId}])

      n =
        length(
          for m <- r_app(app, :mods),
              r_mod(m, :is_included) === true do
            m
          end
        )

      {row + 1, modCount + n, [{row, text} | items]}
    end

    {_, n, newItems} = :wx.foldl(show, {0, 0, []}, :lists.sort(imageApps))
    :reltool_utils.select_items(listCtrl, oldItems, :lists.reverse(newItems))
    n
  end

  defp update_app_graph(s) do
    {:ok, whiteApps} =
      :reltool_server.get_apps(
        r_state(s, :server_pid),
        :whitelist
      )

    {:ok, derivedApps} =
      :reltool_server.get_apps(
        r_state(s, :server_pid),
        :derived
      )

    whiteNames =
      for a <- whiteApps do
        r_app(a, :name)
      end

    derivedNames =
      for a <- derivedApps do
        r_app(a, :name)
      end

    nodes = whiteNames ++ derivedNames

    whiteLinks =
      for a <- whiteApps,
          u <- r_app(a, :uses_apps),
          u !== r_app(a, :name),
          :lists.member(u, nodes) do
        [r_app(a, :name), u]
      end

    derivedLinks =
      for a <- derivedApps,
          u <- r_app(a, :uses_apps),
          u !== r_app(a, :name),
          :lists.member(u, nodes) do
        [r_app(a, :name), u]
      end

    links = :lists.usort(whiteLinks ++ derivedLinks)
    title = :lists.concat([:reltool, ' - application graph'])
    create_fgraph_window(s, title, nodes, links)
  end

  defp update_mod_graph(s) do
    {:ok, whiteApps} =
      :reltool_server.get_apps(
        r_state(s, :server_pid),
        :whitelist
      )

    {:ok, derivedApps} =
      :reltool_server.get_apps(
        r_state(s, :server_pid),
        :derived
      )

    whiteMods =
      :lists.usort(
        for a <- whiteApps,
            m <- r_app(a, :mods),
            r_mod(m, :is_included) === true do
          m
        end
      )

    derivedMods =
      :lists.usort(
        for a <- derivedApps,
            m <- r_app(a, :mods),
            r_mod(m, :is_included) === true do
          m
        end
      )

    whiteNames =
      for m <- whiteMods do
        r_mod(m, :name)
      end

    derivedNames =
      for m <- derivedMods do
        r_mod(m, :name)
      end

    nodes = whiteNames ++ derivedNames

    whiteLinks =
      for m <- whiteMods,
          u <- r_mod(m, :uses_mods),
          u !== r_mod(m, :name),
          :lists.member(u, nodes) do
        [r_mod(m, :name), u]
      end

    derivedLinks =
      for m <- derivedMods,
          u <- r_mod(m, :uses_mods),
          u !== r_mod(m, :name),
          :lists.member(u, nodes) do
        [r_mod(m, :name), u]
      end

    links = :lists.usort(whiteLinks ++ derivedLinks)
    title = :lists.concat([:reltool, ' - module graph'])
    create_fgraph_window(s, title, nodes, links)
  end

  defp create_fgraph_window(s, title, nodes, links) do
    frame = :wxFrame.new(:wx.null(), -1, title, [])
    :wxFrame.setSize(frame, {800, 600})
    panel = :wxPanel.new(frame, [])
    options = [{:size, {:lists.max([100, 800 - 100]), 600}}]

    {server, fgraph} =
      :reltool_fgraph_win.new(
        panel,
        options
      )

    choose = fn
      :"*MISSING*" ->
        :alternate

      _ ->
        :default
    end

    for n <- nodes do
      :reltool_fgraph_win.add_node(server, n, choose.(n))
    end

    for [from, to] <- links do
      :reltool_fgraph_win.add_link(server, {from, to})
    end

    sizer = :wxBoxSizer.new(8)
    :wxSizer.add(sizer, fgraph, [{:flag, 8192}, {:proportion, 1}])
    :wxPanel.setSizer(panel, sizer)
    :wxFrame.connect(frame, :close_window)
    :wxFrame.show(frame)
    fW = r_fgraph_win(frame: frame, pid: server)
    r_state(s, fgraph_wins: [fW | r_state(s, :fgraph_wins)])
  end

  defp reset_config(r_state(status_bar: bar) = s) do
    :wxStatusBar.setStatusText(bar, 'Processing libraries...')
    status = :reltool_server.reset_config(r_state(s, :server_pid))
    check_and_refresh(s, status)
  end

  defp undo_config(r_state(status_bar: bar) = s) do
    :wxStatusBar.setStatusText(bar, 'Processing libraries...')
    :ok = :reltool_server.undo_config(r_state(s, :server_pid))
    refresh(s)
  end

  defp load_config(r_state(status_bar: bar, config_file: oldFile) = s) do
    style = 1 ||| 16

    case select_file(
           r_state(s, :frame),
           'Select a file to load the configuration from',
           oldFile,
           style
         ) do
      {:ok, newFile} ->
        :wxStatusBar.setStatusText(bar, 'Processing libraries...')

        status =
          :reltool_server.load_config(
            r_state(s, :server_pid),
            newFile
          )

        check_and_refresh(r_state(s, config_file: newFile), status)

      :cancel ->
        s
    end
  end

  defp save_config(r_state(config_file: oldFile) = s, inclDefaults, inclDerivates) do
    style = 2 ||| 4

    case select_file(
           r_state(s, :frame),
           'Select a file to save the configuration to',
           oldFile,
           style
         ) do
      {:ok, newFile} ->
        status =
          :reltool_server.save_config(
            r_state(s, :server_pid),
            newFile,
            inclDefaults,
            inclDerivates
          )

        check_and_refresh(r_state(s, config_file: newFile), status)

      :cancel ->
        s
    end
  end

  defp gen_rel_files(r_state(status_bar: bar, target_dir: oldDir) = s) do
    style = 2 ||| 4

    case select_dir(
           r_state(s, :frame),
           'Select a directory to generate rel, script and boot files to',
           oldDir,
           style
         ) do
      {:ok, newDir} ->
        :wxStatusBar.setStatusText(bar, 'Processing libraries...')

        status =
          :reltool_server.gen_rel_files(
            r_state(s, :server_pid),
            newDir
          )

        check_and_refresh(s, status)

      :cancel ->
        s
    end
  end

  defp gen_target(r_state(status_bar: bar, target_dir: oldDir) = s) do
    style = 2 ||| 4

    case select_dir(
           r_state(s, :frame),
           'Select a directory to generate a target system to',
           oldDir,
           style
         ) do
      {:ok, newDir} ->
        :wxStatusBar.setStatusText(bar, 'Processing libraries...')

        status =
          :reltool_server.gen_target(
            r_state(s, :server_pid),
            newDir
          )

        check_and_refresh(r_state(s, target_dir: newDir), status)

      :cancel ->
        s
    end
  end

  defp select_file(frame, message, defaultFile, style) do
    dialog =
      :wxFileDialog.new(
        frame,
        [
          {:message, message},
          {:defaultDir, :filename.dirname(defaultFile)},
          {:defaultFile, :filename.basename(defaultFile)},
          {:style, style}
        ]
      )

    choice =
      case :wxMessageDialog.showModal(dialog) do
        5101 ->
          :cancel

        5100 ->
          {:ok, :wxFileDialog.getPath(dialog)}
      end

    :wxFileDialog.destroy(dialog)
    choice
  end

  defp select_dir(frame, message, defaultDir, style) do
    dialog =
      :wxDirDialog.new(
        frame,
        [{:title, message}, {:defaultPath, defaultDir}, {:style, style}]
      )

    choice =
      case :wxMessageDialog.showModal(dialog) do
        5101 ->
          :cancel

        5100 ->
          {:ok, :wxDirDialog.getPath(dialog)}
      end

    :wxDirDialog.destroy(dialog)
    choice
  end

  defp check_and_refresh(s, status) do
    case status do
      :ok ->
        true

      {:ok, _Warnings} ->
        true

      {:error, reason} when is_list(reason) ->
        display_message(reason, 512)
        false

      {:error, reason} ->
        msg = :lists.flatten(:io_lib.format('Error:\n\n~tp\n', [reason]))
        display_message(msg, 512)
        false
    end

    refresh(s)
  end

  defp refresh(s) do
    {:ok, sys} = :reltool_server.get_sys(r_state(s, :server_pid))

    for aW <- r_state(s, :app_wins) do
      :ok = :reltool_app_win.refresh(r_app_win(aW, :pid))
    end

    s2 = r_state(s, sys: sys)
    s3 = redraw_libs(s2)
    s4 = redraw_apps(s3)
    redraw_config_page(s4)
  end

  defp question_dialog(question, details) do
    parent = :wx.typeCast(:wx.null(), :wxWindow)
    dialogStyle = 64 ||| 536_870_912 ||| 2048 ||| 1024 ||| 512 ||| 4096
    dialog = :wxDialog.new(parent, -1, 'Undo dialog', [{:style, dialogStyle}])
    color = :wxWindow.getBackgroundColour(dialog)
    textStyle = 16 ||| 32 ||| 1_073_741_824
    text1 = :wxTextCtrl.new(dialog, -1, [{:style, 16 ||| 2_097_152}])
    :wxWindow.setBackgroundColour(text1, color)
    :wxTextCtrl.appendText(text1, question)
    text2 = :wxTextCtrl.new(dialog, -1, [{:size, {600, 400}}, {:style, textStyle}])
    :wxWindow.setBackgroundColour(text2, color)
    :wxTextCtrl.appendText(text2, details)
    sizer = :wxBoxSizer.new(8)
    :wxSizer.add(sizer, text1, [{:border, 2}, {:flag, 8192}])
    :wxSizer.add(sizer, text2, [{:border, 2}, {:flag, 8192}, {:proportion, 1}])

    buttSizer =
      :wxDialog.createStdDialogButtonSizer(
        dialog,
        4 ||| 16
      )

    :wxSizer.add(sizer, buttSizer, [{:border, 2}, {:flag, 8192}])
    :wxPanel.setSizer(dialog, sizer)
    :wxSizer.fit(sizer, dialog)
    :wxSizer.setSizeHints(sizer, dialog)
    answer = :wxDialog.showModal(dialog)
    :wxDialog.destroy(dialog)
    answer
  end

  defp display_message(message, icon) do
    dialog = :wxMessageDialog.new(:wx.null(), message, [{:style, 4 ||| icon}])
    :wxMessageDialog.showModal(dialog)
    :wxMessageDialog.destroy(dialog)
  end

  defp question_dialog_2(dialogLabel, strings) do
    parent = :wx.typeCast(:wx.null(), :wxWindow)
    dialogStyle = 64 ||| 536_870_912 ||| 2048 ||| 1024 ||| 512 ||| 4096
    dialog = :wxDialog.new(parent, -1, dialogLabel, [{:style, dialogStyle}])
    color = :wxWindow.getBackgroundColour(dialog)
    textStyle = 16 ||| 32 ||| 1_073_741_824
    text = :wxTextCtrl.new(dialog, -1, [{:size, {600, 400}}, {:style, textStyle}])
    :wxWindow.setBackgroundColour(text, color)
    textAttr = :wxTextAttr.new()
    add_text(text, textAttr, strings)
    sizer = :wxBoxSizer.new(8)
    :wxSizer.add(sizer, text, [{:border, 2}, {:flag, 8192}, {:proportion, 1}])

    buttSizer =
      :wxDialog.createStdDialogButtonSizer(
        dialog,
        4 ||| 16
      )

    :wxSizer.add(sizer, buttSizer, [{:border, 2}, {:flag, 8192}])
    :wxPanel.setSizer(dialog, sizer)
    :wxSizer.fit(sizer, dialog)
    :wxSizer.setSizeHints(sizer, dialog)
    answer = :wxDialog.showModal(dialog)
    :wxDialog.destroy(dialog)
    answer
  end

  defp add_text(text, attr, [{color, string} | strings]) do
    :wxTextAttr.setTextColour(attr, color)
    :wxTextCtrl.setDefaultStyle(text, attr)
    :wxTextCtrl.appendText(text, string)
    add_text(text, attr, strings)
  end

  defp add_text(_, _, []) do
    :ok
  end

  def system_continue(_Parent, _Debug, s) do
    :reltool_sys_win.loop(s)
  end

  def system_terminate(reason, _Parent, _Debug, _S) do
    exit(reason)
  end

  def system_code_change(s, _Module, _OldVsn, _Extra) do
    {:ok, s}
  end
end
