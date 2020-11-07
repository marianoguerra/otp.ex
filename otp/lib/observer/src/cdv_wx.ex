defmodule :m_cdv_wx do
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

  Record.defrecord(:r_file_info, :file_info,
    size: :undefined,
    type: :undefined,
    access: :undefined,
    atime: :undefined,
    mtime: :undefined,
    ctime: :undefined,
    mode: :undefined,
    links: :undefined,
    major_device: :undefined,
    minor_device: :undefined,
    inode: :undefined,
    uid: :undefined,
    gid: :undefined
  )

  Record.defrecord(:r_file_descriptor, :file_descriptor,
    module: :undefined,
    data: :undefined
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
    server: :undefined,
    file: :undefined,
    frame: :undefined,
    menubar: :undefined,
    menus: [],
    status_bar: :undefined,
    notebook: :undefined,
    main_panel: :undefined,
    gen_panel: :undefined,
    pro_panel: :undefined,
    port_panel: :undefined,
    ets_panel: :undefined,
    timer_panel: :undefined,
    sched_panel: :undefined,
    fun_panel: :undefined,
    atom_panel: :undefined,
    dist_panel: :undefined,
    mod_panel: :undefined,
    mem_panel: :undefined,
    persistent_panel: :undefined,
    int_panel: :undefined,
    active_tab: :undefined
  )

  def start(file) do
    case :wx_object.start(:cdv_wx, file, []) do
      err = {:error, _} ->
        err

      _Obj ->
        :ok
    end
  end

  def get_attrib(what) do
    :wx_object.call(:cdv_wx, {:get_attrib, what})
  end

  def set_status(what) do
    :wx_object.cast(:cdv_wx, {:status_bar, what})
  end

  def init(file0) do
    :erlang.register(:cdv_wx, self())
    :wx.new()
    {:ok, cdvServer} = :crashdump_viewer.start_link()

    try do
      :wxSystemOptions.setOption('mac.listctrl.always_use_generic', 1)
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end

    scale = :observer_wx.get_scale()

    frame =
      :wxFrame.new(:wx.null(), -1, 'Crashdump Viewer', [
        {:size, {scale * 850, scale * 600}},
        {:style, 2048 ||| 64 ||| 1024 ||| 512 ||| 4096 ||| 536_870_912 ||| 4_194_304}
      ])

    iconFile = :filename.join(:code.priv_dir(:observer), 'erlang_observer.png')
    icon = :wxIcon.new(iconFile, [{:type, 2 + 13}])
    :wxFrame.setIcon(frame, icon)
    :wxIcon.destroy(icon)
    panel = :wxPanel.new(frame, [])
    notebook = :wxNotebook.new(panel, 3, [{:style, 0}])
    statusBar = :observer_lib.create_status_bar(panel)
    mainSizer = :wxBoxSizer.new(8)
    :wxSizer.add(mainSizer, notebook, [{:proportion, 1}, {:flag, 8192}])

    :wxSizer.add(mainSizer, statusBar, [
      {:flag, 8192 ||| (64 ||| 128 ||| 32 ||| 16)},
      {:proportion, 0},
      {:border, 4}
    ])

    :wxPanel.setSizer(panel, mainSizer)

    :wxNotebook.connect(
      notebook,
      :command_notebook_page_changing
    )

    :wxFrame.connect(frame, :close_window, [{:skip, true}])
    :wxMenu.connect(frame, :command_menu_selected)

    case load_dump(frame, file0) do
      {:ok, file} ->
        t1 = 'Crashdump Viewer: '
        fileLength = :string.length(file)

        title =
          cond do
            fileLength > 70 ->
              t1 ++ :filename.basename(file)

            true ->
              t1 ++ file
          end

        :wxFrame.setTitle(frame, title)

        setup(
          r_state(
            server: cdvServer,
            file: file,
            frame: frame,
            status_bar: statusBar,
            notebook: notebook,
            main_panel: panel
          )
        )

      :error ->
        :wxFrame.destroy(frame)
        :wx.destroy()
        :crashdump_viewer.stop()
        :ignore
    end
  end

  defp setup(r_state(frame: frame, notebook: notebook) = state) do
    menuBar = :wxMenuBar.new()
    defMenus = default_menus()
    :observer_lib.create_menus(defMenus, menuBar, :default)
    :wxFrame.setMenuBar(frame, menuBar)
    genPanel = add_page(notebook, 'General', :cdv_info_wx, :cdv_gen_cb)
    proPanel = add_page(notebook, 'Processes', :cdv_virtual_list_wx, :cdv_proc_cb)
    portPanel = add_page(notebook, 'Ports', :cdv_virtual_list_wx, :cdv_port_cb)
    etsPanel = add_page(notebook, 'ETS Tables', :cdv_virtual_list_wx, :cdv_ets_cb)
    timerPanel = add_page(notebook, 'Timers', :cdv_virtual_list_wx, :cdv_timer_cb)
    schedPanel = add_page(notebook, 'Schedulers', :cdv_virtual_list_wx, :cdv_sched_cb)
    funPanel = add_page(notebook, 'Funs', :cdv_virtual_list_wx, :cdv_fun_cb)
    atomPanel = add_page(notebook, 'Atoms', :cdv_virtual_list_wx, :cdv_atom_cb)
    distPanel = add_page(notebook, 'Nodes', :cdv_virtual_list_wx, :cdv_dist_cb)
    modPanel = add_page(notebook, 'Modules', :cdv_virtual_list_wx, :cdv_mod_cb)
    memPanel = add_page(notebook, 'Memory', :cdv_multi_wx, :cdv_mem_cb)
    persistentPanel = add_page(notebook, 'Persistent Terms', :cdv_html_wx, :cdv_persistent_cb)
    intPanel = add_page(notebook, 'Internal Tables', :cdv_multi_wx, :cdv_int_tab_cb)
    :wxFrame.show(frame)
    genPid = :wx_object.get_pid(genPanel)
    send(genPid, :active)
    :observer_lib.destroy_progress_dialog()
    :erlang.process_flag(:trap_exit, true)

    {frame,
     r_state(state,
       menubar: menuBar,
       gen_panel: genPanel,
       pro_panel: proPanel,
       port_panel: portPanel,
       ets_panel: etsPanel,
       timer_panel: timerPanel,
       sched_panel: schedPanel,
       fun_panel: funPanel,
       atom_panel: atomPanel,
       dist_panel: distPanel,
       mod_panel: modPanel,
       mem_panel: memPanel,
       persistent_panel: persistentPanel,
       int_panel: intPanel,
       active_tab: genPid
     )}
  end

  def handle_event(
        r_wx(event: r_wxNotebook(type: :command_notebook_page_changing)),
        r_state(active_tab: previous) = state
      ) do
    case get_active_pid(state) do
      ^previous ->
        {:noreply, state}

      pid ->
        send(pid, :active)
        {:noreply, r_state(state, active_tab: pid)}
    end
  end

  def handle_event(r_wx(event: r_wxClose()), state) do
    {:stop, :normal, state}
  end

  def handle_event(
        r_wx(
          id: 5000,
          event: r_wxCommand(type: :command_menu_selected)
        ),
        state
      ) do
    newState =
      case load_dump(
             r_state(state, :frame),
             :undefined
           ) do
        {:ok, file} ->
          panels = [
            r_state(state, :gen_panel),
            r_state(state, :pro_panel),
            r_state(state, :port_panel),
            r_state(state, :ets_panel),
            r_state(state, :timer_panel),
            r_state(state, :fun_panel),
            r_state(state, :atom_panel),
            r_state(state, :dist_panel),
            r_state(state, :mod_panel),
            r_state(state, :mem_panel),
            r_state(state, :persistent_panel),
            r_state(state, :int_panel)
          ]

          _ =
            for panel <- panels do
              :wx_object.call(panel, :new_dump)
            end

          :wxNotebook.setSelection(r_state(state, :notebook), 0)
          :observer_lib.destroy_progress_dialog()
          r_state(state, file: file)

        :error ->
          state
      end

    {:noreply, newState}
  end

  def handle_event(
        r_wx(
          id: 5006,
          event: r_wxCommand(type: :command_menu_selected)
        ),
        state
      ) do
    {:stop, :normal, state}
  end

  def handle_event(
        r_wx(
          id: helpId,
          event: r_wxCommand(type: :command_menu_selected)
        ),
        state
      )
      when helpId == 5009 or helpId == 1 or helpId == 2 do
    help = get_help_doc(helpId)

    :wx_misc.launchDefaultBrowser(help) or
      create_txt_dialog(
        r_state(state, :frame),
        'Could not launch browser: ~n ' ++ help,
        'Error',
        512
      )

    {:noreply, state}
  end

  def handle_event(
        r_wx(
          id: 5014,
          event: r_wxCommand(type: :command_menu_selected)
        ),
        state = r_state(frame: frame)
      ) do
    aboutString = 'Display information from an erlang crash dump'
    style = [{:style, 4 ||| 32768}, {:caption, 'About'}]
    :wxMessageDialog.showModal(:wxMessageDialog.new(frame, aboutString, style))
    {:noreply, state}
  end

  def handle_event(event, state) do
    pid = get_active_pid(state)
    send(pid, event)
    {:noreply, state}
  end

  def handle_cast({:status_bar, msg}, state = r_state(status_bar: sB)) do
    :wxTextCtrl.clear(sB)
    :wxTextCtrl.writeText(sB, msg)
    {:noreply, state}
  end

  def handle_cast(_Cast, state) do
    {:noreply, state}
  end

  def handle_call({:get_attrib, attrib}, _From, state) do
    {:reply, :erlang.get(attrib), state}
  end

  def handle_call(_Msg, _From, state) do
    {:reply, :ok, state}
  end

  def handle_info({:EXIT, pid, :normal}, r_state(server: pid) = state) do
    {:stop, :normal, state}
  end

  def handle_info({:EXIT, pid, _Reason}, state) do
    :io.format('Child (~s) crashed exiting:  ~p ~tp~n', [pid2panel(pid, state), pid, _Reason])
    {:stop, :normal, state}
  end

  def handle_info(_Info, state) do
    {:noreply, state}
  end

  def terminate(_Reason, r_state(frame: frame)) do
    :wxFrame.destroy(frame)
    :wx.destroy()
    :crashdump_viewer.stop()
    :ok
  end

  def code_change(_, _, state) do
    {:ok, state}
  end

  defp add_page(notebook, title, callback, extra) do
    panel = callback.start_link(notebook, extra)
    :wxNotebook.addPage(notebook, panel, title, [])
    panel
  end

  def create_txt_dialog(frame, msg, title, style) do
    mD = :wxMessageDialog.new(frame, msg, [{:style, style}])
    :wxMessageDialog.setTitle(mD, title)
    :wxDialog.showModal(mD)
    :wxDialog.destroy(mD)
  end

  def check_page_title(notebook) do
    selection = :wxNotebook.getSelection(notebook)
    :wxNotebook.getPageText(notebook, selection)
  end

  defp get_active_pid(
         r_state(
           notebook: notebook,
           gen_panel: gen,
           pro_panel: pro,
           port_panel: ports,
           ets_panel: ets,
           timer_panel: timers,
           fun_panel: funs,
           atom_panel: atoms,
           dist_panel: dist,
           mod_panel: mods,
           mem_panel: mem,
           persistent_panel: persistent,
           int_panel: int,
           sched_panel: sched
         )
       ) do
    panel =
      case check_page_title(notebook) do
        'General' ->
          gen

        'Processes' ->
          pro

        'Ports' ->
          ports

        'ETS Tables' ->
          ets

        'Timers' ->
          timers

        'Schedulers' ->
          sched

        'Funs' ->
          funs

        'Atoms' ->
          atoms

        'Nodes' ->
          dist

        'Modules' ->
          mods

        'Memory' ->
          mem

        'Persistent Terms' ->
          persistent

        'Internal Tables' ->
          int
      end

    :wx_object.get_pid(panel)
  end

  defp pid2panel(
         pid,
         r_state(
           gen_panel: gen,
           pro_panel: pro,
           port_panel: ports,
           ets_panel: ets,
           timer_panel: timers,
           fun_panel: funs,
           atom_panel: atoms,
           dist_panel: dist,
           mod_panel: mods,
           mem_panel: mem,
           persistent_panel: persistent,
           int_panel: int
         )
       ) do
    case pid do
      ^gen ->
        'General'

      ^pro ->
        'Processes'

      ^ports ->
        'Ports'

      ^ets ->
        'ETS Tables'

      ^timers ->
        'Timers'

      ^funs ->
        'Funs'

      ^atoms ->
        'Atoms'

      ^dist ->
        'Nodes'

      ^mods ->
        'Modules'

      ^mem ->
        'Memory'

      'Persistent Terms' ->
        persistent

      ^int ->
        'Internal Tables'

      _ ->
        'unknown'
    end
  end

  defp default_menus() do
    open = r_create_menu(id: 5000, text: 'Open new crash dump')
    quit = r_create_menu(id: 5006, text: 'Quit')
    about = r_create_menu(id: 5014, text: 'About')
    help = r_create_menu(id: 5009)
    uG = r_create_menu(id: 1, text: 'Crashdump viewer user\'s guide')
    howto = r_create_menu(id: 2, text: 'How to interpret crash dump')

    case :os.type() === {:unix, :darwin} do
      false ->
        fileMenu = {'File', [open, quit]}
        helpMenu = {'Help', [about, help, uG, howto]}
        [fileMenu, helpMenu]

      true ->
        [{'File', [open, about, quit]}, {'&Help', [help, uG, howto]}]
    end
  end

  defp load_dump(frame, :undefined) do
    fD = :wxFileDialog.new(:wx.null(), [{:style, 1 ||| 16}])

    case :wxFileDialog.showModal(fD) do
      5100 ->
        path = :wxFileDialog.getPath(fD)
        :wxDialog.destroy(fD)
        load_dump(frame, path)

      _ ->
        :wxDialog.destroy(fD)
        :error
    end
  end

  defp load_dump(frame, fileName) do
    case maybe_warn_filename(fileName) do
      :continue ->
        do_load_dump(frame, fileName)

      :stop ->
        :error
    end
  end

  defp do_load_dump(frame, fileName) do
    :ok =
      :observer_lib.display_progress_dialog(:wx.null(), 'Crashdump Viewer', 'Loading crashdump')

    :crashdump_viewer.read_file(fileName)

    case :observer_lib.wait_for_progress() do
      :ok ->
        t1 = 'Crashdump Viewer: '
        fileLength = :string.length(fileName)

        title =
          cond do
            fileLength > 70 ->
              t1 ++ :filename.basename(fileName)

            true ->
              t1 ++ fileName
          end

        :wxFrame.setTitle(frame, title)
        {:ok, fileName}

      :error ->
        :error
    end
  end

  defp maybe_warn_filename(fileName) do
    case :os.getenv('ERL_CRASH_DUMP_SECONDS') == '0' or :os.getenv('ERL_CRASH_DUMP_BYTES') == '0' do
      true ->
        :continue

      false ->
        dumpName = :filename.absname(:os.getenv('ERL_CRASH_DUMP', 'erl_crash.dump'))

        case :filename.absname(fileName) do
          ^dumpName ->
            warning =
              'WARNING: the current crashdump might be overwritten if the crashdump_viewer node crashes.\n\nRenaming the file before inspecting it will remove the problem.\n\nDo you want to continue?'

            case :observer_lib.display_yes_no_dialog(warning) do
              5103 ->
                :continue

              5104 ->
                :stop
            end

          _ ->
            :continue
        end
    end
  end

  defp get_help_doc(helpId) do
    internal = get_internal_help_doc(helpId)

    case :filelib.is_file(internal) do
      true ->
        internal

      false ->
        get_external_help_doc(helpId)
    end
  end

  defp get_internal_help_doc(2) do
    :filename.join(erts_doc_dir(), help_file(2))
  end

  defp get_internal_help_doc(helpId) do
    :filename.join(observer_doc_dir(), help_file(helpId))
  end

  defp get_external_help_doc(2) do
    :filename.join('http://www.erlang.org/doc/apps/erts', help_file(2))
  end

  defp get_external_help_doc(helpId) do
    :filename.join('http://www.erlang.org/doc/apps/observer', help_file(helpId))
  end

  defp observer_doc_dir() do
    :filename.join([:code.lib_dir(:observer), 'doc', 'html'])
  end

  defp erts_doc_dir() do
    ertsVsn = :erlang.system_info(:version)
    rootDir = :code.root_dir()
    vsnErtsDir = :filename.join(rootDir, 'erts-' ++ ertsVsn)
    docDir = :filename.join(['doc', 'html'])

    case :filelib.is_dir(vsnErtsDir) do
      true ->
        :filename.join(vsnErtsDir, docDir)

      false ->
        :filename.join([rootDir, 'erts', docDir])
    end
  end

  defp help_file(5009) do
    'crashdump_help.html'
  end

  defp help_file(1) do
    'crashdump_ug.html'
  end

  defp help_file(2) do
    'crash_dump.html'
  end
end
