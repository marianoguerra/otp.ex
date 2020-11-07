defmodule :m_reltool_mod_win do
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
    rel_pid: :undefined,
    mod_wins: :undefined,
    sys: :undefined,
    common: :undefined,
    mod: :undefined,
    name: :undefined,
    frame: :undefined,
    panel: :undefined,
    book: :undefined,
    status_bar: :undefined,
    deps_used_by_ctrl: :undefined,
    deps_uses_ctrl: :undefined,
    popup_menu: :undefined,
    active_page: :undefined,
    code_pages: :undefined
  )

  Record.defrecord(:r_code_page, :code_page,
    name: :undefined,
    editor: :undefined,
    find_objs: :undefined,
    find_data: :undefined
  )

  Record.defrecord(:r_find_objs, :find_objs,
    search: :undefined,
    goto: :undefined,
    radio: :undefined
  )

  Record.defrecord(:r_find_data, :find_data,
    start: :undefined,
    found: :undefined,
    history: :undefined
  )

  def start_link(wxEnv, xref, relPid, common, modName) do
    :proc_lib.start_link(
      :reltool_mod_win,
      :init,
      [self(), wxEnv, xref, relPid, common, modName],
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

  def init(parent, wxEnv, xref, relPid, c, modName) do
    try do
      do_init(parent, wxEnv, xref, relPid, c, modName)
    catch
      :error, reason ->
        exit({reason, __STACKTRACE__})
    end
  end

  defp do_init(parent, wxEnv, xref, relPid, c, modName) do
    :erlang.process_flag(:trap_exit, r_common(c, :trap_exit))
    {:ok, mod} = :reltool_server.get_mod(xref, modName)
    {:ok, sys} = :reltool_server.get_sys(xref)

    s =
      r_state(
        parent_pid: parent,
        xref_pid: xref,
        rel_pid: relPid,
        sys: sys,
        mod: mod,
        name: :erlang.atom_to_list(r_mod(mod, :name)),
        common: c
      )

    :proc_lib.init_ack(parent, {:ok, self()})
    :wx.set_env(wxEnv)
    :wx.debug(r_common(c, :wx_debug))

    s2 =
      :wx.batch(fn ->
        create_window(s)
      end)

    loop(s2)
  end

  def loop(r_state(xref_pid: xref, common: c, mod: mod) = s) do
    receive do
      msg ->
        case msg do
          {:system, from, sysMsg} ->
            dbg = r_common(c, :sys_debug)

            :sys.handle_system_msg(
              sysMsg,
              from,
              r_state(s, :parent_pid),
              :reltool_mod_win,
              dbg,
              s
            )

          {:cast, _From, :raise} ->
            :wxFrame.raise(r_state(s, :frame))
            :wxFrame.setFocus(r_state(s, :frame))
            :reltool_mod_win.loop(s)

          {:cast, _From, :refresh} ->
            case :reltool_server.get_mod(xref, r_mod(mod, :name)) do
              {:ok, mod2} ->
                {:ok, sys} = :reltool_server.get_sys(xref)
                s2 = redraw_window(r_state(s, sys: sys, mod: mod2))
                :reltool_mod_win.loop(s2)

              {:error, _} ->
                :wxFrame.destroy(r_state(s, :frame))
                exit(:shutdown)
            end

          {:EXIT, pid, reason} when pid === r_state(s, :parent_pid) ->
            exit(reason)

          r_wx(event: r_wxSize()) = wx ->
            wx2 = :reltool_utils.get_latest_resize(wx)
            s2 = handle_event(s, wx2)
            :reltool_mod_win.loop(s2)

          r_wx(obj: objRef, event: r_wxClose(type: :close_window)) ->
            :wxFrame.destroy(objRef)
            exit(:shutdown)

          r_wx() = wx ->
            s2 = handle_event(s, wx)
            :reltool_mod_win.loop(s2)

          _ ->
            :error_logger.format('~w~w got unexpected message:\n\t~tp\n', [
              :reltool_mod_win,
              self(),
              msg
            ])

            :reltool_mod_win.loop(s)
        end
    end
  end

  defp create_window(r_state(mod: mod, name: modStr) = s) do
    title =
      :erlang.atom_to_list(:reltool) ++
        ' - ' ++ :erlang.atom_to_list(r_mod(mod, :app_name)) ++ ' - ' ++ modStr ++ '.erl'

    frame = :wxFrame.new(:wx.null(), -1, title, [])
    panel = :wxPanel.new(frame, [])
    statusBar = :wxFrame.createStatusBar(frame, [])
    book = :wxNotebook.new(panel, -1, [])
    s2 = r_state(s, frame: frame, panel: panel, book: book, status_bar: statusBar, code_pages: [])
    s3 = create_deps_page(s2)
    s4 = create_code_page(s3, 'Code')
    s5 = create_config_page(s4)
    :wxNotebook.setSelection(book, 0)
    sizer = :wxBoxSizer.new(8)
    :wxSizer.add(sizer, book, [{:flag, 8192}, {:proportion, 1}])
    :wxPanel.setSizer(panel, sizer)
    :wxSizer.fit(sizer, frame)
    :wxSizer.setSizeHints(sizer, frame)
    :wxEvtHandler.connect(book, :command_notebook_page_changed, [{:skip, true}])
    :wxFrame.connect(frame, :close_window)
    :wxFrame.show(frame)
    s5
  end

  defp create_deps_page(s) do
    panel = :wxPanel.new(r_state(s, :book), [])
    main = :wxBoxSizer.new(4)

    usedByCtrl =
      create_mods_list_ctrl(panel, main, 'Modules using this', ' and their applications')

    :wxSizer.add(
      main,
      :wxStaticLine.new(panel, [{:style, 8}]),
      [{:border, 2}, {:flag, 64 ||| 128 ||| 32 ||| 16 ||| 8192}]
    )

    usesCtrl = create_mods_list_ctrl(panel, main, 'Used modules', ' and their applications')

    s2 =
      r_state(s,
        deps_used_by_ctrl: usedByCtrl,
        deps_uses_ctrl: usesCtrl
      )

    redraw_mods(s2)
    :wxPanel.setSizer(panel, main)
    :wxNotebook.addPage(r_state(s2, :book), panel, 'Dependencies', [])
    s2
  end

  defp create_mods_list_ctrl(panel, sizer, modText, appText) do
    width = div(:lists.max([100, 800 - 40]), 2)
    height = :lists.max([100, 600 - 100])

    listCtrl =
      :wxListCtrl.new(
        panel,
        [{:style, 32 ||| 8192 ||| 1_073_741_824 ||| 2_147_483_648}, {:size, {width, height}}]
      )

    :reltool_utils.assign_image_list(listCtrl)
    listItem = :wxListItem.new()
    :wxListItem.setAlign(listItem, 0)
    :wxListItem.setText(listItem, modText)
    :wxListCtrl.insertColumn(listCtrl, 0, listItem)
    :wxListItem.setText(listItem, appText)
    :wxListCtrl.insertColumn(listCtrl, 1, listItem)
    :wxListItem.destroy(listItem)
    :wxEvtHandler.connect(listCtrl, :size, [{:skip, true}, {:userData, :mods_list_ctrl}])

    :wxListCtrl.connect(
      listCtrl,
      :command_list_item_activated,
      [{:userData, :open_app}]
    )

    :wxWindow.connect(listCtrl, :enter_window)

    :wxSizer.add(sizer, listCtrl, [
      {:border, 2},
      {:flag, 64 ||| 128 ||| 32 ||| 16 ||| 8192},
      {:proportion, 1}
    ])

    listCtrl
  end

  defp create_code_page(
         r_state(book: book, code_pages: pages, name: modStr) = s,
         pageName
       ) do
    case find_page(s, pageName) do
      :not_found ->
        page = do_create_code_page(s, pageName)
        pages2 = pages ++ [page]
        pos = length(pages2)
        :wxNotebook.setSelection(book, pos)

        case find_page(s, 'Code') do
          :not_found ->
            :ignore

          {:found, _, codePos} ->
            :wxNotebook.setPageText(book, codePos, modStr)
        end

        r_state(s, active_page: page, code_pages: pages2)

      {:found, page, pos} ->
        :wxNotebook.setSelection(book, pos)
        r_state(s, active_page: page)
    end
  end

  defp find_page(s, pageName) do
    find_page(r_state(s, :code_pages), pageName, 1)
  end

  defp find_page([page | pages], pageName, pos) do
    case r_code_page(page, :name) === pageName do
      true ->
        {:found, page, pos}

      false ->
        find_page(pages, pageName, pos + 1)
    end
  end

  defp find_page([], _PageName, _Pos) do
    :not_found
  end

  defp do_create_code_page(r_state(xref_pid: xref, mod: m) = s, pageName) do
    panel = :wxPanel.new(r_state(s, :book), [])
    editor = create_editor(panel)
    toolTip = 'Double click on a function call to search the function definition.'
    :wxBitmapButton.setToolTip(editor, toolTip)
    {objs, data, searchSz} = create_search_area(panel)

    {:ok, app} =
      :reltool_server.get_app(
        xref,
        r_mod(m, :app_name)
      )

    erlBin =
      case r_app(app, :is_escript) do
        false ->
          find_regular_bin(app, m)

        _ ->
          find_escript_bin(app, m)
      end

    load_code(editor, erlBin)
    sizer = :wxBoxSizer.new(8)
    :wxSizer.add(sizer, editor, [{:flag, 8192}, {:proportion, 1}])
    :wxSizer.add(sizer, searchSz, [{:flag, 8192}])
    :wxPanel.setSizer(panel, sizer)
    :wxNotebook.addPage(r_state(s, :book), panel, pageName, [])
    r_code_page(name: pageName, editor: editor, find_objs: objs, find_data: data)
  end

  defp find_regular_bin(app, mod) do
    activeDir = r_app(app, :active_dir)
    srcDir = :filename.join([activeDir, 'src'])
    modStr = :erlang.atom_to_list(r_mod(mod, :name))
    base = '^' ++ modStr ++ '\\.erl$'

    find = fn f, _Acc ->
      throw({:file.read_file(f), :epp.read_encoding(f)})
    end

    case (try do
            :filelib.fold_files(srcDir, base, true, find, {:error, :enoent})
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {{:ok, bin}, encoding0} ->
        encoding =
          case encoding0 do
            :none ->
              :epp.default_encoding()

            _ ->
              encoding0
          end

        :unicode.characters_to_binary(bin, encoding, :utf8)

      {:error, :enoent} ->
        beamFile = :filename.join([activeDir, 'ebin', modStr ++ '.beam'])

        case source_from_beam(beamFile) do
          {:ok, source} ->
            source

          :error ->
            :unicode.characters_to_binary([
              '%% Bad luck, cannot find any debug info in the file "',
              beamFile
            ])
        end
    end
  end

  defp source_from_beam(beam) do
    case :beam_lib.chunks(beam, [:abstract_code]) do
      {:ok, {_, [{:abstract_code, {_, aC}}]}} ->
        ioList =
          for f <- aC do
            :erl_pp.form(f, [{:encoding, :utf8}])
          end

        {:ok, :unicode.characters_to_binary(ioList)}

      _ ->
        :error
    end
  end

  defp find_escript_bin(r_app(active_dir: activeDir), mod) do
    notFound = false
    modName = r_mod(mod, :name)

    {fun, escript} =
      case :filelib.is_regular(activeDir) do
        true ->
          {fn fullName, _GetInfo, getBin, acc ->
             case :filename.split(fullName) do
               [_] ->
                 bin = getBin.()

                 case :beam_lib.version(bin) do
                   {:ok, {m, _}}
                   when m === modName or
                          fullName === '.' ->
                     case source_from_beam(bin) do
                       {:ok, source} ->
                         {:obj, source}

                       :error ->
                         acc
                     end

                   _ ->
                     acc
                 end

               _ ->
                 acc
             end
           end, activeDir}

        false ->
          ext = :code.objfile_extension()
          srcFile = :lists.concat([modName, '.erl'])
          objFile = :lists.concat([modName, ext])

          {fn fullName, _GetInfo, getBin, acc ->
             :io.format('', [])

             case :filename.split(fullName) do
               [_AppName, 'ebin', f]
               when f === objFile and
                      acc === notFound ->
                 case source_from_beam(getBin.()) do
                   {:ok, source} ->
                     {:obj, source}

                   _ ->
                     acc
                 end

               [_AppName, 'src', f] when f === srcFile ->
                 {:text, getBin.()}

               _ ->
                 acc
             end
           end, :filename.dirname(activeDir)}
      end

    try do
      case :reltool_utils.escript_foldl(fun, notFound, escript) do
        {:ok, {:text, bin}} ->
          bin

        {:ok, {:obj, bin}} ->
          bin

        _ ->
          :unicode.characters_to_binary([
            '%% Bad luck, cannot find the code in the escript ',
            escript,
            '.'
          ])
      end
    catch
      reason when is_list(reason) ->
        :unicode.characters_to_binary([
          '%% Bad luck, cannot find the code in the escript ',
          escript,
          ': ',
          reason
        ])
    end
  end

  defp create_config_page(s) do
    s
  end

  defp handle_event(r_state(xref_pid: xref) = s, wx) do
    case wx do
      r_wx(obj: listCtrl, userData: :mods_list_ctrl, event: r_wxSize(type: :size, size: {w, _H})) ->
        :wxListCtrl.setColumnWidth(listCtrl, 0, div(2 * w, 3))
        :wxListCtrl.setColumnWidth(listCtrl, 1, div(w, 3))
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
        modStr = :wxListCtrl.getItemText(listCtrl, pos)
        modName = :erlang.list_to_atom(modStr)
        {:ok, mod} = :reltool_server.get_mod(xref, modName)
        s2 = create_code_page(r_state(s, mod: mod), modStr)
        find_regexp_forward(s2, r_state(s2, :name) ++ ':')

      r_wx(obj: editor, event: r_wxStyledText(type: :stc_doubleclick)) ->
        goto_function(s, editor)

      r_wx(
        id: 413,
        event: r_wxCommand(type: :command_text_enter, cmdString: str)
      ) ->
        find_string(s, str)

      r_wx(
        id: 414,
        event: r_wxCommand(type: :command_text_enter, cmdString: str)
      ) ->
        goto_line(s, str)

      r_wx(event: r_wxNotebook(type: :command_notebook_page_changed)) ->
        case :wxNotebook.getSelection(r_state(s, :book)) do
          0 ->
            s

          n ->
            page = :lists.nth(n, r_state(s, :code_pages))
            r_state(s, active_page: page)
        end

      r_wx(
        event: r_wxCommand(type: :command_button_clicked),
        userData: :history_back
      ) ->
        goto_back(s)

      r_wx(obj: objRef, event: r_wxMouse(type: :enter_window)) ->
        :wxWindow.setFocus(objRef)
        s

      _ ->
        :error_logger.format('~w~w got unexpected mod event from wx:\n\t~tp\n', [
          :reltool_mod_win,
          self(),
          wx
        ])

        s
    end
  end

  defp redraw_mods(
         r_state(
           xref_pid: xref,
           deps_used_by_ctrl: usedByCtrl,
           deps_uses_ctrl: usesCtrl,
           mod:
             r_mod(
               is_pre_included: isPre,
               is_included: isIncl,
               uses_mods: usesModNames,
               used_by_mods: usedByModNames
             ),
           status_bar: bar
         )
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
        ' uses ',
        length(usesModNames),
        ' modules and ',
        ' is used by ',
        length(usedByModNames),
        ' modules.'
      ])

    :wxStatusBar.setStatusText(bar, status)

    usesMods =
      for m <- usesModNames do
        select_image(xref, m)
      end

    usedByMods =
      for m <- usedByModNames do
        select_image(xref, m)
      end

    redraw_mods(usedByCtrl, usedByMods)
    redraw_mods(usesCtrl, usesMods)
  end

  defp select_image(xref, modName) do
    {:ok, m} = :reltool_server.get_mod(xref, modName)

    image =
      case r_mod(m, :is_included) do
        _ when r_mod(m, :app_name) === :"*MISSING*" ->
          0

        true ->
          3

        false ->
          1

        :undefined ->
          0
      end

    {image, r_mod(m, :app_name), m}
  end

  defp redraw_mods(listCtrl, []) do
    :wxListCtrl.deleteAllItems(listCtrl)
  end

  defp redraw_mods(listCtrl, imageMods) do
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
      :wxListCtrl.setItem(listCtrl, row, 1, :erlang.atom_to_list(appName), [{:imageId, imageId}])
      row + 1
    end

    :wx.foldl(add, 0, :lists.sort(imageMods))
  end

  defp redraw_config(s) do
    s
  end

  defp redraw_window(s) do
    redraw_config(s)
    redraw_mods(s)
    s
  end

  defp goto_line(r_state(active_page: p) = s, lineNo)
       when is_integer(lineNo) do
    editor = r_code_page(p, :editor)
    :wxStyledTextCtrl.gotoLine(editor, lineNo)
    left = :wxStyledTextCtrl.getCurrentPos(editor)

    right =
      :wxStyledTextCtrl.getLineEndPosition(
        editor,
        lineNo
      )

    :wxStyledTextCtrl.setSelection(editor, left, right)
    s
  end

  defp goto_line(r_state(active_page: p) = s, str) when is_list(str) do
    try do
      lineNo = :erlang.list_to_integer(str)
      currentPos = :wxStyledTextCtrl.getCurrentPos(r_code_page(p, :editor))
      s2 = add_pos_to_history(s, currentPos)
      goto_line(s2, lineNo - 1)
    catch
      _, _ ->
        :wxStatusBar.setStatusText(r_state(s, :status_bar), 'Not a line number')
        s
    end
  end

  defp find_string(s, str) do
    find_string(s, str, 0)
  end

  defp find_regexp_forward(s, str) do
    s2 = find_string(s, str, 2_097_152)
    textCtrl = r_find_objs(r_code_page(r_state(s2, :active_page), :find_objs), :search)
    :wxTextCtrl.setValue(textCtrl, str)
    s2
  end

  defp find_string(
         r_state(
           active_page:
             r_code_page(
               editor: editor,
               find_objs: r_find_objs(radio: {nextO, _, caseO}),
               find_data: r_find_data(found: found) = data
             ) = p
         ) = s,
         str,
         flag
       ) do
    :wxStyledTextCtrl.hideSelection(editor, true)

    dir =
      :erlang.xor(
        :wxRadioButton.getValue(nextO),
        :wx_misc.getKeyState(306)
      )

    case__ = :wxCheckBox.getValue(caseO)

    pos =
      cond do
        found and dir ->
          :wxStyledTextCtrl.getAnchor(editor)

        found ->
          :wxStyledTextCtrl.getCurrentPos(editor)

        dir ->
          0

        true ->
          :wxStyledTextCtrl.getLength(editor)
      end

    :wxStyledTextCtrl.gotoPos(editor, pos)
    :wxStyledTextCtrl.searchAnchor(editor)

    flag2 =
      cond do
        case__ ->
          flag ||| 4

        true ->
          flag
      end

    res =
      cond do
        dir ->
          :wxStyledTextCtrl.searchNext(editor, flag2, str)

        true ->
          :wxStyledTextCtrl.searchPrev(editor, flag2, str)
      end

    found2 =
      case res >= 0 do
        true ->
          :wxStyledTextCtrl.hideSelection(editor, false)
          lineNo = :wxStyledTextCtrl.lineFromPosition(editor, res)
          :wxStyledTextCtrl.scrollToLine(editor, lineNo - 3)
          :wxStatusBar.setStatusText(r_state(s, :status_bar), '')
          true

        false ->
          :wxStatusBar.setStatusText(
            r_state(s, :status_bar),
            'Not found (Hit Enter to wrap search)'
          )

          false
      end

    p2 = r_code_page(p, find_data: r_find_data(data, found: found2))

    pages =
      :lists.keystore(r_code_page(p, :name), r_code_page(:name), r_state(s, :code_pages), p2)

    r_state(s, active_page: p2, code_pages: pages)
  end

  defp goto_function(s, editor) do
    :wxStyledTextCtrl.hideSelection(editor, false)
    currentPos = :wxStyledTextCtrl.getCurrentPos(editor)
    left = :wxStyledTextCtrl.wordStartPosition(editor, currentPos, true)
    right = :wxStyledTextCtrl.wordEndPosition(editor, currentPos, true)
    colonPos = left - 1

    left2 =
      case :wxStyledTextCtrl.getCharAt(
             editor,
             colonPos
           ) do
        ?: ->
          :wxStyledTextCtrl.wordStartPosition(editor, colonPos, true)

        _ ->
          left
      end

    right2 =
      case :wxStyledTextCtrl.getCharAt(
             editor,
             right
           ) do
        ?: ->
          :wxStyledTextCtrl.wordEndPosition(editor, right + 1, true)

        _ ->
          right
      end

    case [:wxStyledTextCtrl.getCharAt(editor, right2)] do
      '(' ->
        :wxStyledTextCtrl.setSelection(editor, left2, right2)
        text = :wxStyledTextCtrl.getSelectedText(editor)
        s2 = add_pos_to_history(s, currentPos)
        do_goto_function(s2, :string.lexemes(text, ':'))

      _ ->
        :wxStyledTextCtrl.hideSelection(editor, false)
        :wxStyledTextCtrl.setSelection(editor, left2, right2)
        s
    end
  end

  defp do_goto_function(s, []) do
    s
  end

  defp do_goto_function(r_state(active_page: p) = s, [funName]) do
    :wxStyledTextCtrl.gotoPos(r_code_page(p, :editor), 1)
    find_regexp_forward(s, '^' ++ funName ++ '(')
  end

  defp do_goto_function(s, [modStr, funStr]) do
    case :reltool_server.get_mod(
           r_state(s, :xref_pid),
           :erlang.list_to_atom(modStr)
         ) do
      {:ok, mod} when r_mod(mod, :app_name) !== :"*MISSING*" ->
        s2 = create_code_page(r_state(s, mod: mod), modStr)
        find_regexp_forward(s2, '^' ++ funStr ++ '(')

      {:ok, _} ->
        :wxStatusBar.setStatusText(
          r_state(s, :status_bar),
          'No such module: ' ++ modStr
        )

        s
    end
  end

  defp goto_back(
         r_state(
           active_page:
             r_code_page(
               editor: editor,
               find_data: data
             ) = page,
           code_pages: pages
         ) = s
       ) do
    case r_find_data(data, :history) do
      [prevPos | history] ->
        lineNo =
          :wxStyledTextCtrl.lineFromPosition(
            editor,
            prevPos
          )

        data2 = r_find_data(data, history: history)
        page2 = r_code_page(page, find_data: data2)
        pages2 = :lists.keystore(r_code_page(page2, :name), r_code_page(:name), pages, page2)

        goto_line(
          r_state(s, active_page: page2, code_pages: pages2),
          lineNo
        )

      [] ->
        :wxStatusBar.setStatusText(r_state(s, :status_bar), 'No history')
        s
    end
  end

  defp add_pos_to_history(
         r_state(active_page: page, code_pages: pages) = s,
         currentPos
       ) do
    data = r_code_page(page, :find_data)

    data2 =
      r_find_data(data,
        history: [
          currentPos
          | r_find_data(data, :history)
        ]
      )

    page2 = r_code_page(page, find_data: data2)
    pages2 = :lists.keystore(r_code_page(page2, :name), r_code_page(:name), pages, page2)
    r_state(s, active_page: page2, code_pages: pages2)
  end

  defp create_editor(parent) do
    fixedFont = :wxFont.new(10, 76, 90, 90, [])
    ed = :wxStyledTextCtrl.new(parent)
    :wxStyledTextCtrl.styleClearAll(ed)
    :wxStyledTextCtrl.styleSetFont(ed, 32, fixedFont)
    :wxStyledTextCtrl.setLexer(ed, 53)
    :wxStyledTextCtrl.setMarginType(ed, 0, 1)
    lW = :wxStyledTextCtrl.textWidth(ed, 33, '9')
    :wxStyledTextCtrl.setMarginWidth(ed, 0, lW)
    :wxStyledTextCtrl.setSelectionMode(ed, 2)

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

    :lists.foreach(
      fn style ->
        setStyle.(style)
      end,
      styles
    )

    :wxStyledTextCtrl.setKeyWords(ed, 0, keyWords())
    :wxStyledTextCtrl.markerDefine(ed, 0, 0, [{:foreground, {170, 20, 20}}])
    :wxStyledTextCtrl.markerDefine(ed, 0, 0, [{:background, {200, 120, 120}}])
    :wxStyledTextCtrl.markerDefine(ed, 1, 0, [{:foreground, {20, 20, 170}}])
    :wxStyledTextCtrl.markerDefine(ed, 1, 0, [{:background, {120, 120, 200}}])
    :wxStyledTextCtrl.markerDefine(ed, 2, 2, [{:foreground, {20, 170, 20}}])
    :wxStyledTextCtrl.markerDefine(ed, 2, 2, [{:background, {200, 255, 200}}])
    :wxStyledTextCtrl.markerDefine(ed, 3, 22, [{:background, {200, 255, 200}}])
    policy = 1 ||| 16 ||| 8
    :wxStyledTextCtrl.setYCaretPolicy(ed, policy, 3)
    :wxStyledTextCtrl.setVisiblePolicy(ed, policy, 3)
    :wxStyledTextCtrl.connect(ed, :stc_doubleclick)
    :wxWindow.connect(ed, :enter_window)
    :wxStyledTextCtrl.setReadOnly(ed, true)
    ed
  end

  defp create_search_area(parent) do
    sizer = :wxBoxSizer.new(4)
    :wxSizer.add(sizer, :wxStaticText.new(parent, -1, 'Find:'), [{:flag, 2048}])
    tC1 = :wxTextCtrl.new(parent, 413, [{:style, 1024}])
    :wxSizer.add(sizer, tC1, [{:proportion, 3}, {:flag, 8192}])
    nbtn = :wxRadioButton.new(parent, -1, 'Next')
    :wxRadioButton.setValue(nbtn, true)
    :wxSizer.add(sizer, nbtn, [{:flag, 2048}])
    pbtn = :wxRadioButton.new(parent, -1, 'Previous')
    :wxSizer.add(sizer, pbtn, [{:flag, 2048}])
    cbtn = :wxCheckBox.new(parent, -1, 'Match Case')
    :wxSizer.add(sizer, cbtn, [{:flag, 2048}])
    :wxSizer.add(sizer, 15, 15, [{:proportion, 1}, {:flag, 8192}])
    :wxSizer.add(sizer, :wxStaticText.new(parent, -1, 'Goto Line:'), [{:flag, 2048}])
    tC2 = :wxTextCtrl.new(parent, 414, [{:style, 1024}])
    :wxSizer.add(sizer, tC2, [{:proportion, 0}, {:flag, 8192}])
    button = :wxButton.new(parent, -1, [{:label, 'Back'}])
    :wxSizer.add(sizer, button, [])
    :wxEvtHandler.connect(button, :command_button_clicked, [{:userData, :history_back}])
    :wxTextCtrl.connect(tC1, :command_text_enter)
    :wxTextCtrl.connect(tC2, :command_text_enter)
    :wxWindow.connect(parent, :command_button_clicked)

    {r_find_objs(search: tC1, goto: tC2, radio: {nbtn, pbtn, cbtn}),
     r_find_data(start: 0, found: false, history: []), sizer}
  end

  defp load_code(ed, code) when is_binary(code) do
    :wxStyledTextCtrl.setReadOnly(ed, false)

    :wxStyledTextCtrl.setTextRaw(
      ed,
      <<code::binary, 0::size(8)>>
    )

    lines = :wxStyledTextCtrl.getLineCount(ed)
    sz = trunc(:math.log10(lines)) + 1
    lW = :wxStyledTextCtrl.textWidth(ed, 33, :lists.duplicate(sz, ?9))
    :wxStyledTextCtrl.setMarginWidth(ed, 0, lW + 5)
    :wxStyledTextCtrl.setReadOnly(ed, true)
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

  def system_continue(_Parent, _Debug, s) do
    :reltool_mod_win.loop(s)
  end

  def system_terminate(reason, _Parent, _Debug, _S) do
    exit(reason)
  end

  def system_code_change(s, _Module, _OldVsn, _Extra) do
    {:ok, s}
  end
end
