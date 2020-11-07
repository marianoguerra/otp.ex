defmodule :m_reltool_utils do
  use Bitwise
  require Record

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

  def root_dir() do
    :code.root_dir()
  end

  def erl_libs() do
    erl_libs(:os.getenv('ERL_LIBS', ''), :os.type())
  end

  def erl_libs(erlLibs, osType) when is_list(erlLibs) do
    sep =
      case osType do
        {:win32, _} ->
          ';'

        _ ->
          ':'
      end

    :string.lexemes(erlLibs, sep)
  end

  def lib_dirs(dir) do
    case :erl_prim_loader.list_dir(dir) do
      {:ok, files} ->
        for f <- files,
            :filelib.is_dir(
              :filename.join([dir, f]),
              :erl_prim_loader
            ) do
          f
        end

      :error ->
        []
    end
  end

  def split_app_name(name) do
    pred = fn elem ->
      cond do
        elem === ?. ->
          true

        elem >= ?0 and elem <= ?9 ->
          true

        true ->
          false
      end
    end

    case :lists.splitwith(pred, :lists.reverse(name)) do
      {vsn, [?- | app]} ->
        {:erlang.list_to_atom(:lists.reverse(app)), :lists.reverse(vsn)}

      _ ->
        {:erlang.list_to_atom(name), ''}
    end
  end

  def normalize_dir(relDir) do
    tokens = :filename.split(:filename.absname(relDir))

    :filename.join(
      :lists.reverse(
        normalize_dir(
          tokens,
          []
        )
      )
    )
  end

  defp normalize_dir(['..' | dirs], [_Dir | path]) do
    normalize_dir(dirs, path)
  end

  defp normalize_dir(['.' | dirs], path) do
    normalize_dir(dirs, path)
  end

  defp normalize_dir([dir | dirs], path) do
    normalize_dir(dirs, [dir | path])
  end

  defp normalize_dir([], path) do
    path
  end

  def prim_consult(bin) when is_binary(bin) do
    case :erl_scan.string(
           :unicode.characters_to_list(
             bin,
             encoding(bin)
           )
         ) do
      {:ok, tokens, _EndLine} ->
        prim_parse(tokens, [])

      {:error, {_ErrorLine, module, reason}, _EndLine} ->
        {:error, module.format_error(reason)}
    end
  end

  def prim_consult(fullName) when is_list(fullName) do
    case :erl_prim_loader.get_file(fullName) do
      {:ok, bin, _} ->
        prim_consult(bin)

      :error ->
        {:error, :file.format_error(:enoent)}
    end
  end

  defp encoding(bin) when is_binary(bin) do
    case :epp.read_encoding_from_binary(bin) do
      :none ->
        :epp.default_encoding()

      e ->
        e
    end
  end

  defp prim_parse(tokens, acc) do
    case :lists.splitwith(
           fn t ->
             :erlang.element(1, t) !== :dot
           end,
           tokens
         ) do
      {[], []} ->
        {:ok, :lists.reverse(acc)}

      {tokens2, [{:dot, _} = dot | rest]} ->
        case :erl_parse.parse_term(tokens2 ++ [dot]) do
          {:ok, term} ->
            prim_parse(rest, [term | acc])

          {:error, {_ErrorLine, module, reason}} ->
            {:error, module.format_error(reason)}
        end

      {tokens2, []} ->
        case :erl_parse.parse_term(tokens2) do
          {:ok, term} ->
            {:ok, :lists.reverse([term | acc])}

          {:error, {_ErrorLine, module, reason}} ->
            {:error, module.format_error(reason)}
        end
    end
  end

  def default_rels() do
    [
      r_rel(name: 'start_clean', vsn: '1.0', rel_apps: []),
      r_rel(name: 'start_sasl', vsn: '1.0', rel_apps: [r_rel_app(name: :sasl)]),
      r_rel(name: 'no_dot_erlang', vsn: '1.0', rel_apps: [], load_dot_erlang: false)
    ]
  end

  def choose_default(tag, profile, inclDefs)
      when profile === :development or inclDefs do
    case tag do
      :incl_sys_filters ->
        ['.*']

      :excl_sys_filters ->
        []

      :incl_app_filters ->
        ['.*']

      :excl_app_filters ->
        []

      :embedded_app_type ->
        :undefined
    end
  end

  def choose_default(tag, :standalone, _InclDefs) do
    case tag do
      :incl_sys_filters ->
        [
          '^bin/(erl|epmd)(|\\.exe|\\.ini)$',
          '^bin/start(|_clean).boot$',
          '^bin/no_dot_erlang\\.boot$',
          '^erts.*/bin',
          '^lib$'
        ]

      :excl_sys_filters ->
        [
          '^erts.*/bin/(erlc|dialyzer|typer)(|\\.exe)$',
          '^erts.*/bin/(start|escript|to_erl|run_erl)(|\\.exe)$',
          '^erts.*/bin/.*(debug|pdb)'
        ]

      :incl_app_filters ->
        ['^ebin', '^priv']

      :excl_app_filters ->
        ['^ebin/.*\\.appup$']

      :embedded_app_type ->
        :undefined
    end
  end

  def choose_default(tag, :embedded, _InclDefs) do
    case tag do
      :incl_sys_filters ->
        ['^bin', '^erts', '^lib', '^releases']

      :excl_sys_filters ->
        [
          '^bin/(erlc|dialyzer|typer)(|\\.exe)$',
          '^erts.*/bin/(erlc|dialyzer|typer)(|\\.exe)$',
          '^erts.*/bin/.*(debug|pdb)'
        ]

      :incl_app_filters ->
        ['^ebin', '^include', '^priv']

      :excl_app_filters ->
        []

      :embedded_app_type ->
        :load
    end
  end

  def assign_image_list(listCtrl) do
    art = :wxImageList.new(16, 16)

    for image <- [
          'wxART_ERROR',
          'wxART_WARNING',
          'wxART_QUESTION',
          'wxART_TICK_MARK',
          'wxART_CROSS_MARK',
          'wxART_GO_HOME'
        ] do
      :wxImageList.add(
        art,
        :wxArtProvider.getBitmap(image, [{:size, {16, 16}}])
      )
    end

    :wxListCtrl.assignImageList(listCtrl, art, 1)
  end

  def get_latest_resize(r_wx(obj: objRef, event: r_wxSize()) = wx) do
    receive do
      r_wx(obj: ^objRef, event: r_wxSize()) = wx2 ->
        get_latest_resize(wx2)
    after
      10 ->
        wx
    end
  end

  def wait_for_stop_motion(objRef, {_, _} = pos) do
    receive do
      r_wx(obj: ^objRef, event: r_wxMouse(type: :motion, x: x, y: y)) ->
        wait_for_stop_motion(objRef, {x, y})
    after
      100 ->
        pos
    end
  end

  def mod_conds() do
    ['all (ebin + app file)', 'ebin + derived', 'app file + derived', 'derived', 'none']
  end

  def list_to_mod_cond(list) do
    case list do
      'all' ++ _ ->
        :all

      'ebin' ++ _ ->
        :ebin

      'app' ++ _ ->
        :app

      'derived' ->
        :derived

      'none' ->
        :none
    end
  end

  def mod_cond_to_index(modCond) do
    case modCond do
      :all ->
        0

      :ebin ->
        1

      :app ->
        2

      :derived ->
        3

      :undefined ->
        3

      :none ->
        4
    end
  end

  def incl_conds() do
    ['include', 'exclude', 'derived']
  end

  def list_to_incl_cond(list) do
    case list do
      'include' ->
        :include

      'exclude' ->
        :exclude

      'derived' ->
        :derived
    end
  end

  def incl_cond_to_index(modCond) do
    case modCond do
      :include ->
        0

      :exclude ->
        1

      :derived ->
        2
    end
  end

  def elem_to_index(elem, list) do
    elem_to_index(elem, list, 1)
  end

  defp elem_to_index(elem, [h | t], index) do
    case elem === h do
      true ->
        index

      false ->
        elem_to_index(elem, t, index + 1)
    end
  end

  defp elem_to_index(elem, [], _) do
    :erlang.error({:not_found, elem})
  end

  def app_dir_test(dir1, dir2) do
    {name1, vsn1, parent1} = split_app_dir(dir1)
    {name2, vsn2, parent2} = split_app_dir(dir2)

    cond do
      name1 < name2 ->
        true

      name1 > name2 ->
        false

      vsn1 < vsn2 ->
        false

      vsn1 > vsn2 ->
        true

      parent1 <= parent2 ->
        true

      true ->
        false
    end
  end

  def split_app_dir(dir) do
    parentDir = :filename.dirname(dir)
    base = :filename.basename(dir)
    {name, vsn} = split_app_name(base)

    vsn2 =
      try do
        for n <- :string.lexemes(vsn, '.') do
          :erlang.list_to_integer(n)
        end
      catch
        _, _ ->
          vsn
      end

    {name, vsn2, parentDir}
  end

  def get_item(listCtrl) do
    case :wxListCtrl.getItemCount(listCtrl) do
      0 ->
        :undefined

      _ ->
        case :wxListCtrl.getNextItem(listCtrl, -1, [{:geometry, 1}, {:state, 4}]) do
          -1 ->
            itemNo = :wxListCtrl.getTopItem(listCtrl)

            case :wxListCtrl.getItemText(listCtrl, itemNo) do
              '' ->
                :undefined

              text ->
                {itemNo, text}
            end

          itemNo ->
            text = :wxListCtrl.getItemText(listCtrl, itemNo)
            {itemNo, text}
        end
    end
  end

  def get_items(listCtrl) do
    case :wxListCtrl.getItemCount(listCtrl) do
      0 ->
        []

      count ->
        case get_selected_items(listCtrl, -1, []) do
          [] ->
            itemNo = :wxListCtrl.getTopItem(listCtrl)

            case :wxListCtrl.getItemText(listCtrl, itemNo) do
              '' ->
                []

              text when text !== '*MISSING*' ->
                [{itemNo, text}]

              _MissingText when count > 1 ->
                case :wxListCtrl.getItemText(listCtrl, itemNo + 1) do
                  '' ->
                    []

                  text ->
                    [{itemNo, text}]
                end

              _MissingText ->
                []
            end

          items ->
            items
        end
    end
  end

  def get_selected_items(listCtrl, prevItem, acc) do
    case :wxListCtrl.getNextItem(listCtrl, prevItem, [{:geometry, 1}, {:state, 4}]) do
      -1 ->
        acc

      itemNo ->
        case :wxListCtrl.getItemText(listCtrl, itemNo) do
          text when text !== '*MISSING*' ->
            get_selected_items(listCtrl, itemNo, [{itemNo, text} | acc])

          _Text ->
            get_selected_items(listCtrl, itemNo, acc)
        end
    end
  end

  def select_items(_ListCtrl, _OldItems, []) do
    false
  end

  def select_items(listCtrl, [], items) do
    select_item(listCtrl, items)
  end

  def select_items(listCtrl, _OldItems, [item]) do
    select_item(listCtrl, [item])
  end

  def select_items(listCtrl, oldItems, newItems) do
    filter = fn {_OldItemNo, text} ->
      case :lists.keysearch(text, 2, newItems) do
        {:value, item} ->
          {true, item}

        false ->
          false
      end
    end

    case :lists.zf(filter, oldItems) do
      [] ->
        select_item(listCtrl, newItems)

      validItems ->
        :lists.foreach(
          fn item ->
            select_item(listCtrl, [item])
          end,
          validItems
        )
    end
  end

  def select_item(listCtrl, [{itemNo, text} | items]) do
    case text === '*MISSING*' do
      true ->
        select_item(listCtrl, items)

      false ->
        stateMask = 4
        state = :wxListCtrl.getItemState(listCtrl, itemNo, stateMask)
        state2 = state ||| 4
        :wxListCtrl.setItemState(listCtrl, itemNo, state2, stateMask)
        :wxListCtrl.refreshItem(listCtrl, itemNo)
    end
  end

  def select_item(_ListCtrl, []) do
    :ok
  end

  def get_column_width(listCtrl) do
    :wx.batch(fn ->
      {total, _} = :wxWindow.getClientSize(listCtrl)
      total - scroll_size(listCtrl)
    end)
  end

  defp scroll_size(objRef) do
    case :os.type() do
      {:win32, :nt} ->
        0

      {:unix, :darwin} ->
        :wxSystemSettings.getMetric(28)

      _ ->
        case :wxWindow.hasScrollbar(objRef, 8) do
          true ->
            :wxSystemSettings.getMetric(28)

          false ->
            0
        end
    end
  end

  def safe_keysearch(key, pos, list, mod, line) do
    case :lists.keysearch(key, pos, list) do
      false ->
        :io.format('~w(~w): lists:keysearch(~tp, ~w, ~tp) -> false\n', [mod, line, key, pos, list])

        :erlang.error({mod, line, :lists, :keysearch, [key, pos, list]})

      {:value, val} ->
        val
    end
  end

  def print(x, x, format, args) do
    :io.format(format, args)
  end

  def print(_, _, _, _) do
    :ok
  end

  def add_warning(format, args, {:ok, warnings}) do
    warning = :lists.flatten(:io_lib.format(format, args))

    case :lists.member(warning, warnings) do
      true ->
        {:ok, warnings}

      false ->
        {:ok, [warning | warnings]}
    end
  end

  def create_dir(dir) do
    :filelib.ensure_dir(dir)

    case :file.make_dir(dir) do
      :ok ->
        :ok

      {:error, :eexist} ->
        :ok

      {:error, reason} ->
        text = :file.format_error(reason)
        throw_error('create dir ~ts: ~ts', [dir, text])
    end
  end

  def list_dir(dir) do
    case :erl_prim_loader.list_dir(dir) do
      {:ok, files} ->
        files

      :error ->
        text = :file.format_error(:enoent)
        throw_error('list dir ~ts: ~ts', [dir, text])
    end
  end

  def read_file_info(file) do
    case :file.read_file_info(file) do
      {:ok, info} ->
        info

      {:error, reason} ->
        text = :file.format_error(reason)
        throw_error('read file info ~ts: ~ts', [file, text])
    end
  end

  def write_file_info(file, info) do
    case :file.write_file_info(file, info) do
      :ok ->
        :ok

      {:error, reason} ->
        text = :file.format_error(reason)
        throw_error('write file info ~ts: ~ts', [file, text])
    end
  end

  def read_file(file) do
    case :file.read_file(file) do
      {:ok, bin} ->
        bin

      {:error, reason} ->
        text = :file.format_error(reason)
        throw_error('read file ~ts: ~ts', [file, text])
    end
  end

  def write_file(file, bin) do
    case :file.write_file(file, bin) do
      :ok ->
        :ok

      {:error, reason} ->
        text = :file.format_error(reason)
        throw_error('write file ~ts: ~ts', [file, text])
    end
  end

  def recursive_delete(dir) do
    case :filelib.is_dir(dir) do
      true ->
        case :file.list_dir(dir) do
          {:ok, files} ->
            fun = fn f ->
              recursive_delete(:filename.join([dir, f]))
            end

            :lists.foreach(fun, files)
            delete(dir, :directory)

          {:error, :enoent} ->
            :ok

          {:error, reason} ->
            text = :file.format_error(reason)
            throw_error('delete file ~ts: ~ts\n', [dir, text])
        end

      false ->
        delete(dir, :regular)
    end
  end

  def delete(file, type) do
    case do_delete(file, type) do
      :ok ->
        :ok

      {:error, :enoent} ->
        :ok

      {:error, reason} ->
        text = :file.format_error(reason)
        throw_error('delete file ~ts: ~ts\n', [file, text])
    end
  end

  defp do_delete(file, :regular) do
    :file.delete(file)
  end

  defp do_delete(dir, :directory) do
    :file.del_dir(dir)
  end

  def recursive_copy_file(from, to) do
    case :erl_prim_loader.list_dir(from) do
      {:ok, files} ->
        create_dir(to)

        copy = fn f ->
          recursive_copy_file(
            :filename.join([from, f]),
            :filename.join([to, f])
          )
        end

        :lists.foreach(copy, files)

      :error ->
        copy_file(from, to)
    end
  end

  def copy_file(from, to) do
    case :erl_prim_loader.get_file(from) do
      {:ok, bin, _} ->
        case :file.write_file(to, bin) do
          :ok ->
            fromInfo = read_file_info(from)
            toInfo = read_file_info(to)
            fromMode = r_file_info(fromInfo, :mode)
            toMode = r_file_info(toInfo, :mode)
            toMode2 = fromMode ||| toMode
            fileInfo = r_file_info(toInfo, mode: toMode2)
            write_file_info(to, fileInfo)
            :ok

          {:error, reason} ->
            text = :file.format_error(reason)
            throw_error('copy file ~ts -> ~ts: ~ts\n', [from, to, text])
        end

      :error ->
        text = :file.format_error(:enoent)
        throw_error('copy file ~ts -> ~ts: ~ts\n', [from, to, text])
    end
  end

  def throw_error(format, args) do
    throw({:error, :lists.flatten(:io_lib.format(format, args))})
  end

  def decode_regexps(key, regexps, :undefined) do
    decode_regexps(key, regexps, [])
  end

  def decode_regexps(key, {:add, regexps}, old)
      when is_list(regexps) do
    do_decode_regexps(key, regexps, old)
  end

  def decode_regexps(_Key, {:del, regexps}, old)
      when is_list(regexps) do
    for re <- old,
        not :lists.member(r_regexp(re, :source), regexps) do
      re
    end
  end

  def decode_regexps(key, regexps, _Old) when is_list(regexps) do
    do_decode_regexps(key, regexps, [])
  end

  defp do_decode_regexps(key, [regexp | regexps], acc) do
    case (try do
            :re.compile(regexp, [:unicode])
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, mP} ->
        do_decode_regexps(key, regexps, [r_regexp(source: regexp, compiled: mP) | acc])

      _ ->
        text =
          :lists.flatten(
            :io_lib.format(
              '~tp',
              [{key, regexp}]
            )
          )

        throw({:error, 'Illegal option: ' ++ text})
    end
  end

  defp do_decode_regexps(_Key, [], acc) do
    :lists.sort(acc)
  end

  def default_val(val, default) do
    case val do
      :undefined ->
        default

      _ ->
        val
    end
  end

  def escript_foldl(fun, acc, file) do
    case :escript.extract(file, [:compile_source]) do
      {:ok, [_Shebang, _Comment, _EmuArgs, body]} ->
        case body do
          {:source, beamCode} ->
            getInfo = fn ->
              :file.read_file_info(file)
            end

            getBin = fn ->
              beamCode
            end

            {:ok, fun.('.', getInfo, getBin, acc)}

          {:beam, beamCode} ->
            getInfo = fn ->
              :file.read_file_info(file)
            end

            getBin = fn ->
              beamCode
            end

            {:ok, fun.('.', getInfo, getBin, acc)}

          {:archive, archiveBin} ->
            :zip.foldl(fun, acc, {file, archiveBin})
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  def call(name, msg) when is_atom(name) do
    case :erlang.whereis(name) do
      :undefined ->
        {:error, {:noproc, name}}

      pid ->
        call(pid, msg)
    end
  end

  def call(pid, msg) when is_pid(pid) do
    ref = :erlang.monitor(:process, pid)
    send(pid, {:call, self(), ref, msg})

    receive do
      {^ref, reply} ->
        reply

      {:EXIT, ^pid, reason} ->
        :erlang.demonitor(ref, [:flush])
        {:error, reason}

      {:DOWN, ^ref, _, _, reason} ->
        {:error, reason}
    end
  end

  def cast(pid, msg) do
    send(pid, {:cast, self(), msg})
    :ok
  end

  def reply(pid, ref, msg) do
    send(pid, {ref, msg})
  end
end
