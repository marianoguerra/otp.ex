defmodule :m_dbg_wx_win do
  use Bitwise
  import Kernel, except: [to_string: 1]
  require Record

  Record.defrecord(:r_break, :break,
    mb: :undefined,
    smi: :undefined,
    emi: :undefined,
    dimi: :undefined,
    demi: :undefined
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

  def init() do
    _ = :wx.new()
    :ok
  end

  def create_menus(mB, [{title, items} | ms], win, id0) do
    menu = :wxMenu.new([])
    :erlang.put(title, menu)
    id = create_menu_item(menu, items, win, id0, true)
    :wxMenuBar.append(mB, menu, menu_name(title, :ignore))
    create_menus(mB, ms, win, id)
  end

  def create_menus(_MB, [], _Win, id) do
    id
  end

  defp create_menu_item(menu, [:separator | is], win, id, connect) do
    _ = :wxMenu.appendSeparator(menu)
    create_menu_item(menu, is, win, id + 1, connect)
  end

  defp create_menu_item(menu, [{name, _N, :cascade, items} | is], win, id0, connect) do
    sub = :wxMenu.new([])
    id = create_menu_item(sub, items, win, id0, false)
    _ = :wxMenu.append(menu, -1, menu_name(name, :ignore), sub)
    self = self()

    butts =
      for {mI, _, _} <- items do
        {mI, :erlang.get(mI)}
      end

    isChecked = fn {miName, mI}, acc ->
      case :wxMenuItem.isChecked(mI) do
        true ->
          [miName | acc]

        false ->
          acc
      end
    end

    filter = fn ev, _ ->
      enabled = :lists.foldl(isChecked, [], butts)
      send(self, r_wx(ev, userData: {name, enabled}))
    end

    _ =
      :wxMenu.connect(win, :command_menu_selected, [
        {:id, id0},
        {:lastId, id - 1},
        {:callback, filter}
      ])

    create_menu_item(menu, is, win, id, connect)
  end

  defp create_menu_item(menu, [{name, pos} | is], win, id, connect) do
    menuId =
      case :lists.member(name, [:Debugger]) do
        true ->
          5009

        _ ->
          id
      end

    item = :wxMenu.append(menu, menuId, menu_name(name, pos))
    :erlang.put(name, item)

    cond do
      connect ->
        :wxMenu.connect(win, :command_menu_selected, [{:id, menuId}, {:userData, name}])

      true ->
        :ignore
    end

    create_menu_item(menu, is, win, id + 1, connect)
  end

  defp create_menu_item(menu, [{name, n, :check} | is], win, id, connect) do
    item = :wxMenu.appendCheckItem(menu, id, menu_name(name, n))
    :erlang.put(name, item)

    cond do
      connect ->
        :wxMenu.connect(win, :command_menu_selected, [{:id, id}, {:userData, name}])

      true ->
        :ignore
    end

    create_menu_item(menu, is, win, id + 1, connect)
  end

  defp create_menu_item(menu, [{name, n, :radio} | is], win, id, connect) do
    item = :wxMenu.appendRadioItem(menu, id, menu_name(name, n))
    :erlang.put(name, item)

    cond do
      connect ->
        :wxMenu.connect(win, :command_menu_selected, [{:id, id}, {:userData, name}])

      true ->
        :ignore
    end

    create_menu_item(menu, is, win, id + 1, connect)
  end

  defp create_menu_item(_, [], _, id, _) do
    id
  end

  def add_break(win, menuName, point) do
    {mod, line} = point
    label = to_string('~w ~5w', [mod, line])
    menu = :erlang.get(menuName)

    add = fn item, action ->
      id = :wxMenuItem.getId(item)
      :wxMenu.connect(win, :command_menu_selected, [{:id, id}, {:userData, action}])
    end

    sub = :wxMenu.new([])
    dis = :wxMenu.append(sub, -1, 'Disable')
    add.(dis, {:break, point, :status})
    del = :wxMenu.append(sub, -1, 'Delete')
    add.(del, {:break, point, :delete})
    trigger = :wxMenu.new([])
    enable = :wxMenu.appendRadioItem(trigger, -1, 'Enable')
    add.(enable, {:break, point, {:trigger, :enable}})
    tDisable = :wxMenu.appendRadioItem(trigger, -1, 'Disable')
    add.(tDisable, {:break, point, {:trigger, :disable}})
    delete = :wxMenu.appendRadioItem(trigger, -1, 'Delete')
    add.(delete, {:break, point, {:trigger, :delete}})
    _ = :wxMenu.append(sub, -1, 'Trigger Action', trigger)
    menuBtn = :wxMenu.append(menu, -1, label, sub)
    r_break(mb: {menu, menuBtn}, smi: dis, emi: enable, dimi: tDisable, demi: delete)
  end

  def update_break(break, options) do
    [[status, trigger] | _] = options

    label =
      case status do
        :active ->
          'Disable'

        :inactive ->
          'Enable'
      end

    :wxMenuItem.setText(r_break(break, :smi), label)

    triggerMI =
      case trigger do
        :enable ->
          r_break(break, :emi)

        :disable ->
          r_break(break, :dimi)

        :delete ->
          r_break(break, :demi)
      end

    :wxMenuItem.check(triggerMI)
  end

  def delete_break(break) do
    {menu, menuBtn} = r_break(break, :mb)
    :wxMenu."Destroy"(menu, menuBtn)
  end

  def motion(x, y) do
    receive do
      {:gs, _Id, :motion, _Data, [nX, nY]} ->
        motion(nX, nY)
    after
      0 ->
        {x, y}
    end
  end

  def confirm(win, message) do
    mD =
      :wxMessageDialog.new(win, to_string(message), [{:style, 4 ||| 16}, {:caption, 'Confirm'}])

    res =
      case :wxDialog.showModal(mD) do
        5100 ->
          :ok

        _ ->
          :cancel
      end

    :wxDialog.destroy(mD)
    res
  end

  def notify(win, message) do
    mD = :wxMessageDialog.new(win, to_string(message), [{:style, 4}, {:caption, 'Confirm'}])
    :wxDialog.showModal(mD)
    :wxDialog.destroy(mD)
    :ok
  end

  def entry(parent, title, prompt, {type, value}) do
    ted =
      :wxTextEntryDialog.new(parent, to_string(prompt), [
        {:caption, to_string(title)},
        {:value, value}
      ])

    case :wxDialog.showModal(ted) do
      5100 ->
        res =
          case verify(
                 type,
                 :wxTextEntryDialog.getValue(ted)
               ) do
            {:edit, newVal} ->
              {prompt, newVal}

            :ignore ->
              :cancel
          end

        :wxTextEntryDialog.destroy(ted)
        res

      _ ->
        :cancel
    end
  end

  defp verify(type, str) do
    case :erl_scan.string(str, 1, [:text]) do
      {:ok, tokens, _EndLine} when type == :term ->
        case :erl_eval.extended_parse_term(tokens ++ [{:dot, :erl_anno.new(1)}]) do
          {:ok, value} ->
            {:edit, value}

          _Error ->
            :ignore
        end

      {:ok, [{^type, _Line, value}], _EndLine}
      when type != :term ->
        {:edit, value}

      _Err ->
        :ignore
    end
  end

  def open_help(_Parent, helpHtmlFile) do
    :wx_misc.launchDefaultBrowser('file://' ++ helpHtmlFile)
  end

  def to_string(atom) when is_atom(atom) do
    :erlang.atom_to_list(atom)
  end

  def to_string(integer) when is_integer(integer) do
    :erlang.integer_to_list(integer)
  end

  def to_string([]) do
    ''
  end

  def to_string(list) when is_list(list) do
    try do
      :unicode.characters_to_list(list)
    catch
      _, _ ->
        :io_lib.format('~tp', [list])
    end
  end

  def to_string(term) do
    :io_lib.format('~tp', [term])
  end

  def to_string(format, args) do
    :io_lib.format(format, args)
  end

  defp menu_name(atom, n) when is_atom(atom) do
    menu_name(:erlang.atom_to_list(atom), n)
  end

  defp menu_name('Help', _) do
    '&Help'
  end

  defp menu_name(str, pos) when is_integer(pos) do
    {s1, s2} = :lists.split(pos, str)
    s1 ++ [?& | s2]
  end

  defp menu_name(str, _) do
    str
  end

  def find_icon(file) do
    privDir = :code.priv_dir(:debugger)
    privIcon = :filename.append(privDir, file)

    case :filelib.is_regular(privIcon) do
      true ->
        privIcon

      false ->
        currDir = :filename.dirname(:code.which(:dbg_wx_win))
        currIcon = :filename.append(currDir, file)
        true = :filelib.is_regular(currIcon)
        currIcon
    end
  end
end
