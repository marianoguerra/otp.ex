defmodule :m_dbg_wx_code do
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

  def code_area(parent) do
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
      {13, {0, 0, 0}},
      {14, {160, 53, 35}},
      {15, {160, 53, 35}},
      {16, {160, 53, 35}},
      {17, {160, 53, 35}},
      {18, {0, 0, 0}},
      {19, {40, 144, 170}},
      {20, {40, 100, 20}},
      {21, {0, 0, 0}},
      {22, {130, 40, 172}},
      {23, {64, 102, 244}},
      {24, {64, 102, 244}}
    ]

    setStyle = fn {style, color} ->
      :wxStyledTextCtrl.styleSetFont(ed, style, fixedFont)
      :wxStyledTextCtrl.styleSetForeground(ed, style, color)
    end

    for style <- styles do
      setStyle.(style)
    end

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
    :wxStyledTextCtrl.setReadOnly(ed, true)
    ed
  end

  def load_code(ed, code) do
    :wxStyledTextCtrl.setReadOnly(ed, false)
    :wxStyledTextCtrl.setTextRaw(ed, code)
    lines = :wxStyledTextCtrl.getLineCount(ed)
    sz = trunc(:math.log10(lines)) + 1
    lW = :wxStyledTextCtrl.textWidth(ed, 33, :lists.duplicate(sz, ?9))
    :wxStyledTextCtrl.setMarginWidth(ed, 0, lW + 5)
    :wxStyledTextCtrl.setReadOnly(ed, true)
    :ok
  end

  def unload_code(ed) do
    :wxStyledTextCtrl.setReadOnly(ed, false)
    :wxStyledTextCtrl.setTextRaw(ed, <<0::size(8)>>)
    :wxStyledTextCtrl.setReadOnly(ed, true)
    :ok
  end

  def add_break_to_code(ed, line, :active) do
    :wxStyledTextCtrl.markerDelete(ed, line - 1, 1)
    :wxStyledTextCtrl.markerAdd(ed, line - 1, 0)
    :ok
  end

  def add_break_to_code(ed, line, :inactive) do
    :wxStyledTextCtrl.markerDelete(ed, line - 1, 0)
    :wxStyledTextCtrl.markerAdd(ed, line - 1, 1)
    :ok
  end

  def del_break_from_code(ed, line) do
    :wxStyledTextCtrl.markerDelete(ed, line - 1, 0)
    :wxStyledTextCtrl.markerDelete(ed, line - 1, 1)
  end

  def mark_line(ed, prev, line) do
    goto_line(ed, line)
    :wxStyledTextCtrl.markerDelete(ed, prev - 1, 2)
    :wxStyledTextCtrl.markerAdd(ed, line - 1, 2)
    :wxStyledTextCtrl.markerDelete(ed, prev - 1, 3)
    :wxStyledTextCtrl.markerAdd(ed, line - 1, 3)
  end

  def get_no_lines(ed) do
    :wxStyledTextCtrl.getLineCount(ed)
  end

  def goto_line(_Ed, 0) do
    :ignore
  end

  def goto_line(ed, line) do
    :wxStyledTextCtrl.gotoLine(ed, line - 1)
  end

  def current_pos(ed) do
    :wxStyledTextCtrl.getCurrentPos(ed)
  end

  def goto_pos(ed, pos) do
    :wxStyledTextCtrl.gotoPos(ed, pos)
  end

  def find(ed, str, case__, next) do
    :wxStyledTextCtrl.searchAnchor(ed)

    flag =
      cond do
        case__ ->
          4

        true ->
          0
      end

    res =
      cond do
        next ->
          :wxStyledTextCtrl.searchNext(ed, flag, str)

        true ->
          :wxStyledTextCtrl.searchPrev(ed, flag, str)
      end

    case res >= 0 do
      true ->
        :wxStyledTextCtrl.scrollToLine(
          ed,
          :wxStyledTextCtrl.lineFromPosition(
            ed,
            res
          ) - 3
        )

        true

      false ->
        false
    end
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
end
