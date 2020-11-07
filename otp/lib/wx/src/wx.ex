defmodule :m_wx do
  use Bitwise
  require Record
  Record.defrecord(:r_wx_ref, :wx_ref, ref: :undefined, type: :undefined, state: [])
  Record.defrecord(:r_wx_env, :wx_env, port: :undefined, sv: :undefined, debug: 0)

  Record.defrecord(:r_wx_mem, :wx_mem,
    bin: :undefined,
    size: :undefined
  )

  Record.defrecord(:r_evh, :evh,
    et: :null,
    id: -1,
    lastId: -1,
    cb: 0,
    skip: :undefined,
    userdata: [],
    handler: :undefined
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

  def parent_class(_) do
    true
  end

  def new() do
    new([])
  end

  def new(options) when is_list(options) do
    debug = :proplists.get_value(:debug, options, 0)
    silentStart = :proplists.get_value(:silent_start, options, false)
    level = calc_level(debug)
    r_wx_env(port: port) = :wxe_server.start(silentStart and level === 0)
    :erlang.put(:opengl_port, port)
    set_debug(level)
    null()
  end

  def destroy() do
    :wxe_server.stop()
    :erlang.erase(:wx_env)
    :ok
  end

  def get_env() do
    case :erlang.get(:wx_env) do
      :undefined ->
        :erlang.error({:wxe, :unknown_port})

      env = r_wx_env() ->
        env
    end
  end

  def set_env(r_wx_env(sv: pid, port: port) = env) do
    :erlang.put(:wx_env, env)
    :erlang.put(:opengl_port, port)
    :wxe_server.register_me(pid)
    :ok
  end

  def null() do
    r_wx_ref(ref: 0, type: :wx)
  end

  def is_null(r_wx_ref(ref: nULL)) do
    nULL === 0
  end

  def equal(r_wx_ref(ref: ref1), r_wx_ref(ref: ref2)) do
    ref1 === ref2
  end

  def getObjectType(r_wx_ref(type: type)) do
    type
  end

  def typeCast(old = r_wx_ref(), newType) when is_atom(newType) do
    r_wx_ref(old, type: newType)
  end

  def batch(fun) do
    :ok = :wxe_util.cast(0, <<>>)

    try do
      fun.()
    catch
      :error, w ->
        :erlang.exit({w, __STACKTRACE__})

      w ->
        :erlang.throw(w)

      :exit, w ->
        :erlang.exit(w)
    after
      :ok = :wxe_util.cast(1, <<>>)
    end
  end

  def foreach(fun, list) do
    :ok = :wxe_util.cast(0, <<>>)

    try do
      :lists.foreach(fun, list)
    catch
      :error, w ->
        :erlang.exit({w, __STACKTRACE__})

      w ->
        :erlang.throw(w)

      :exit, w ->
        :erlang.exit(w)
    after
      :ok = :wxe_util.cast(1, <<>>)
    end
  end

  def map(fun, list) do
    :ok = :wxe_util.cast(0, <<>>)

    try do
      :lists.map(fun, list)
    catch
      :error, w ->
        :erlang.exit({w, __STACKTRACE__})

      w ->
        :erlang.throw(w)

      :exit, w ->
        :erlang.exit(w)
    after
      :ok = :wxe_util.cast(1, <<>>)
    end
  end

  def foldl(fun, acc, list) do
    :ok = :wxe_util.cast(0, <<>>)

    try do
      :lists.foldl(fun, acc, list)
    catch
      :error, w ->
        :erlang.exit({w, __STACKTRACE__})

      w ->
        :erlang.throw(w)

      :exit, w ->
        :erlang.exit(w)
    after
      :ok = :wxe_util.cast(1, <<>>)
    end
  end

  def foldr(fun, acc, list) do
    :ok = :wxe_util.cast(0, <<>>)

    try do
      :lists.foldr(fun, acc, list)
    catch
      :error, w ->
        :erlang.exit({w, __STACKTRACE__})

      w ->
        :erlang.throw(w)

      :exit, w ->
        :erlang.exit(w)
    after
      :ok = :wxe_util.cast(1, <<>>)
    end
  end

  def create_memory(size) when size > 64 do
    r_wx_mem(bin: <<0::size(size * 8)>>, size: size)
  end

  def create_memory(size) do
    r_wx_mem(bin: <<0::size((64 + 1) * 8)>>, size: size)
  end

  def get_memory_bin(r_wx_mem(bin: bin, size: size)) when size > 64 do
    bin
  end

  def get_memory_bin(r_wx_mem(bin: bin, size: size)) do
    <<withCorrectSize::size(size)-binary, _::binary>> = bin
    withCorrectSize
  end

  def retain_memory(r_wx_mem(bin: bin)) do
    :wxe_util.send_bin(bin)
    :ok = :wxe_util.cast(11, <<>>)
  end

  def retain_memory(bin) when is_binary(bin) do
    case byte_size(bin) > 64 do
      true ->
        :ok

      false ->
        :erlang.error(:small_bin)
    end

    :wxe_util.send_bin(bin)
    :ok = :wxe_util.cast(11, <<>>)
  end

  def release_memory(r_wx_mem(bin: bin)) do
    :wxe_util.send_bin(bin)
    :ok = :wxe_util.cast(12, <<>>)
  end

  def release_memory(bin) when is_binary(bin) do
    :wxe_util.send_bin(bin)
    :ok = :wxe_util.cast(12, <<>>)
  end

  def debug(debug) do
    level = calc_level(debug)
    set_debug(level)
  end

  defp calc_level(:none) do
    calc_level(0)
  end

  defp calc_level(:verbose) do
    calc_level(1)
  end

  defp calc_level(:trace) do
    calc_level(2)
  end

  defp calc_level(:driver) do
    calc_level(16)
  end

  defp calc_level([]) do
    calc_level(0)
  end

  defp calc_level(list) when is_list(list) do
    {drv, erl} =
      :lists.foldl(
        fn
          :verbose, {drv, _Erl} ->
            {drv, 1}

          :trace, {drv, _Erl} ->
            {drv, 2}

          :driver, {_Drv, erl} ->
            {16, erl}
        end,
        {0, 0},
        list
      )

    drv + erl
  end

  defp calc_level(level) when is_integer(level) do
    level
  end

  defp set_debug(level) when is_integer(level) do
    case :erlang.get(:wx_env) do
      :undefined ->
        :erlang.error({:wxe, :unknown_port})

      r_wx_env(debug: old) when old === level ->
        :ok

      env = r_wx_env(sv: server, port: port, debug: old) ->
        cond do
          old > 16 and level > 16 ->
            :ok

          old < 16 and level < 16 ->
            :ok

          true ->
            :erlang.port_call(port, 9, [level >>> 4])
        end

        :erlang.put(:wx_env, r_wx_env(env, debug: level))
        :wxe_server.set_debug(server, level)
        :ok
    end
  end

  def demo() do
    priv = :code.priv_dir(:wx)
    demo = :filename.join([:filename.dirname(priv), :examples, :demo])
    mod = :erlang.list_to_atom('demo')

    case :file.set_cwd(demo) do
      :ok ->
        apply(mod, :start, [])
        :ok

      _ ->
        {:error, :no_demo_dir}
    end
  end
end
