defmodule :m_dbg_wx_filedialog_win do
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

  Record.defrecord(:r_state, :state,
    win: :undefined,
    back: :undefined,
    forward: :undefined,
    up: :undefined,
    text: :undefined,
    ptext: :undefined,
    icons: [],
    completion: :undefined,
    list: :undefined,
    path: :undefined,
    files: :undefined,
    rstack: [],
    fstack: [],
    filter: :undefined,
    sort: :undefined,
    cancel: :undefined,
    ok: :undefined
  )

  Record.defrecord(:r_file, :file, name: '', type: 'file', date: '', icon: 0, color: {0, 0, 0})

  def new(parent, id, options0) do
    :wx_object.start_link(:dbg_wx_filedialog_win, [parent, id, options0], [])
  end

  def getFilename(fD) do
    :wx_object.call(fD, :getFilename)
  end

  def getFilenames(fD) do
    :wx_object.call(fD, :getFilenames)
  end

  def getDirectory(fD) do
    :wx_object.call(fD, :getDirectory)
  end

  def destroy(fD) do
    :wx_object.call(fD, :destroy)
  end

  def init([parent, id, options0]) do
    name = :proplists.get_value(:message, options0, 'Open')
    size = :proplists.get_value(:size, options0, {-1, -1})
    pos = :proplists.get_value(:pos, options0, {-1, -1})
    {:ok, defPath} = :file.get_cwd()
    path = :proplists.get_value(:defaultDir, options0, defPath)
    extraIcons = :proplists.get_value(:icons, options0, [])
    filter = :proplists.get_value(:filter, options0, &file_type_and_icon/2)
    sortCol = sort_col(:proplists.get_value(:sort, options0, :name))

    dlg =
      :wxDialog.new(parent, id, name, [
        {:size, size},
        {:pos, pos},
        {:style, 536_870_912 ||| 2048 ||| 4096 ||| 64}
      ])

    back = :wxButton.new(dlg, 5107)
    :wxButton.disable(back)
    forw = :wxButton.new(dlg, 5106)
    :wxButton.disable(forw)
    up = :wxButton.new(dlg, 5120)
    dir = :wxTextCtrl.new(dlg, 200, [{:style, 1024}])
    update_dir(path, dir)
    :wxTextCtrl.connect(dir, :command_text_updated)
    :wxTextCtrl.connect(dir, :command_text_enter)
    self = self()

    isTab = fn
      ev = r_wx(event: r_wxKey(keyCode: kC, controlDown: false, shiftDown: false, altDown: false)),
      _Object
      when kC === 9 or kC === 27 ->
        send(self, ev)

      _Ev, object ->
        :wxEvent.skip(object)
    end

    :wxTextCtrl.connect(dir, :char, [{:callback, isTab}])
    top = :wxBoxSizer.new(4)
    _ = :wxSizer.add(top, back, [{:border, 2}, {:flag, 64 ||| 128 ||| 32 ||| 16 ||| 8192}])
    _ = :wxSizer.add(top, forw, [{:border, 2}, {:flag, 64 ||| 128 ||| 32 ||| 16 ||| 8192}])
    _ = :wxSizer.add(top, up, [{:border, 2}, {:flag, 64 ||| 128 ||| 32 ||| 16 ||| 8192}])
    {art, iconMap} = create_icons(extraIcons)

    lC =
      :wxListCtrl.new(
        dlg,
        [{:style, 32 ||| 2_147_483_648}, {:size, {400, 200}}]
      )

    :wxListCtrl.assignImageList(lC, art, 1)
    lI = :wxListItem.new()

    add = fn menuName, row ->
      :wxListItem.setText(lI, menuName)
      :wxListItem.setAlign(lI, 0)
      :wxListCtrl.insertColumn(lC, row, lI)
      row + 1
    end

    :lists.foldl(add, 0, ['Name', 'Type', 'Modified'])
    :wxListItem.destroy(lI)
    files = list_files(path, {sortCol, false}, filter)
    update_files(files, lC, iconMap)
    :wxListCtrl.setColumnWidth(lC, 0, -1)
    :wxListCtrl.setColumnWidth(lC, 1, -1)
    :wxListCtrl.setColumnWidth(lC, 2, -1)
    :wxListCtrl.connect(lC, :command_list_item_activated)
    :wxListCtrl.connect(lC, :command_list_col_click)
    :wxListCtrl.connect(lC, :size, [{:skip, true}])
    bott = :wxDialog.createButtonSizer(dlg, 16 ||| 4)
    :wxDialog.connect(dlg, :command_button_clicked)
    box = :wxBoxSizer.new(8)
    _ = :wxSizer.add(box, top, [{:border, 2}, {:flag, 64 ||| 128 ||| 32 ||| 16 ||| 8192}])
    _ = :wxSizer.add(box, dir, [{:border, 2}, {:flag, 64 ||| 128 ||| 32 ||| 16 ||| 8192}])

    _ =
      :wxSizer.add(box, lC, [
        {:border, 2},
        {:flag, 64 ||| 128 ||| 32 ||| 16 ||| 8192},
        {:proportion, 1}
      ])

    _ = :wxSizer.add(box, bott, [{:border, 2}, {:flag, 64 ||| 128 ||| 32 ||| 16 ||| 8192}])
    :wxWindow.setSizer(dlg, box)
    _ = :wxSizer.fit(box, dlg)
    :wxSizer.setSizeHints(box, dlg)

    state =
      r_state(
        win: dlg,
        back: back,
        forward: forw,
        up: up,
        text: dir,
        list: lC,
        icons: iconMap,
        sort: {sortCol, false},
        filter: filter,
        path: path,
        files: files
      )

    {dlg, state}
  end

  def handle_call(:getFilename, _From, state = r_state(list: lC, files: fs)) do
    case :wxListCtrl.getNextItem(lC, -1, [{:state, 4}]) do
      -1 ->
        {:reply, '', state}

      item ->
        {:reply, r_file(:lists.nth(item + 1, fs), :name), state}
    end
  end

  def handle_call(:getFilenames, _From, state = r_state(list: lC, files: fs)) do
    items = get_selection(lC, -1, [])

    files =
      for item <- items do
        r_file(:lists.nth(item + 1, fs), :name)
      end

    {:reply, files, state}
  end

  def handle_call(:getDirectory, _From, state = r_state(path: dir)) do
    {:reply, dir, state}
  end

  def handle_call(:destroy, _From, state) do
    {:stop, :normal, :ok, state}
  end

  def handle_cast(_, state) do
    {:noreply, state}
  end

  def handle_event(r_wx(id: 5120), state0) do
    state = update_window(change_dir(0, state0))
    {:noreply, state}
  end

  def handle_event(
        r_wx(id: 5107),
        state0 = r_state(rstack: [prev | stack])
      ) do
    state = update_window(change_dir(prev, r_state(state0, rstack: stack), :forward))
    {:noreply, state}
  end

  def handle_event(
        r_wx(id: 5106),
        state0 = r_state(fstack: [prev | stack])
      ) do
    state = update_window(change_dir(prev, r_state(state0, fstack: stack), :reverse))
    {:noreply, state}
  end

  def handle_event(r_wx(id: id = 5101), state = r_state(win: win)) do
    :wxDialog.endModal(win, id)
    {:noreply, state}
  end

  def handle_event(r_wx(id: id = 5100), state = r_state(win: win)) do
    :wxDialog.endModal(win, id)
    {:noreply, state}
  end

  def handle_event(
        r_wx(
          event:
            r_wxList(
              type: :command_list_col_click,
              col: column0
            )
        ),
        state0 = r_state(files: fs, sort: sort0)
      ) do
    case column0 >= 0 do
      true ->
        column = sort_col(column0 + 1)

        sort =
          case sort0 do
            {^column, bool} ->
              {column, not bool}

            {_, _} ->
              {column, false}
          end

        {:noreply,
         update_window(
           r_state(state0,
             files: sort_files(fs, sort),
             sort: sort
           )
         )}

      false ->
        {:noreply, state0}
    end
  end

  def handle_event(
        r_wx(event: r_wxList(itemIndex: index)),
        state0 = r_state(files: fs, win: win)
      ) do
    case :lists.nth(index + 1, fs) do
      r_file(type: 'directory') ->
        state = update_window(change_dir(index, state0))
        {:noreply, state}

      _Dbg = r_file() ->
        :wxDialog.endModal(win, 5100)
        {:noreply, state0}
    end
  end

  def handle_event(
        r_wx(
          event:
            r_wxCommand(
              type: :command_text_updated,
              cmdString: wanted
            )
        ),
        state = r_state(ptext: previous, completion: comp)
      ) do
    case previous === :undefined or
           :lists.prefix(
             wanted,
             previous
           ) do
      true ->
        destroy_completion(comp)
        {:noreply, r_state(state, ptext: wanted, completion: :undefined)}

      false ->
        {:noreply, show_completion(wanted, state)}
    end
  end

  def handle_event(
        r_wx(
          event:
            r_wxCommand(
              type: :command_text_enter,
              cmdString: wanted
            )
        ),
        state
      ) do
    case :filelib.is_dir(wanted, :erl_prim_loader) do
      true ->
        {path0, dir} = split_dir(wanted)
        path = :filename.join(path0, dir)
        {:noreply, update_window(change_dir(path, state))}

      false ->
        {path, _} = split_dir(wanted)
        {:noreply, update_window(change_dir(path, state))}
    end
  end

  def handle_event(
        r_wx(event: r_wxKey(keyCode: 9)),
        state = r_state(text: tC, ptext: wanted, completion: comp)
      ) do
    case :wxTextCtrl.getSelection(tC) do
      {pos, pos} ->
        {:noreply, show_completion(wanted, state)}

      _ ->
        :wxTextCtrl.setInsertionPointEnd(tC)
        destroy_completion(comp)
        {:noreply, r_state(state, completion: :undefined)}
    end
  end

  def handle_event(r_wx(id: 201, event: r_wxCommand(cmdString: [])), state) do
    {:noreply, state}
  end

  def handle_event(
        r_wx(id: 201, obj: lB, event: _Ev = r_wxCommand(cmdString: dir, commandInt: n)),
        state = r_state(ptext: wanted0, text: tC)
      ) do
    case :wxListBox.isSelected(lB, n) do
      true ->
        wanted =
          case wanted0 do
            :undefined ->
              :wxTextCtrl.getValue(tC)

            _ ->
              wanted0
          end

        path1 =
          case :filelib.is_dir(
                 wanted,
                 :erl_prim_loader
               ) do
            true ->
              wanted

            false ->
              {thePath, _} = split_dir(wanted)
              thePath
          end

        path = :filename.join(path1, dir)
        {:noreply, update_window(change_dir(path, state))}

      false ->
        {:noreply, state}
    end
  end

  def handle_event(
        r_wx(event: r_wxSize(size: {width, _})),
        state = r_state(list: lC)
      ) do
    :wx.batch(fn ->
      tot =
        :wx.foldl(
          fn c, sum ->
            sum + :wxListCtrl.getColumnWidth(lC, c)
          end,
          0,
          [1, 2]
        )

      :wxListCtrl.setColumnWidth(lC, 0, width - tot - 30)
    end)

    {:noreply, state}
  end

  def handle_event(_Event, state) do
    {:noreply, state}
  end

  def handle_info(_Msg, state) do
    {:noreply, state}
  end

  def terminate(_Reason, state) do
    :wxDialog.destroy(r_state(state, :win))
    :ok
  end

  def code_change(_, _, state) do
    state
  end

  defp update_window(
         state =
           r_state(files: fs, list: lC, path: path, text: tC, icons: icons, completion: comp)
       ) do
    update_files(fs, lC, icons)
    update_dir(path, tC)

    cond do
      r_state(state, :rstack) == [] ->
        :wxButton.disable(r_state(state, :back))

      true ->
        :wxButton.enable(r_state(state, :back))
    end

    cond do
      r_state(state, :fstack) == [] ->
        :wxButton.disable(r_state(state, :forward))

      true ->
        :wxButton.enable(r_state(state, :forward))
    end

    cond do
      path == '/' ->
        :wxButton.disable(r_state(state, :up))

      true ->
        :wxButton.enable(r_state(state, :up))
    end

    destroy_completion(comp)
    r_state(state, completion: :undefined, ptext: :undefined)
  end

  defp update_dir(path, tC) do
    case path do
      '/' ->
        :wxTextCtrl.setValue(tC, path)

      _ ->
        :wxTextCtrl.setValue(tC, path ++ '/')
    end

    :wxTextCtrl.setInsertionPointEnd(tC)
  end

  defp update_files(files, lC, icons) do
    :wxListCtrl.deleteAllItems(lC)

    :wx.foldl(
      fn f = r_file(name: name, type: typeStr, date: date, color: color), row ->
        :wxListCtrl.insertItem(lC, row, '')

        cond do
          rem(row, 2) === 0 ->
            :wxListCtrl.setItemBackgroundColour(lC, row, {240, 240, 255})

          true ->
            :ignore
        end

        :wxListCtrl.setItemTextColour(lC, row, color)
        :wxListCtrl.setItem(lC, row, 0, name, [{:imageId, get_icon(f, icons)}])
        :wxListCtrl.setItem(lC, row, 2, format_date(date))
        :wxListCtrl.setItem(lC, row, 1, typeStr)
        row + 1
      end,
      0,
      files
    )
  end

  defp show_completion(:undefined, state = r_state(text: tC)) do
    show_completion(:wxTextCtrl.getValue(tC), state)
  end

  defp show_completion(
         wanted,
         state = r_state(text: tC, win: win, list: lC, completion: comp)
       ) do
    paths0 =
      :filelib.wildcard(
        wanted ++ '*',
        :erl_prim_loader
      )

    paths =
      for file <- paths0,
          :filelib.is_dir(file, :erl_prim_loader) do
        file
      end

    case paths do
      [path] ->
        start = length(wanted)
        :wxTextCtrl.setValue(tC, path ++ '/')
        :wxTextCtrl.setInsertionPoint(tC, start)
        last = :wxTextCtrl.getLastPosition(tC)
        :wxTextCtrl.setSelection(tC, start, last)
        destroy_completion(comp)
        r_state(state, ptext: path, completion: :undefined)

      ^paths when comp === :undefined ->
        {posX, posY} = :wxListCtrl.getPosition(lC)
        {szX, szY} = :wxListCtrl.getSize(lC)
        pos0 = {posX + 5, posY}
        size = {szX - 50, szY - 50}

        files =
          for file <- paths do
            :filename.basename(file)
          end

        temp =
          case :os.type() do
            {:win32, :nt} ->
              pos = :wxWindow.clientToScreen(win, pos0)
              :wxFrame.new(win, -1, '', [{:pos, pos}, {:size, size}, {:style, 8}])

            _ ->
              :wxWindow.new(win, -1, [{:pos, pos0}, {:size, size}, {:style, 8}])
          end

        lB = :wxListBox.new(temp, 201, [{:style, 32}, {:choices, files}, {:size, size}])
        :wxListBox.connect(lB, :command_listbox_selected)
        :wxWindow.show(temp)
        {start, last} = :wxTextCtrl.getSelection(tC)
        :wxWindow.setFocus(tC)
        :wxTextCtrl.setSelection(tC, start, last)
        r_state(state, completion: {temp, lB}, ptext: wanted)

      ^paths ->
        {_Temp, lB} = comp
        :wxListBox.clear(lB)

        files =
          for file <- paths do
            :filename.basename(file)
          end

        files != [] and :wxListBox.insertItems(lB, files, 0)
        r_state(state, ptext: wanted)
    end
  end

  defp destroy_completion(:undefined) do
    :ok
  end

  defp destroy_completion({window, _LB}) do
    parent = :wxWindow.getParent(window)
    :wxWindow.hide(window)
    :wxWindow.destroy(window)
    :wxWindow.refresh(parent)
  end

  defp split_dir(path0) do
    split1 = :filename.split(path0)

    case :lists.reverse(split1) do
      [file | split2] when split2 !== [] ->
        split3 = :lists.reverse(split2)
        path = :filename.join(split3)
        {path, file}

      _ ->
        {'/', ''}
    end
  end

  defp change_dir(what, state) do
    change_dir(what, state, :new)
  end

  defp change_dir(num, state = r_state(files: fs0, path: path0), stack)
       when is_integer(num) do
    case :lists.nth(num + 1, fs0) do
      r_file(name: '..') ->
        {path, _} = split_dir(path0)

      r_file(name: dir) ->
        path = :filename.join(path0, dir)
    end

    change_dir(path, state, stack)
  end

  defp change_dir(
         path,
         state0 = r_state(path: path0, sort: sort, filter: filter),
         stackDir
       ) do
    files = list_files(path, sort, filter)
    add_to_stack(stackDir, path0, r_state(state0, files: files, path: path))
  end

  defp add_to_stack(:new, path, state = r_state(rstack: stack0)) do
    stack = [path | stack0]
    r_state(state, rstack: stack, fstack: [])
  end

  defp add_to_stack(:reverse, path, state = r_state(rstack: stack0)) do
    stack = [path | stack0]
    r_state(state, rstack: stack)
  end

  defp add_to_stack(:forward, path, state = r_state(fstack: stack0)) do
    stack = [path | stack0]
    r_state(state, fstack: stack)
  end

  defp list_files(dir, sort, filter) do
    contents0 =
      :filelib.wildcard(
        dir ++ '/*',
        :erl_prim_loader
      )

    contents =
      case dir do
        '/' ->
          contents0

        _ ->
          ['..' | contents0]
      end

    {ds0, fs0} = get_file_info(contents, dir, filter, [], [])
    sort_files(:lists.reverse(ds0), fs0, sort)
  end

  defp sort_files(mixed, sort) do
    {ds, fs} =
      :lists.foldr(
        fn
          dir = r_file(type: 'directory'), {ds, fs} ->
            {[dir | ds], fs}

          file, {ds, fs} ->
            {ds, [file | fs]}
        end,
        {[], []},
        mixed
      )

    sort_files(ds, fs, sort)
  end

  defp sort_files(ds0, fs0, {sortElement, rev}) do
    {top, ds1} =
      case ds0 do
        [up = r_file(name: '..') | rest] ->
          {up, rest}

        _ ->
          {:undefined, ds0}
      end

    ds = :lists.keysort(sortElement, ds1)

    fs =
      case rev do
        true ->
          :lists.reverse(:lists.keysort(sortElement, fs0))

        false ->
          :lists.keysort(sortElement, fs0)
      end

    case top do
      :undefined ->
        ds ++ fs

      _ ->
        [top | ds ++ fs]
    end
  end

  defp get_file_info([absName | rest], dir, filter, files, dirs) do
    name = :filename.basename(absName)
    mod = :filelib.last_modified(absName, :erl_prim_loader)
    isDir = :filelib.is_dir(absName, :erl_prim_loader)
    entry0 = r_file(name: name, date: mod)

    case isDir do
      true when name === '..' ->
        entry = r_file(entry0, type: 'directory', icon: :prev_dir)
        get_file_info(rest, dir, filter, files, [entry | dirs])

      true ->
        entry = r_file(entry0, type: 'directory', icon: :dir)
        get_file_info(rest, dir, filter, files, [entry | dirs])

      false ->
        case filter.(dir, name) do
          {type, icon, color} ->
            entry = r_file(entry0, type: type, icon: icon, color: color)
            get_file_info(rest, dir, filter, [entry | files], dirs)

          :skip ->
            get_file_info(rest, dir, filter, files, dir)
        end
    end
  end

  defp get_file_info([], _, _, fs, ds) do
    {ds, fs}
  end

  defp format_date({{y, m, d}, {h, mi, s}}) do
    :lists.flatten(:io_lib.format('~w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w', [y, m, d, h, mi, s]))
  end

  defp format_date(_) do
    'unknown'
  end

  defp get_selection(lC, prev, acc) do
    case :wxListCtrl.getNextItem(lC, prev, [{:state, 4}]) do
      -1 ->
        :lists.reverse(acc)

      item ->
        get_selection(lC, item, [item | acc])
    end
  end

  defp file_type_and_icon(_Dir, name) do
    case :filename.extension(name) do
      '.erl' ->
        {'erl src', :erl_src, {0, 90, 0}}

      '.hrl' ->
        {'erl hrl', :erl_hrl, {0, 90, 0}}

      '.beam' ->
        {'erl bin', :erl_bin, {0, 0, 0}}

      _ ->
        {'file', :file, {0, 0, 0}}
    end
  end

  defp create_icons(extra) do
    art = :wxImageList.new(16, 16)

    builtIn0 = [
      {:file, 'wxART_NORMAL_FILE'},
      {:dir, 'wxART_FOLDER'},
      {:prev_dir, 'wxART_GO_DIR_UP'}
    ]

    builtIn =
      for {type, artID} <- builtIn0 do
        {type, :wxArtProvider.getBitmap(artID, [{:size, {16, 16}}])}
      end

    test =
      for {type, bin} <- [
            {:erl_src,
             <<255, 255, 255, 255, 255, 255, 255, 255, 255, 192, 192, 192, 192, 192, 192, 192,
               192, 192, 192, 192, 192, 192, 192, 192, 192, 192, 192, 192, 192, 192, 192, 192,
               192, 192, 192, 192, 160, 160, 160, 191, 191, 191, 255, 255, 255, 255, 255, 255,
               191, 191, 191, 128, 128, 128, 128, 128, 128, 96, 96, 96, 128, 128, 128, 128, 128,
               128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128,
               128, 128, 128, 192, 192, 192, 144, 144, 144, 191, 191, 191, 255, 255, 255, 128,
               128, 128, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
               128, 128, 128, 0, 0, 0, 0, 0, 0, 192, 192, 192, 192, 192, 192, 144, 144, 144, 191,
               191, 191, 128, 128, 128, 0, 0, 0, 64, 64, 64, 128, 128, 128, 128, 128, 128, 0, 0,
               0, 191, 191, 191, 128, 128, 128, 0, 0, 0, 128, 128, 128, 0, 0, 0, 0, 0, 0, 223,
               223, 223, 128, 128, 128, 128, 128, 128, 64, 64, 64, 128, 128, 128, 0, 0, 0, 128,
               128, 128, 128, 128, 128, 128, 128, 128, 0, 0, 0, 128, 128, 128, 0, 0, 0, 0, 0, 0,
               128, 128, 128, 0, 0, 0, 0, 0, 0, 255, 255, 255, 255, 255, 255, 255, 255, 255, 128,
               128, 128, 128, 128, 128, 0, 0, 0, 0, 0, 0, 128, 128, 128, 64, 64, 64, 0, 0, 0, 64,
               64, 64, 0, 0, 0, 0, 0, 0, 128, 128, 128, 0, 0, 0, 0, 0, 0, 255, 255, 255, 255, 255,
               255, 255, 255, 255, 128, 128, 128, 160, 160, 192, 64, 64, 128, 64, 64, 128, 64, 64,
               128, 64, 64, 128, 64, 64, 128, 64, 64, 128, 64, 64, 128, 64, 64, 128, 64, 64, 128,
               64, 64, 128, 64, 64, 128, 255, 255, 255, 255, 255, 255, 255, 255, 255, 128, 128,
               128, 239, 239, 239, 224, 224, 224, 224, 224, 224, 192, 192, 192, 224, 224, 224,
               224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224,
               224, 224, 224, 224, 224, 255, 255, 255, 255, 255, 255, 255, 255, 255, 128, 128,
               128, 255, 255, 255, 255, 255, 255, 255, 255, 255, 192, 192, 192, 255, 255, 255,
               192, 128, 128, 239, 239, 239, 255, 255, 255, 255, 255, 255, 239, 239, 239, 255,
               255, 255, 255, 255, 255, 255, 255, 255, 208, 176, 176, 223, 191, 191, 128, 128,
               128, 255, 255, 255, 255, 255, 255, 255, 255, 255, 192, 192, 192, 255, 255, 255,
               176, 112, 112, 255, 255, 255, 255, 255, 255, 223, 191, 191, 128, 0, 0, 160, 64, 64,
               255, 255, 255, 255, 255, 255, 239, 239, 239, 192, 128, 128, 128, 128, 128, 255,
               255, 255, 255, 255, 255, 255, 255, 255, 192, 192, 192, 255, 255, 255, 192, 128,
               128, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
               255, 255, 255, 255, 255, 255, 255, 255, 255, 192, 128, 128, 128, 128, 128, 255,
               255, 255, 255, 255, 255, 255, 255, 255, 192, 192, 192, 255, 255, 255, 192, 128,
               128, 255, 255, 255, 255, 255, 255, 223, 191, 191, 192, 128, 128, 192, 128, 128,
               192, 128, 128, 192, 128, 128, 192, 128, 128, 192, 128, 128, 128, 128, 128, 255,
               255, 255, 255, 255, 255, 255, 255, 255, 192, 192, 192, 255, 255, 255, 192, 128,
               128, 255, 255, 255, 255, 255, 255, 192, 128, 128, 128, 0, 0, 128, 0, 0, 128, 0, 0,
               144, 48, 48, 128, 0, 0, 192, 128, 128, 128, 128, 128, 255, 255, 255, 255, 255, 255,
               255, 255, 255, 192, 192, 192, 255, 255, 255, 176, 112, 112, 255, 255, 255, 255,
               255, 255, 239, 239, 239, 144, 48, 48, 128, 0, 0, 144, 48, 48, 239, 239, 239, 192,
               160, 160, 192, 128, 128, 128, 128, 128, 255, 255, 255, 255, 255, 255, 255, 255,
               255, 192, 192, 192, 255, 255, 255, 192, 128, 128, 239, 239, 239, 255, 255, 255,
               255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 208,
               176, 176, 223, 191, 191, 128, 128, 128, 255, 255, 255, 255, 255, 255, 255, 255,
               255, 192, 192, 192, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128,
               128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128,
               128, 128, 128, 128, 128, 64, 64, 64>>},
            {:erl_hrl,
             <<255, 255, 255, 255, 255, 255, 255, 255, 255, 192, 192, 192, 192, 192, 192, 192,
               192, 192, 192, 192, 192, 192, 192, 192, 192, 192, 192, 192, 192, 192, 192, 192,
               192, 192, 192, 192, 160, 160, 160, 191, 191, 191, 255, 255, 255, 255, 255, 255,
               191, 191, 191, 128, 128, 128, 128, 128, 128, 96, 96, 96, 128, 128, 128, 128, 128,
               128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128,
               128, 128, 128, 192, 192, 192, 144, 144, 144, 191, 191, 191, 255, 255, 255, 128,
               128, 128, 0, 0, 0, 128, 128, 128, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 128,
               128, 128, 0, 0, 0, 0, 0, 0, 0, 0, 0, 192, 192, 192, 192, 192, 192, 144, 144, 144,
               191, 191, 191, 128, 128, 128, 0, 0, 0, 128, 128, 128, 128, 128, 128, 64, 64, 64,
               128, 128, 128, 128, 128, 128, 64, 64, 64, 128, 128, 128, 0, 0, 0, 0, 0, 0, 0, 0, 0,
               223, 223, 223, 128, 128, 128, 128, 128, 128, 64, 64, 64, 128, 128, 128, 0, 0, 0,
               128, 128, 128, 0, 0, 0, 128, 128, 128, 128, 128, 128, 0, 0, 0, 0, 0, 0, 128, 128,
               128, 0, 0, 0, 0, 0, 0, 0, 0, 0, 255, 255, 255, 255, 255, 255, 255, 255, 255, 128,
               128, 128, 128, 128, 128, 0, 0, 0, 64, 64, 64, 0, 0, 0, 64, 64, 64, 64, 64, 64, 0,
               0, 0, 0, 0, 0, 64, 64, 64, 64, 64, 64, 0, 0, 0, 0, 0, 0, 255, 255, 255, 255, 255,
               255, 255, 255, 255, 128, 128, 128, 160, 160, 192, 64, 64, 128, 64, 64, 128, 64, 64,
               128, 64, 64, 128, 64, 64, 128, 64, 64, 128, 64, 64, 128, 64, 64, 128, 64, 64, 128,
               64, 64, 128, 64, 64, 128, 255, 255, 255, 255, 255, 255, 255, 255, 255, 128, 128,
               128, 239, 239, 239, 224, 224, 224, 224, 224, 224, 192, 192, 192, 224, 224, 224,
               224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224,
               224, 224, 224, 224, 224, 255, 255, 255, 255, 255, 255, 255, 255, 255, 128, 128,
               128, 255, 255, 255, 255, 255, 255, 255, 255, 255, 192, 192, 192, 255, 255, 255,
               192, 128, 128, 239, 239, 239, 255, 255, 255, 255, 255, 255, 239, 239, 239, 255,
               255, 255, 255, 255, 255, 255, 255, 255, 208, 176, 176, 223, 191, 191, 128, 128,
               128, 255, 255, 255, 255, 255, 255, 255, 255, 255, 192, 192, 192, 255, 255, 255,
               176, 112, 112, 255, 255, 255, 255, 255, 255, 223, 191, 191, 128, 0, 0, 160, 64, 64,
               255, 255, 255, 255, 255, 255, 239, 239, 239, 192, 128, 128, 128, 128, 128, 255,
               255, 255, 255, 255, 255, 255, 255, 255, 192, 192, 192, 255, 255, 255, 192, 128,
               128, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
               255, 255, 255, 255, 255, 255, 255, 255, 255, 192, 128, 128, 128, 128, 128, 255,
               255, 255, 255, 255, 255, 255, 255, 255, 192, 192, 192, 255, 255, 255, 192, 128,
               128, 255, 255, 255, 255, 255, 255, 223, 191, 191, 192, 128, 128, 192, 128, 128,
               192, 128, 128, 192, 128, 128, 192, 128, 128, 192, 128, 128, 128, 128, 128, 255,
               255, 255, 255, 255, 255, 255, 255, 255, 192, 192, 192, 255, 255, 255, 192, 128,
               128, 255, 255, 255, 255, 255, 255, 192, 128, 128, 128, 0, 0, 128, 0, 0, 128, 0, 0,
               144, 48, 48, 128, 0, 0, 192, 128, 128, 128, 128, 128, 255, 255, 255, 255, 255, 255,
               255, 255, 255, 192, 192, 192, 255, 255, 255, 176, 112, 112, 255, 255, 255, 255,
               255, 255, 239, 239, 239, 144, 48, 48, 128, 0, 0, 144, 48, 48, 239, 239, 239, 192,
               160, 160, 192, 128, 128, 128, 128, 128, 255, 255, 255, 255, 255, 255, 255, 255,
               255, 192, 192, 192, 255, 255, 255, 192, 128, 128, 239, 239, 239, 255, 255, 255,
               255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 208,
               176, 176, 223, 191, 191, 128, 128, 128, 255, 255, 255, 255, 255, 255, 255, 255,
               255, 192, 192, 192, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128,
               128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128,
               128, 128, 128, 128, 128, 64, 64, 64>>},
            {:erl_bin,
             <<255, 255, 255, 255, 255, 255, 255, 255, 255, 192, 192, 192, 192, 192, 192, 192,
               192, 192, 192, 192, 192, 192, 192, 192, 192, 192, 192, 192, 192, 192, 192, 192,
               192, 192, 192, 192, 160, 160, 160, 191, 191, 191, 255, 255, 255, 255, 255, 255,
               239, 239, 239, 224, 224, 224, 224, 224, 224, 192, 192, 192, 224, 224, 224, 224,
               224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224,
               224, 224, 224, 224, 192, 192, 192, 144, 144, 144, 191, 191, 191, 255, 255, 255,
               160, 160, 192, 64, 64, 128, 64, 64, 128, 64, 64, 128, 64, 64, 128, 64, 64, 128, 64,
               64, 128, 64, 64, 128, 64, 64, 128, 64, 64, 128, 64, 64, 128, 64, 64, 128, 192, 192,
               192, 192, 192, 192, 144, 144, 144, 191, 191, 191, 128, 128, 128, 64, 64, 64, 0, 0,
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
               223, 223, 223, 128, 128, 128, 128, 128, 128, 64, 64, 64, 128, 128, 128, 191, 191,
               191, 64, 64, 64, 0, 0, 0, 128, 128, 128, 0, 0, 0, 64, 64, 64, 128, 128, 128, 64,
               64, 64, 64, 64, 64, 64, 64, 64, 0, 0, 0, 255, 255, 255, 255, 255, 255, 255, 255,
               255, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128,
               128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128,
               128, 128, 128, 128, 128, 0, 0, 0, 255, 255, 255, 255, 255, 255, 255, 255, 255, 128,
               128, 128, 128, 128, 128, 191, 191, 191, 128, 128, 128, 64, 64, 64, 128, 128, 128,
               0, 0, 0, 128, 128, 128, 191, 191, 191, 128, 128, 128, 128, 128, 128, 128, 128, 128,
               0, 0, 0, 255, 255, 255, 255, 255, 255, 255, 255, 255, 128, 128, 128, 191, 191, 191,
               128, 128, 128, 128, 128, 128, 96, 96, 96, 128, 128, 128, 128, 128, 128, 128, 128,
               128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128,
               255, 255, 255, 255, 255, 255, 255, 255, 255, 128, 128, 128, 255, 255, 255, 255,
               255, 255, 255, 255, 255, 192, 192, 192, 255, 255, 255, 192, 128, 128, 239, 239,
               239, 255, 255, 255, 255, 255, 255, 239, 239, 239, 255, 255, 255, 255, 255, 255,
               255, 255, 255, 208, 176, 176, 223, 191, 191, 128, 128, 128, 255, 255, 255, 255,
               255, 255, 255, 255, 255, 192, 192, 192, 255, 255, 255, 176, 112, 112, 255, 255,
               255, 255, 255, 255, 223, 191, 191, 128, 0, 0, 160, 64, 64, 255, 255, 255, 255, 255,
               255, 239, 239, 239, 192, 128, 128, 128, 128, 128, 255, 255, 255, 255, 255, 255,
               255, 255, 255, 192, 192, 192, 255, 255, 255, 192, 128, 128, 255, 255, 255, 255,
               255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
               255, 255, 255, 255, 192, 128, 128, 128, 128, 128, 255, 255, 255, 255, 255, 255,
               255, 255, 255, 192, 192, 192, 255, 255, 255, 192, 128, 128, 255, 255, 255, 255,
               255, 255, 223, 191, 191, 192, 128, 128, 192, 128, 128, 192, 128, 128, 192, 128,
               128, 192, 128, 128, 192, 128, 128, 128, 128, 128, 255, 255, 255, 255, 255, 255,
               255, 255, 255, 192, 192, 192, 255, 255, 255, 192, 128, 128, 255, 255, 255, 255,
               255, 255, 192, 128, 128, 128, 0, 0, 128, 0, 0, 128, 0, 0, 144, 48, 48, 128, 0, 0,
               192, 128, 128, 128, 128, 128, 255, 255, 255, 255, 255, 255, 255, 255, 255, 192,
               192, 192, 255, 255, 255, 176, 112, 112, 255, 255, 255, 255, 255, 255, 239, 239,
               239, 144, 48, 48, 128, 0, 0, 144, 48, 48, 239, 239, 239, 192, 160, 160, 192, 128,
               128, 128, 128, 128, 255, 255, 255, 255, 255, 255, 255, 255, 255, 192, 192, 192,
               255, 255, 255, 192, 128, 128, 239, 239, 239, 255, 255, 255, 255, 255, 255, 255,
               255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 208, 176, 176, 223, 191,
               191, 128, 128, 128, 255, 255, 255, 255, 255, 255, 255, 255, 255, 192, 192, 192,
               128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128,
               128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128,
               128, 64, 64, 64>>}
          ] do
        {type, :wxBitmap.new(:wxImage.new(16, 16, bin))}
      end

    icons = builtIn ++ test ++ extra

    for {_, image} <- icons do
      :wxImageList.add(art, image)
    end

    {art, ids(icons, 0)}
  end

  defp get_icon(r_file(icon: icon), icons) do
    :proplists.get_value(icon, icons, 0)
  end

  defp ids([{type, _} | rest], id) do
    [{type, id} | ids(rest, id + 1)]
  end

  defp ids([], _) do
    []
  end

  defp sort_col(1) do
    r_file(:name)
  end

  defp sort_col(2) do
    r_file(:type)
  end

  defp sort_col(3) do
    r_file(:date)
  end

  defp sort_col(:name) do
    r_file(:name)
  end

  defp sort_col(:type) do
    r_file(:type)
  end

  defp sort_col(:date) do
    r_file(:date)
  end
end
