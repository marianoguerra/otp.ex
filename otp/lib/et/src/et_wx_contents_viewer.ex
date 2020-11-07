defmodule :m_et_wx_contents_viewer do
  use Bitwise
  @behaviour :wx_object
  require Record

  Record.defrecord(:r_event, :event,
    detail_level: :undefined,
    trace_ts: :undefined,
    event_ts: :undefined,
    from: :undefined,
    to: :undefined,
    label: :undefined,
    contents: :undefined
  )

  Record.defrecord(:r_filter, :filter,
    name: :undefined,
    function: :undefined
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

  Record.defrecord(:r_state, :state,
    parent_pid: :undefined,
    viewer_pid: :undefined,
    event_order: :undefined,
    event: :undefined,
    filtered_event: :undefined,
    active_filter: :undefined,
    filters: :undefined,
    win: :undefined,
    frame: :undefined,
    panel: :undefined,
    width: :undefined,
    height: :undefined,
    editor: :undefined,
    menu_data: :undefined,
    wx_debug: :undefined,
    trap_exit: :undefined
  )

  def start_link(options) do
    case parse_opt(options, default_state()) do
      {:ok, s} ->
        try do
          wxRef = :wx_object.start_link(:et_wx_contents_viewer, [s], [])
          pid = :wx_object.get_pid(wxRef)

          cond do
            r_state(s, :parent_pid) !== self() ->
              :erlang.unlink(pid)

            true ->
              :ok
          end

          {:ok, pid}
        catch
          :error, reason ->
            {:error, {:EXIT, reason, __STACKTRACE__}}
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp default_state() do
    r_state(
      parent_pid: self(),
      viewer_pid: :undefined,
      active_filter: :all,
      filters: [
        r_filter(
          name: :all,
          function: fn e ->
            e
          end
        )
      ],
      width: 600,
      height: 300,
      wx_debug: 0,
      trap_exit: true
    )
  end

  defp parse_opt([], s) do
    name = r_state(s, :active_filter)
    filters = r_state(s, :filters)

    cond do
      r_state(s, :event) === :undefined ->
        {:error, {:badarg, :no_event}}

      is_atom(name) ->
        case :lists.keysearch(name, r_filter(:name), filters) do
          {:value, f} when elem(f, 0) === :filter ->
            {:ok, r_state(s, active_filter: name)}

          false ->
            {:error, {:badarg, {:no_such_filter, name, filters}}}
        end
    end
  end

  defp parse_opt([h | t], s) do
    case h do
      {:parent_pid, parentPid}
      when is_pid(parentPid) or
             parentPid === :undefined ->
        parse_opt(t, r_state(s, parent_pid: parentPid))

      {:viewer_pid, viewerPid} when is_pid(viewerPid) ->
        parse_opt(t, r_state(s, viewer_pid: viewerPid))

      {:wx_debug, level} ->
        parse_opt(t, r_state(s, wx_debug: level))

      {:trap_exit, bool} when bool === true or bool === false ->
        parse_opt(t, r_state(s, trap_exit: bool))

      {:event_order, :trace_ts} ->
        parse_opt(t, r_state(s, event_order: :trace_ts))

      {:event_order, :event_ts} ->
        parse_opt(t, r_state(s, event_order: :event_ts))

      {:event, event} when elem(event, 0) === :event ->
        parse_opt(t, r_state(s, event: event))

      {:active_filter, name} when is_atom(name) ->
        parse_opt(t, r_state(s, active_filter: name))

      f
      when elem(f, 0) === :filter and
             is_atom(r_filter(f, :name)) and is_function(r_filter(f, :function)) ->
        filters = :lists.keydelete(r_filter(f, :name), r_filter(:name), r_state(s, :filters))
        filters2 = :lists.keysort(r_filter(:name), [f | filters])
        parse_opt(t, r_state(s, filters: filters2))

      {:width, width} when is_integer(width) and width > 0 ->
        parse_opt(t, r_state(s, width: width))

      {:height, height}
      when is_integer(height) and
             height > 0 ->
        parse_opt(t, r_state(s, height: height))

      bad ->
        {:error, {:bad_option, bad}}
    end
  end

  defp parse_opt(badList, _S) do
    {:error, {:bad_option_list, badList}}
  end

  def stop(contentsPid) when is_pid(contentsPid) do
    type = :process
    monitorRef = :erlang.monitor(type, contentsPid)
    send(contentsPid, {:stop, self()})

    receive do
      {:DOWN, ^monitorRef, ^type, ^contentsPid, :shutdown} ->
        :ok

      {:DOWN, ^monitorRef, ^type, ^contentsPid, reason} ->
        {:error, reason}
    end
  end

  def init([s]) when elem(s, 0) === :state do
    :erlang.process_flag(:trap_exit, r_state(s, :trap_exit))

    case r_state(s, :parent_pid) do
      :undefined ->
        :ok

      parentPid ->
        :erlang.link(parentPid)
    end

    :wx.debug(r_state(s, :wx_debug))
    s2 = create_window(s)
    {r_state(s2, :frame), s2}
  end

  def handle_call(request, from, s) do
    :ok =
      :error_logger.format(
        '~p(~p): handle_call(~tp, ~tp, ~tp)~n',
        [:et_wx_contents_viewer, self(), request, from, s]
      )

    reply = {:error, {:bad_request, request}}
    {:reply, reply, s}
  end

  def handle_cast(msg, s) do
    :ok =
      :error_logger.format(
        '~p(~p): handle_cast(~tp, ~tp)~n',
        [:et_wx_contents_viewer, self(), msg, s]
      )

    {:noreply, s}
  end

  def handle_event(
        r_wx(
          id: id,
          event: r_wxCommand(type: :command_menu_selected)
        ),
        s
      ) do
    case :proplists.get_value(id, r_state(s, :menu_data)) do
      :undefined ->
        :ok

      data when elem(data, 0) === :filter ->
        f = data
        childState = r_state(s, active_filter: r_filter(f, :name))
        _ = :wx_object.start_link(:et_wx_contents_viewer, [childState], [])
        :ok

      {:hide, actors} ->
        send_viewer_event(s, {:delete_actors, actors})

      {:show, actors} ->
        send_viewer_event(s, {:insert_actors, actors})

      {:mode, mode} ->
        send_viewer_event(s, {:mode, mode})

      nyi ->
        :ok =
          :error_logger.format(
            '~p: click ~p ignored (nyi)~n',
            [:et_wx_contents_viewer, nyi]
          )
    end

    case id do
      5006 ->
        :wxFrame.destroy(r_state(s, :frame))
        opt_unlink(r_state(s, :parent_pid))
        {:stop, :shutdown, s}

      5003 ->
        event = r_state(s, :event)

        timeStamp =
          case r_state(s, :event_order) do
            :trace_ts ->
              r_event(event, :trace_ts)

            :event_ts ->
              r_event(event, :event_ts)
          end

        fileName = :lists.flatten(['et_contents_viewer_', now_to_string(timeStamp), '.txt'])
        style = 2 ||| 4
        msg = 'Select a file to save events to'

        case select_file(r_state(s, :frame), msg, :filename.absname(fileName), style) do
          {:ok, fileName2} ->
            eventString = event_to_string(event, r_state(s, :event_order))
            bin = :unicode.characters_to_binary(eventString)
            :ok = :file.write_file(fileName2, bin)

          :cancel ->
            :ok
        end

        {:noreply, s}

      5010 ->
        html = :wxHtmlEasyPrinting.new([{:parentWindow, r_state(s, :win)}])
        text = '<pre>' ++ :wxTextCtrl.getValue(r_state(s, :editor)) ++ '</pre>'
        :wxHtmlEasyPrinting.previewText(html, text)
        {:noreply, s}

      _ ->
        {:noreply, s}
    end
  end

  def handle_event(r_wx(event: r_wxKey(rawCode: keyCode)), s) do
    case keyCode do
      ?c ->
        :wxFrame.destroy(r_state(s, :frame))
        opt_unlink(r_state(s, :parent_pid))
        {:stop, :normal, s}

      ?f ->
        e = r_state(s, :filtered_event)
        from = r_event(e, :from)
        send_viewer_event(s, {:delete_actors, [from]})
        {:noreply, s}

      ?t ->
        e = r_state(s, :filtered_event)
        to = r_event(e, :to)
        send_viewer_event(s, {:delete_actors, [to]})
        {:noreply, s}

      ?b ->
        e = r_state(s, :filtered_event)
        from = r_event(e, :from)
        to = r_event(e, :to)
        send_viewer_event(s, {:delete_actors, [from, to]})
        {:noreply, s}

      ?F ->
        e = r_state(s, :filtered_event)
        from = r_event(e, :from)
        send_viewer_event(s, {:insert_actors, [from]})
        {:noreply, s}

      ?T ->
        e = r_state(s, :filtered_event)
        to = r_event(e, :to)
        send_viewer_event(s, {:insert_actors, [to]})
        {:noreply, s}

      ?B ->
        e = r_state(s, :filtered_event)
        from = r_event(e, :from)
        to = r_event(e, :to)
        send_viewer_event(s, {:insert_actors, [from, to]})
        {:noreply, s}

      ?s ->
        e = r_state(s, :filtered_event)
        from = r_event(e, :from)
        to = r_event(e, :to)
        first = :et_collector.make_key(r_state(s, :event_order), e)
        mode = {:search_actors, :forward, first, [from, to]}
        send_viewer_event(s, {:mode, mode})
        {:noreply, s}

      ?r ->
        e = r_state(s, :filtered_event)
        from = r_event(e, :from)
        to = r_event(e, :to)
        first = :et_collector.make_key(r_state(s, :event_order), e)
        mode = {:search_actors, :reverse, first, [from, to]}
        send_viewer_event(s, {:mode, mode})
        {:noreply, s}

      ?a ->
        send_viewer_event(s, {:mode, :all})
        {:noreply, s}

      ?0 ->
        case :lists.keysearch(:all, r_filter(:name), r_state(s, :filters)) do
          {:value, f} when elem(f, 0) === :filter ->
            childState = r_state(s, active_filter: r_filter(f, :name))
            _ = :wx_object.start_link(:et_wx_contents_viewer, [childState], [])
            :ok

          false ->
            :ok
        end

        {:noreply, s}

      int when is_integer(int) and int > ?0 and int <= ?9 ->
        case (try do
                :lists.nth(int - ?0, r_state(s, :filters))
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          f when elem(f, 0) === :filter ->
            childState = r_state(s, active_filter: r_filter(f, :name))
            _ = :wx_object.start_link(:et_wx_contents_viewer, [childState], [])
            :ok

          {:EXIT, _} ->
            :ok
        end

        {:noreply, s}

      _ ->
        :io.format('~p: ignored: ~p~n', [:et_wx_contents_viewer, keyCode])
        {:noreply, s}
    end
  end

  def handle_event(r_wx(event: r_wxClose()), s) do
    opt_unlink(r_state(s, :parent_pid))
    {:stop, :shutdown, s}
  end

  def handle_event(r_wx(event: r_wxSize(size: {w, h})), s) do
    s2 = r_state(s, width: w, height: h)
    {:noreply, s2}
  end

  def handle_event(wx = r_wx(), s) do
    :io.format('~p got an unexpected event: ~tp\n', [self(), wx])
    {:noreply, s}
  end

  def handle_info({:stop, _From}, s) do
    :wxFrame.destroy(r_state(s, :frame))
    opt_unlink(r_state(s, :parent_pid))
    {:stop, :shutdown, s}
  end

  def handle_info({:EXIT, pid, reason}, s) do
    cond do
      pid === r_state(s, :parent_pid) ->
        :wxFrame.destroy(r_state(s, :frame))
        opt_unlink(r_state(s, :parent_pid))
        {:stop, reason, s}

      true ->
        {:noreply, s}
    end
  end

  def handle_info(info, s) do
    :ok =
      :error_logger.format(
        '~p(~p): handle_info(~tp, ~tp)~n',
        [:et_wx_contents_viewer, self(), info, s]
      )

    {:noreply, s}
  end

  def terminate(_Reason, _S) do
    :ignore
  end

  def code_change(_OldVsn, s, _Extra) do
    {:ok, s}
  end

  defp opt_unlink(pid) do
    cond do
      pid === :undefined ->
        :ignore

      true ->
        :erlang.unlink(pid)
    end
  end

  defp create_window(s) do
    h = r_state(s, :height)
    w = r_state(s, :width)
    name = r_state(s, :active_filter)
    title = :lists.concat([:et_wx_contents_viewer, ' (filter: ', name, ')'])
    winOpt = [{:size, {w, h}}]
    frame = :wxFrame.new(:wx.null(), -1, title, winOpt)
    _ = :wxFrame.createStatusBar(frame)
    panel = :wxPanel.new(frame, [])
    bar = :wxMenuBar.new()
    _ = :wxFrame.setMenuBar(frame, bar)
    create_file_menu(bar)

    editor =
      :wxTextCtrl.new(panel, -1, [{:style, 0 ||| 70 ||| 32768 ||| 32 ||| 16 ||| 1_073_741_824}])

    font = :wxFont.new(10, 76, 90, 90, [])

    textAttr =
      :wxTextAttr.new(
        :wxe_util.get_const(:wxBLACK),
        [{:font, font}]
      )

    _ = :wxTextCtrl.setDefaultStyle(editor, textAttr)
    sizer = :wxBoxSizer.new(4)
    _ = :wxSizer.add(sizer, editor, [{:flag, 8192}, {:proportion, 1}])
    filteredEvent = config_editor(editor, s)
    s2 = r_state(s, win: frame, panel: panel, filtered_event: filteredEvent)
    hideData = create_hide_menu(bar, s2)
    searchData = create_search_menu(bar, s2)
    filterData = create_filter_menu(bar, r_state(s, :filters))
    _ = :wxFrame.connect(frame, :command_menu_selected, [])
    _ = :wxFrame.connect(frame, :key_up)
    _ = :wxFrame.connect(frame, :close_window, [{:skip, true}])
    _ = :wxFrame.setFocus(frame)
    _ = :wxPanel.setSizer(panel, sizer)
    _ = :wxSizer.fit(sizer, panel)
    _ = :wxFrame.show(frame)
    r_state(s2, menu_data: hideData ++ searchData ++ filterData, editor: editor, frame: frame)
  end

  defp menuitem(menu, id, text, userData) do
    item = :wxMenu.append(menu, id, text)
    {:wxMenuItem.getId(item), userData}
  end

  defp create_file_menu(bar) do
    menu = :wxMenu.new([])
    _ = :wxMenu.append(menu, 5003, 'Save')
    _ = :wxMenu.append(menu, 5010, 'Print')
    _ = :wxMenu.appendSeparator(menu)
    _ = :wxMenu.append(menu, 5006, 'Close')
    _ = :wxMenuBar.append(bar, menu, 'File')
    :ok
  end

  defp create_filter_menu(bar, filters) do
    menu = :wxMenu.new([])

    _ =
      :wxMenuItem.enable(
        :wxMenu.append(menu, -1, 'Select Filter'),
        [{:enable, false}]
      )

    _ = :wxMenu.appendSeparator(menu)

    item = fn
      f, {n, acc} when r_filter(f, :name) === :all ->
        label = :lists.concat([pad_string(r_filter(f, :name), 20, ?\s, :right), '(0)'])
        menuItem = menuitem(menu, -1, label, f)
        {n + 1, [menuItem | acc]}

      f, {n, acc} ->
        name = r_filter(f, :name)
        label = :lists.concat([pad_string(name, 20, ?\s, :right), '(', n, ')'])
        menuItem = menuitem(menu, -1, label, f)
        {n + 1, [menuItem | acc]}
    end

    filters2 = :lists.keysort(r_filter(:name), filters)
    {_, menuData} = :lists.foldl(item, {1, []}, filters2)
    _ = :wxMenuBar.append(bar, menu, 'Filters')
    menuData
  end

  defp create_hide_menu(bar, s) do
    menu = :wxMenu.new([])
    e = r_state(s, :filtered_event)
    from = r_event(e, :from)
    to = r_event(e, :to)

    menuData =
      cond do
        r_state(s, :viewer_pid) === :undefined ->
          :ignore

        from === to ->
          _ =
            :wxMenuItem.enable(
              :wxMenu.append(menu, -1, 'Hide actor in Viewer '),
              [{:enable, false}]
            )

          _ = :wxMenu.appendSeparator(menu)
          hide = menuitem(menu, -1, 'From=To (f|t|b)', {:hide, [from]})
          _ = :wxMenu.appendSeparator(menu)

          _ =
            :wxMenuItem.enable(
              :wxMenu.append(menu, -1, 'Show actor in Viewer '),
              [{:enable, false}]
            )

          _ = :wxMenu.appendSeparator(menu)
          show = menuitem(menu, -1, 'From=To (F|T|B)', {:show, [from]})
          [show, hide]

        true ->
          _ =
            :wxMenuItem.enable(
              :wxMenu.append(menu, -1, 'Hide actor in Viewer '),
              [{:enable, false}]
            )

          _ = :wxMenu.appendSeparator(menu)

          hide = [
            menuitem(menu, -1, 'From (f)', {:hide, [from]}),
            menuitem(menu, -1, 'To   (t)', {:hide, [to]}),
            menuitem(menu, -1, 'Both (b)', {:hide, [from, to]})
          ]

          _ = :wxMenu.appendSeparator(menu)

          _ =
            :wxMenuItem.enable(
              :wxMenu.append(menu, -1, 'Show actor in Viewer '),
              [{:enable, false}]
            )

          _ = :wxMenu.appendSeparator(menu)

          show = [
            menuitem(menu, -1, 'From (F)', {:show, [from]}),
            menuitem(menu, -1, 'To   (T)', {:show, [to]}),
            menuitem(menu, -1, 'Both (B)', {:show, [from, to]})
          ]

          show ++ hide
      end

    _ = :wxMenuBar.append(bar, menu, 'Hide')
    menuData
  end

  defp create_search_menu(bar, s) do
    menu = :wxMenu.new([])
    e = r_state(s, :filtered_event)
    from = r_event(e, :from)
    to = r_event(e, :to)

    _ =
      :wxMenuItem.enable(
        :wxMenu.append(menu, -1, 'Search in Viewer '),
        [{:enable, false}]
      )

    _ = :wxMenu.appendSeparator(menu)

    menuData =
      cond do
        r_state(s, :viewer_pid) === :undefined ->
          [menuitem(menu, -1, 'Abort search. Display all (a)', {:mode, :all})]

        from === to ->
          key = :et_collector.make_key(r_state(s, :event_order), e)
          modeS = {:search_actors, :forward, key, [from]}
          modeR = {:search_actors, :reverse, key, [from]}

          [
            menuitem(menu, -1, 'Forward from this event   (s)', {:mode, modeS}),
            menuitem(menu, -1, 'Reverse from this event   (r)', {:mode, modeR}),
            menuitem(menu, -1, 'Abort search. Display all (a)', {:mode, :all})
          ]

        true ->
          key = :et_collector.make_key(r_state(s, :event_order), e)
          modeS = {:search_actors, :forward, key, [from, to]}
          modeR = {:search_actors, :reverse, key, [from, to]}

          [
            menuitem(menu, -1, 'Forward from this event   (s)', {:mode, modeS}),
            menuitem(menu, -1, 'Reverse from this event   (r)', {:mode, modeR}),
            menuitem(menu, -1, 'Abort search. Display all (a)', {:mode, :all})
          ]
      end

    _ = :wxMenuBar.append(bar, menu, 'Search')
    menuData
  end

  defp config_editor(editor, s) do
    event = r_state(s, :event)
    name = r_state(s, :active_filter)
    {:value, f} = :lists.keysearch(name, r_filter(:name), r_state(s, :filters))
    filterFun = r_filter(f, :function)

    case (try do
            filterFun.(event)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      true ->
        do_config_editor(editor, event, :lightblue, r_state(s, :event_order))

      {true, event2} when elem(event2, 0) === :event ->
        do_config_editor(editor, event2, :lightblue, r_state(s, :event_order))

      false ->
        do_config_editor(editor, event, :red, r_state(s, :event_order))

      bad ->
        contents = {:bad_filter, name, bad}
        badEvent = r_event(event, contents: contents)
        do_config_editor(editor, badEvent, :red, r_state(s, :event_order))
    end
  end

  defp do_config_editor(editor, event, _Colour, tsKey) do
    string = event_to_string(event, tsKey)
    :wxTextCtrl.appendText(editor, string)
    event
  end

  defp term_to_string(term) do
    case (try do
            :io_lib.format('~ts', [term])
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        :io_lib.format('~tp', [term])

      goodString ->
        goodString
    end
  end

  defp now_to_string({mega, sec, micro} = now)
       when is_integer(mega) and is_integer(sec) and
              is_integer(micro) do
    {{y, mo, d}, {h, mi, s}} = :calendar.now_to_universal_time(now)

    :lists.concat([
      y,
      '-',
      pad_string(mo, 2, ?0, :left),
      '-',
      pad_string(d, 2, ?0, :left),
      'T',
      pad_string(h, 2, ?0, :left),
      ':',
      pad_string(mi, 2, ?0, :left),
      ':',
      pad_string(s, 2, ?0, :left),
      '.',
      micro
    ])
  end

  defp now_to_string(other) do
    term_to_string(other)
  end

  defp event_to_string(event, tsKey) do
    reportedTs = r_event(event, :trace_ts)
    parsedTs = r_event(event, :event_ts)

    deep = [
      'DETAIL LEVEL: ',
      term_to_string(r_event(event, :detail_level)),
      '\nLABEL:        ',
      term_to_string(r_event(event, :label)),
      case r_event(event, :from) === r_event(event, :to) do
        true ->
          ['\nACTOR:        ', term_to_string(r_event(event, :from))]

        false ->
          [
            '\nFROM:         ',
            term_to_string(r_event(event, :from)),
            '\nTO:           ',
            term_to_string(r_event(event, :to))
          ]
      end,
      case reportedTs === parsedTs do
        true ->
          ['\nPARSED:       ', now_to_string(parsedTs)]

        false ->
          case tsKey do
            :trace_ts ->
              [
                '\nTRACE_TS:     ',
                now_to_string(reportedTs),
                '\nEVENT_TS:     ',
                now_to_string(parsedTs)
              ]

            :event_ts ->
              [
                '\nEVENT_TS:     ',
                now_to_string(parsedTs),
                '\nTRACE_TS:     ',
                now_to_string(reportedTs)
              ]
          end
      end,
      '\nCONTENTS:\n\n',
      term_to_string(r_event(event, :contents))
    ]

    :lists.flatten(deep)
  end

  defp pad_string(int, minLen, char, dir) when is_integer(int) do
    pad_string(:erlang.integer_to_list(int), minLen, char, dir)
  end

  defp pad_string(atom, minLen, char, dir) when is_atom(atom) do
    pad_string(:erlang.atom_to_list(atom), minLen, char, dir)
  end

  defp pad_string(string, minLen, char, dir)
       when is_integer(minLen) and minLen >= 0 do
    len = :string.length(string)

    case {len >= minLen, dir} do
      {true, _} ->
        string

      {false, :right} ->
        string ++ :lists.duplicate(minLen - len, char)

      {false, :left} ->
        :lists.duplicate(minLen - len, char) ++ string
    end
  end

  defp send_viewer_event(s, event) do
    case r_state(s, :viewer_pid) do
      viewerPid when is_pid(viewerPid) ->
        send(viewerPid, {:et, event})
        :ok

      :undefined ->
        :ok
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
end
