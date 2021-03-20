defmodule :m_et_wx_viewer do
  use Bitwise
  @behaviour :gen_server
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
    auto_shutdown: :undefined,
    collector_pid: :undefined,
    event_order: :undefined,
    trace_pattern: :undefined,
    active_filter: :undefined,
    filters: :undefined,
    filter_menu: :undefined,
    pending_actor: :undefined,
    first_event: :undefined,
    last_event: :undefined,
    events_per_page: :undefined,
    events: :undefined,
    n_events: :undefined,
    max_actors: :undefined,
    actors: :undefined,
    refresh_needed: :undefined,
    detail_level: :undefined,
    hide_actions: :undefined,
    hide_actors: :undefined,
    display_all: :undefined,
    context: :undefined,
    title: :undefined,
    frame: :undefined,
    menubar: :undefined,
    packer: :undefined,
    width: :undefined,
    height: :undefined,
    scale: :undefined,
    normal_font: :undefined,
    bold_font: :undefined,
    pen: :undefined,
    brush: :undefined,
    print_psdd: :undefined,
    print_d: :undefined,
    canvas_width: :undefined,
    canvas_height: :undefined,
    canvas: :undefined,
    canvas_sizer: :undefined,
    scroll_bar: :undefined,
    y_pos: :undefined,
    menu_data: :undefined,
    checkbox_data: :undefined,
    hide_actions_box: :undefined,
    hide_actors_box: :undefined,
    status_bar: :undefined,
    event_file: :undefined,
    wx_debug: :undefined,
    trap_exit: :undefined
  )

  Record.defrecord(:r_actor, :actor,
    name: :undefined,
    string: :undefined,
    include: :undefined,
    exclude: :undefined
  )

  Record.defrecord(:r_e, :e, pos: :undefined, key: :undefined, event: :undefined)

  def start_link(options) do
    case parse_opt(options, default_state(), []) do
      {:ok, s, collectorOpt} ->
        case r_state(s, :collector_pid) do
          collectorPid when is_pid(collectorPid) ->
            case :gen_server.start_link(:et_wx_viewer, [s], []) do
              {:ok, pid} when r_state(s, :parent_pid) !== self() ->
                :erlang.unlink(pid)
                {:ok, pid}

              other ->
                other
            end

          :undefined ->
            case :et_collector.start_link([
                   {:auto_shutdown, true}
                   | collectorOpt
                 ]) do
              {:ok, collectorPid} ->
                s2 = r_state(s, collector_pid: collectorPid)

                case :gen_server.start_link(:et_wx_viewer, [s2], []) do
                  {:ok, pid} when r_state(s, :parent_pid) !== self() ->
                    :erlang.unlink(pid)
                    {:ok, pid}

                  other ->
                    other
                end

              {:error, reason} ->
                {:error, {:et_collector, reason}}
            end
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp default_state() do
    r_state(
      parent_pid: self(),
      collector_pid: :undefined,
      n_events: 0,
      detail_level: 100,
      active_filter: :all,
      filters: [
        r_filter(
          name: :all,
          function: fn e ->
            e
          end
        )
      ],
      event_order: :trace_ts,
      events_per_page: 100,
      first_event: :first,
      last_event: :first,
      events: queue_new(),
      max_actors: 5,
      actors: [create_actor('UNKNOWN')],
      pending_actor: 'UNKNOWN',
      hide_actions: false,
      hide_actors: false,
      display_all: true,
      context: :display,
      refresh_needed: false,
      scale: 2,
      canvas_height: 0,
      canvas_width: 0,
      width: 800,
      height: 600,
      event_file: :filename.absname('et_viewer.etrace'),
      wx_debug: 0,
      trap_exit: true
    )
  end

  defp parse_opt([], s, collectorOpt) do
    {:ok, s, [{:parent_pid, r_state(s, :parent_pid)} | collectorOpt]}
  end

  defp parse_opt([h | t], s, collectorOpt) do
    case h do
      {:parent_pid, parent}
      when is_pid(parent) or
             parent === :undefined ->
        parse_opt(t, r_state(s, parent_pid: parent), collectorOpt)

      {:wx_debug, level} ->
        parse_opt(t, r_state(s, wx_debug: level), collectorOpt)

      {:trap_exit, bool} when bool === true or bool === false ->
        parse_opt(t, r_state(s, trap_exit: bool), collectorOpt)

      {:title, title} ->
        parse_opt(t, r_state(s, title: name_to_string(title)), collectorOpt)

      {:detail_level, level}
      when is_integer(level) and
             level >= 0 and level <= 100 ->
        parse_opt(t, r_state(s, detail_level: level), collectorOpt)

      {:detail_level, :max} ->
        parse_opt(t, r_state(s, detail_level: 100), collectorOpt)

      {:detail_level, :min} ->
        parse_opt(t, r_state(s, detail_level: 0), collectorOpt)

      {:scale, scale} when is_integer(scale) and scale > 0 ->
        parse_opt(t, r_state(s, scale: scale), collectorOpt)

      {:width, w} when is_integer(w) and w > 0 ->
        parse_opt(t, r_state(s, width: w, canvas_width: w), collectorOpt)

      {:height, wH} when is_integer(wH) and wH > 0 ->
        parse_opt(t, r_state(s, height: wH, canvas_height: wH), collectorOpt)

      {:collector_pid, pid} when is_pid(pid) ->
        parse_opt(t, r_state(s, collector_pid: pid), collectorOpt)

      {:collector_pid, :undefined} ->
        parse_opt(t, r_state(s, collector_pid: :undefined), collectorOpt)

      {:active_filter, name} when is_atom(name) ->
        parse_opt(t, r_state(s, active_filter: name), collectorOpt)

      {:event_order, :trace_ts} ->
        collectorOpt2 = [h | collectorOpt]
        parse_opt(t, r_state(s, event_order: :trace_ts), collectorOpt2)

      {:event_order, :event_ts} ->
        collectorOpt2 = [h | collectorOpt]
        parse_opt(t, r_state(s, event_order: :event_ts), collectorOpt2)

      {:trace_port, _Port} ->
        collectorOpt2 = [h | collectorOpt]
        parse_opt(t, s, collectorOpt2)

      {:trace_max_queue, _Queue} ->
        collectorOpt2 = [h | collectorOpt]
        parse_opt(t, s, collectorOpt2)

      {:trace_pattern, _Pattern} ->
        collectorOpt2 = [h | collectorOpt]
        parse_opt(t, s, collectorOpt2)

      {:trace_global, _Boolean} ->
        collectorOpt2 = [h | collectorOpt]
        parse_opt(t, s, collectorOpt2)

      {:trace_client, _Client} ->
        collectorOpt2 = [h | collectorOpt]
        parse_opt(t, s, collectorOpt2)

      {:dict_insert, {:filter, name}, fun} ->
        cond do
          is_atom(name) and is_function(fun) ->
            f = r_filter(name: name, function: fun)
            filters = :lists.keydelete(name, r_filter(:name), r_state(s, :filters))
            collectorOpt2 = [h | collectorOpt]
            parse_opt(t, r_state(s, filters: filters ++ [f]), collectorOpt2)

          true ->
            {:error, {:bad_option, h}}
        end

      {:dict_insert, {:subscriber, pid}, _Val} ->
        cond do
          is_pid(pid) ->
            collectorOpt2 = [h | collectorOpt]
            parse_opt(t, s, collectorOpt2)

          true ->
            {:error, {:bad_option, h}}
        end

      {:dict_insert, _Key, _Val} ->
        collectorOpt2 = [h | collectorOpt]
        parse_opt(t, s, collectorOpt2)

      {:dict_delete, {:filter, name}} ->
        filters = :lists.keydelete(name, r_filter(:name), r_state(s, :filters))
        collectorOpt2 = [h | collectorOpt]
        parse_opt(t, r_state(s, filters: filters), collectorOpt2)

      {:dict_delete, _Key} ->
        collectorOpt2 = [h | collectorOpt]
        parse_opt(t, s, collectorOpt2)

      {:max_events, _Max} ->
        parse_opt(t, s, collectorOpt)

      {:max_actors, max} when is_integer(max) and max >= 0 ->
        parse_opt(t, r_state(s, max_actors: max), collectorOpt)

      {:max_actors, max} when max === :infinity ->
        parse_opt(t, r_state(s, max_actors: max), collectorOpt)

      {:actors, actorNames} when is_list(actorNames) ->
        actorNames2 =
          case :lists.member('UNKNOWN', actorNames) do
            false ->
              ['UNKNOWN' | actorNames]

            true ->
              actorNames
          end

        actors =
          for name <- actorNames2 do
            create_actor(name)
          end

        parse_opt(t, r_state(s, actors: actors), collectorOpt)

      {:include, actorNames} when is_list(actorNames) ->
        actors =
          for name <- actorNames do
            opt_create_actor(name, :include, s)
          end

        parse_opt(t, r_state(s, actors: actors), collectorOpt)

      {:exclude, actorNames} when is_list(actorNames) ->
        actors =
          for name <- actorNames do
            opt_create_actor(name, :exclude, s)
          end

        parse_opt(t, r_state(s, actors: actors), collectorOpt)

      {:first_event, _FirstKey} ->
        parse_opt(t, s, collectorOpt)

      {:hide_actors, bool}
      when bool === true or
             bool === false ->
        parse_opt(t, r_state(s, hide_actors: bool), collectorOpt)

      {:hide_actions, bool}
      when bool === true or
             bool === false ->
        parse_opt(t, r_state(s, hide_actions: bool), collectorOpt)

      {:hide_unknown, bool}
      when bool === true or
             bool === false ->
        parse_opt(t, s, collectorOpt)

      {:display_mode, _Mode} ->
        parse_opt(t, s, collectorOpt)

      bad ->
        {:error, {:bad_option, bad}}
    end
  end

  defp parse_opt(badList, _S, _CollectorOpt) do
    {:error, {:bad_option_list, badList}}
  end

  defp do_dict_insert({:filter, name}, fun, s)
       when is_atom(name) and
              is_function(fun) do
    f = r_filter(name: name, function: fun)
    filters = :lists.keydelete(name, r_filter(:name), r_state(s, :filters))
    filters2 = :lists.keysort(r_filter(:name), [f | filters])
    s2 = create_filter_menu(s, r_state(s, :active_filter), filters2)
    r_state(s2, filters: filters2)
  end

  defp do_dict_insert(_Key, _Val, s) do
    s
  end

  defp do_dict_delete({:filter, name}, s)
       when is_atom(name) and
              name !== r_state(s, :active_filter) do
    filters = :lists.keydelete(name, r_filter(:name), r_state(s, :filters))
    s2 = create_filter_menu(s, r_state(s, :active_filter), filters)
    r_state(s2, filters: filters)
  end

  defp do_dict_delete(_Key, s) do
    s
  end

  def init([s]) when elem(s, 0) === :state do
    :erlang.process_flag(:trap_exit, r_state(s, :trap_exit))

    case r_state(s, :parent_pid) do
      :undefined ->
        :ok

      parentPid ->
        :erlang.link(parentPid)
    end

    _ = :wx.new()
    _ = :wx.debug(r_state(s, :wx_debug))
    :et_collector.dict_insert(r_state(s, :collector_pid), {:subscriber, self()}, :et_wx_viewer)
    s2 = create_main_window(s)
    eventsPerPage = events_per_page(s2, r_state(s2, :height))
    s3 = revert_main_window(r_state(s2, events_per_page: eventsPerPage))
    timeout = timeout(s3)
    {:ok, s3, timeout}
  end

  def handle_call(:get_collector_pid, _From, s) do
    reply = r_state(s, :collector_pid)
    reply(reply, s)
  end

  def handle_call(:stop, _From, s) do
    :wxFrame.destroy(r_state(s, :frame))
    opt_unlink(r_state(s, :parent_pid))
    {:stop, :shutdown, :ok, s}
  end

  def handle_call({:open_event, n}, _From, s)
      when is_integer(n) and n > 0 do
    reply = do_open_event(s, n)
    reply(reply, s)
  end

  def handle_call(request, from, s) do
    :ok =
      :error_logger.format(
        '~p(~p): handle_call(~tp, ~tp, ~tp)~n',
        [:et_wx_viewer, self(), request, from, s]
      )

    reply = {:error, {:bad_request, request}}
    reply(reply, s)
  end

  def handle_cast(msg, s) do
    :ok =
      :error_logger.format(
        '~p(~p): handle_cast(~tp, ~tp)~n',
        [:et_wx_viewer, self(), msg, s]
      )

    noreply(s)
  end

  def handle_info({:et, {:more_events, n}}, s) do
    s4 =
      cond do
        n === r_state(s, :n_events) ->
          s

        true ->
          missing = r_state(s, :events_per_page) - queue_length(r_state(s, :events))

          cond do
            missing === 0 ->
              update_scroll_bar(r_state(s, n_events: n))

            missing > 0 ->
              oldEvents = queue_to_list(r_state(s, :events))

              {s2, newEvents} =
                collect_more_events(
                  r_state(s, n_events: n),
                  r_state(s, :last_event),
                  missing
                )

              s3 = replace_events(s2, oldEvents ++ newEvents)
              refresh_main_window(s3)
          end
      end

    noreply(s4)
  end

  def handle_info({:et, {:insert_actors, actorNames}}, s)
      when is_list(actorNames) do
    fun = fn n, actors ->
      case :lists.keymember(n, r_actor(:name), actors) do
        true ->
          actors

        false ->
          actors ++ [create_actor(n)]
      end
    end

    actors = :lists.foldl(fun, r_state(s, :actors), actorNames)
    s2 = refresh_main_window(r_state(s, actors: actors))
    noreply(s2)
  end

  def handle_info({:et, {:delete_actors, actorNames}}, s)
      when is_list(actorNames) do
    fun = fn
      n, actors when n === 'UNKNOWN' ->
        actors

      n, actors ->
        :lists.keydelete(n, r_actor(:name), actors)
    end

    actors = :lists.foldl(fun, r_state(s, :actors), actorNames)
    s2 = refresh_main_window(r_state(s, actors: actors))
    noreply(s2)
  end

  def handle_info({:et, {:dict_insert, key, val}}, s) do
    s2 = do_dict_insert(key, val, s)
    noreply(s2)
  end

  def handle_info({:et, {:dict_delete, key}}, s) do
    s2 = do_dict_delete(key, s)
    noreply(s2)
  end

  def handle_info({:et, :first}, s) do
    s2 = scroll_first(s)
    noreply(s2)
  end

  def handle_info({:et, :prev}, s) do
    s2 = scroll_prev(s)
    noreply(s2)
  end

  def handle_info({:et, :next}, s) do
    s2 = scroll_next(s)
    noreply(s2)
  end

  def handle_info({:et, :last}, s) do
    s2 = scroll_last(s)
    noreply(s2)
  end

  def handle_info({:et, :refresh}, s) do
    s2 = revert_main_window(s)
    noreply(s2)
  end

  def handle_info({:et, {:display_mode, _Mode}}, s) do
    noreply(s)
  end

  def handle_info({:et, :close}, s) do
    :wxFrame.destroy(r_state(s, :frame))
    opt_unlink(r_state(s, :parent_pid))
    {:stop, :shutdown, s}
  end

  def handle_info(r_wx(id: 5009), s) do
    helpString =
      'Vertical scroll:\n\tUse mouse wheel and up/down arrows to scroll little.\n\tUse page up/down and home/end buttons to scroll more.\n\nDisplay details of an event:\n\tLeft mouse click on the event label or the arrow.\n\nHighlight actor (toggle):\n\tLeft mouse click on the actor name tag.\n\tThe actor name will be enclosed in square brackets [].\n\nExclude actor (toggle):\n\tRight mouse click on the actor name tag.\n\tThe actor name will be enclosed in round brackets ().\n\nMove actor:\n\tLeft mouse button drag and drop on actor name tag.\n\nDisplay all (reset settings for hidden and/or highlighted actors):\n\tPress the \'a\' button.'

    dialog =
      :wxMessageDialog.new(r_state(s, :frame), helpString, [
        {:style, 0 ||| 4 ||| 2048 ||| 32768},
        {:caption, 'Help'}
      ])

    :wxMessageDialog.showModal(dialog)
    noreply(s)
  end

  def handle_info(
        r_wx(
          id: id,
          event: r_wxCommand(type: :command_menu_selected)
        ),
        s = r_state(filter_menu: {_, data})
      ) do
    collectorPid = r_state(s, :collector_pid)

    case get_value(id, 3, r_state(s, :menu_data)) do
      :close ->
        :wxFrame.destroy(r_state(s, :frame))
        opt_unlink(r_state(s, :parent_pid))
        {:stop, :shutdown, s}

      :up ->
        s2 = scroll_up(s)
        noreply(s2)

      :down ->
        s2 = scroll_down(s)
        noreply(s2)

      :first ->
        s2 = scroll_first(s)
        noreply(s2)

      :prev ->
        s2 = scroll_prev(s)
        noreply(s2)

      :next ->
        s2 = scroll_next(s)
        noreply(s2)

      :last ->
        s2 = scroll_last(s)
        noreply(s2)

      :refresh ->
        s2 = revert_main_window(s)
        noreply(s2)

      {:display_mode, _Mode} ->
        noreply(s)

      :display_all ->
        s2 = display_all(s)
        noreply(s2)

      :close_all ->
        close_all(s)

      :close_all_others ->
        close_all_others(s)

      :first_all ->
        :et_collector.multicast(collectorPid, :first)
        noreply(s)

      :prev_all ->
        :et_collector.multicast(collectorPid, :prev)
        noreply(s)

      :next_all ->
        :et_collector.multicast(collectorPid, :next)
        noreply(s)

      :last_all ->
        :et_collector.multicast(collectorPid, :last)
        noreply(s)

      :refresh_all ->
        :et_collector.multicast(collectorPid, :refresh)
        noreply(s)

      :clear_all ->
        :et_collector.clear_table(collectorPid)
        :et_collector.multicast(collectorPid, :refresh)
        noreply(s)

      :load_all ->
        style = 1 ||| 4
        msg = 'Select a file to load events from'

        s2 =
          case select_file(r_state(s, :frame), msg, r_state(s, :event_file), style) do
            {:ok, newFile} ->
              _ = :et_collector.start_trace_client(collectorPid, :event_file, newFile)
              r_state(s, event_file: newFile)

            :cancel ->
              s
          end

        noreply(s2)

      :save_all ->
        style = 2 ||| 4
        msg = 'Select a file to save events to'

        s2 =
          case select_file(r_state(s, :frame), msg, r_state(s, :event_file), style) do
            {:ok, newFile} ->
              :ok =
                :et_collector.save_event_file(
                  collectorPid,
                  newFile,
                  [:existing, :write, :keep]
                )

              r_state(s, event_file: newFile)

            :cancel ->
              s
          end

        noreply(s2)

      :print_setup ->
        s2 = print_setup(s)
        noreply(s2)

      :print_one_page = scope ->
        s2 = print(s, scope)
        noreply(s2)

      :print_all_pages = scope ->
        s2 = print(s, scope)
        noreply(s2)

      {:open_viewer, scale} ->
        actors =
          for a <- r_state(s, :actors) do
            r_actor(a, :name)
          end

        open_viewer(scale, r_state(s, :active_filter), actors, s)
        noreply(s)

      _ ->
        case get_value(id, 3, data) do
          {:data, f = r_filter(), scale} ->
            open_viewer(r_state(s, :scale) + scale, r_filter(f, :name), ['UNKNOWN'], s)

          {:data, f = r_filter()} ->
            open_viewer(r_state(s, :scale), r_filter(f, :name), ['UNKNOWN'], s)

          false ->
            :ok
        end

        noreply(s)
    end
  end

  def handle_info(
        r_wx(
          event:
            r_wxCommand(
              type: :command_slider_updated,
              commandInt: level
            )
        ),
        s
      ) do
    cond do
      level >= 0 and level <= 100 ->
        s2 = r_state(s, detail_level: level)
        s3 = revert_main_window(s2)
        noreply(s3)

      true ->
        noreply(s)
    end
  end

  def handle_info(
        r_wx(
          id: id,
          event:
            r_wxCommand(
              type: :command_checkbox_clicked,
              commandInt: int
            )
        ),
        s
      ) do
    case get_value(id, 2, r_state(s, :checkbox_data)) do
      :hide_actions ->
        case int do
          1 ->
            s2 = r_state(s, hide_actions: true)
            s3 = revert_main_window(s2)
            noreply(s3)

          0 ->
            s2 = r_state(s, hide_actions: false)
            s3 = revert_main_window(s2)
            noreply(s3)
        end

      :hide_actors ->
        case int do
          1 ->
            s2 = r_state(s, hide_actors: true)
            s3 = revert_main_window(s2)
            noreply(s3)

          0 ->
            s2 = r_state(s, hide_actors: false)
            s3 = revert_main_window(s2)
            noreply(s3)
        end

      false ->
        noreply(s)
    end
  end

  def handle_info(r_wx(event: r_wxMouse(type: :left_down, x: x, y: y)), s) do
    s3 =
      case y_to_n(y, s) do
        :actor ->
          case r_state(s, :actors) do
            [] ->
              s

            actors ->
              n = x_to_n(x, s)
              a = :lists.nth(n, actors)
              r_state(s, pending_actor: a)
          end

        {:event, n} ->
          do_open_event(s, n)
          s
      end

    noreply(s3)
  end

  def handle_info(r_wx(event: r_wxMouse(type: :left_up)), s)
      when r_state(s, :pending_actor) === :undefined do
    noreply(s)
  end

  def handle_info(r_wx(event: r_wxMouse(type: :left_up, x: x, y: y)), s) do
    s3 =
      case y_to_n(y, s) do
        :actor ->
          case r_state(s, :actors) do
            [] ->
              s

            actors ->
              n = x_to_n(x, s)
              a = :lists.nth(n, actors)
              pending = r_state(s, :pending_actor)

              cond do
                r_actor(a, :name) === r_actor(pending, :name) ->
                  a2 = r_actor(a, include: not r_actor(a, :include))
                  actors2 = :lists.keyreplace(r_actor(a, :name), r_actor(:name), actors, a2)
                  displayAll = not :lists.keymember(true, r_actor(:include), actors2)
                  s2 = r_state(s, actors: actors2, display_all: displayAll)
                  revert_main_window(s2)

                true ->
                  move_actor(pending, a, actors, s)
              end
          end

        {:event, _N} ->
          s
      end

    noreply(r_state(s3, pending_actor: :undefined))
  end

  def handle_info(r_wx(event: r_wxMouse(type: :right_up, x: x, y: y)), s) do
    s3 =
      case y_to_n(y, s) do
        :actor ->
          case r_state(s, :actors) do
            [] ->
              s

            actors ->
              n = x_to_n(x, s)
              a = :lists.nth(n, actors)
              a2 = r_actor(a, exclude: not r_actor(a, :exclude))
              actors2 = :lists.keyreplace(r_actor(a, :name), r_actor(:name), actors, a2)
              s2 = r_state(s, actors: actors2)
              revert_main_window(s2)
          end

        {:event, _N} ->
          s
      end

    noreply(r_state(s3, pending_actor: :undefined))
  end

  def handle_info(
        r_wx(event: r_wxKey(keyCode: keyCode, shiftDown: sD)),
        s
      ) do
    case keyCode do
      ?C when sD === true ->
        close_all(s)

      ?c ->
        close_all_others(s)

      313 ->
        s2 = scroll_first(s)
        noreply(s2)

      312 ->
        s2 = scroll_last(s)
        noreply(s2)

      315 ->
        s2 = scroll_up(s)
        noreply(s2)

      317 ->
        s2 = scroll_down(s)
        noreply(s2)

      366 ->
        s2 = scroll_prev(s)
        noreply(s2)

      367 ->
        s2 = scroll_next(s)
        noreply(s2)

      ?F when sD === true ->
        :et_collector.multicast(r_state(s, :collector_pid), :first)
        noreply(s)

      ?F ->
        s2 = scroll_first(s)
        noreply(s2)

      ?P when sD === true ->
        :et_collector.multicast(r_state(s, :collector_pid), :prev)
        noreply(s)

      ?P ->
        s2 = scroll_prev(s)
        noreply(s2)

      ?N when sD === true ->
        :et_collector.multicast(r_state(s, :collector_pid), :next)
        noreply(s)

      ?N ->
        s2 = scroll_next(s)
        noreply(s2)

      ?L when sD === true ->
        :et_collector.multicast(r_state(s, :collector_pid), :last)
        noreply(s)

      ?L ->
        s2 = scroll_last(s)
        noreply(s2)

      ?R when sD === true ->
        :et_collector.multicast(r_state(s, :collector_pid), :refresh)
        noreply(s)

      ?R ->
        s2 = revert_main_window(s)
        noreply(s2)

      ?A ->
        s2 = display_all(s)
        noreply(s2)

      ?= ->
        scale = r_state(s, :scale)

        actors =
          for a <- r_state(s, :actors) do
            r_actor(a, :name)
          end

        open_viewer(scale, r_state(s, :active_filter), actors, s)
        noreply(s)

      int when int === ?+ or int === 388 ->
        scale = r_state(s, :scale) + 1

        actors =
          for a <- r_state(s, :actors) do
            r_actor(a, :name)
          end

        open_viewer(scale, r_state(s, :active_filter), actors, s)
        noreply(s)

      int when int === ?- or int === 390 ->
        case r_state(s, :scale) do
          1 ->
            :ignore

          scale ->
            actors =
              for a <- r_state(s, :actors) do
                r_actor(a, :name)
              end

            open_viewer(scale - 1, r_state(s, :active_filter), actors, s)
        end

        noreply(s)

      ?0 ->
        case :lists.keysearch(:all, r_filter(:name), r_state(s, :filters)) do
          {:value, f} when elem(f, 0) === :filter ->
            open_viewer(r_state(s, :scale), r_filter(f, :name), ['UNKNOWN'], s)

          false ->
            :ok
        end

        noreply(s)

      int when is_integer(int) and int > ?0 and int <= ?9 ->
        case (try do
                :lists.nth(int - ?0, r_state(s, :filters))
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          f when elem(f, 0) === :filter ->
            open_viewer(r_state(s, :scale), r_filter(f, :name), ['UNKNOWN'], s)

          {:EXIT, _} ->
            :ok
        end

        noreply(s)

      _ ->
        noreply(s)
    end
  end

  def handle_info(r_wx(event: r_wxScroll(type: :scroll_changed)) = wx, s) do
    _ = get_latest_scroll(wx)
    pos = :wxScrollBar.getThumbPosition(r_state(s, :scroll_bar))
    {_, lineTopY, lineBotY} = calc_y(s)
    range = lineBotY - lineTopY
    n = round(r_state(s, :n_events) * pos / range)

    diff =
      case n - event_pos(s) do
        d when d < 0 ->
          d

        d ->
          d
      end

    s2 = scroll_changed(s, diff)
    noreply(s2)
  end

  def handle_info(:timeout, s) do
    noreply(s)
  end

  def handle_info({:EXIT, pid, reason}, s) do
    cond do
      pid === r_state(s, :collector_pid) ->
        :io.format('collector died: ~tp\n\n', [reason])
        :wxFrame.destroy(r_state(s, :frame))
        {:stop, reason, s}

      pid === r_state(s, :parent_pid) ->
        :wxFrame.destroy(r_state(s, :frame))
        {:stop, reason, s}

      true ->
        noreply(s)
    end
  end

  def handle_info(r_wx(event: r_wxClose()), s) do
    opt_unlink(r_state(s, :parent_pid))
    {:stop, :shutdown, s}
  end

  def handle_info(
        r_wx(
          event:
            r_wxMouse(
              type: :mousewheel,
              wheelRotation: rot
            )
        ),
        s
      )
      when rot > 0 do
    s2 = scroll_up(s)
    noreply(s2)
  end

  def handle_info(
        r_wx(
          event:
            r_wxMouse(
              type: :mousewheel,
              wheelRotation: rot
            )
        ),
        s
      )
      when rot < 0 do
    s2 = scroll_down(s)
    noreply(s2)
  end

  def handle_info(r_wx(event: r_wxSize(size: {oldW, oldH})) = wx, s) do
    r_wx(
      event:
        r_wxSize(
          type: :size,
          size: {w, h}
        )
    ) = get_latest_resize(wx)

    s2 = r_state(s, width: w, height: h, canvas_width: w, canvas_height: h)
    eventsPerPage = events_per_page(s, h)
    diff = eventsPerPage - r_state(s, :events_per_page)

    s6 =
      cond do
        oldW === w and oldH === h and
            r_state(s2, :events_per_page) === eventsPerPage ->
          s2

        diff === 0 ->
          refresh_main_window(s2)

        diff > 0 ->
          oldEvents = queue_to_list(r_state(s2, :events))
          {s3, newEvents} = collect_more_events(s2, r_state(s2, :last_event), diff)
          s4 = r_state(s3, events_per_page: eventsPerPage)
          s5 = replace_events(s4, oldEvents ++ newEvents)
          refresh_main_window(s5)

        diff < 0 ->
          oldEvents = queue_to_list(r_state(s2, :events))

          revEvents =
            delete_n(
              :lists.reverse(oldEvents),
              abs(diff)
            )

          s3 = r_state(s2, events_per_page: eventsPerPage)
          s4 = replace_events(s3, :lists.reverse(revEvents))
          refresh_main_window(s4)
      end

    noreply(s6)
  end

  def handle_info(r_wx(event: r_wxMouse(type: :enter_window)), s) do
    :wxWindow.setFocus(r_state(s, :canvas))
    noreply(s)
  end

  def handle_info(r_wx(event: r_wxPaint()), s) do
    s2 = refresh_main_window(s)
    noreply(s2)
  end

  def handle_info(r_wx(event: r_wxMouse(type: t, x: x, y: y)), s) do
    :io.format('~tp ~tp\n', [t, {x, y}])
    noreply(s)
  end

  def handle_info(info, s) do
    :ok =
      :error_logger.format(
        '~p(~p): handle_info(~tp, ~tp)~n',
        [:et_wx_viewer, self(), info, s]
      )

    noreply(s)
  end

  def terminate(_Reason, _S) do
    :ignore
  end

  def code_change(_OldVsn, s, _Extra) do
    {:ok, s}
  end

  defp reply(reply, s) do
    timeout = timeout(s)
    {:reply, reply, s, timeout}
  end

  defp noreply(s) do
    timeout = timeout(s)
    {:noreply, s, timeout}
  end

  defp timeout(_S) do
    :infinity
  end

  defp scroll_first(s) do
    eventsPerPage = r_state(s, :events_per_page)
    {s2, newEvents} = collect_more_events(s, :first, eventsPerPage)

    s3 =
      case newEvents do
        [] ->
          s2

        [firstE | _] ->
          r_state(s2, first_event: firstE)
      end

    s4 = replace_events(s3, newEvents)
    refresh_main_window(s4)
  end

  defp scroll_last(s) do
    case collect_more_events(s, :last, -1) do
      {_, []} ->
        scroll_first(s)

      {s2, newEvents} ->
        [firstE | _] = newEvents

        s3 =
          replace_events(
            r_state(s2, first_event: firstE),
            newEvents
          )

        refresh_main_window(s3)
    end
  end

  defp scroll_prev(s) do
    scroll_up(s, r_state(s, :events_per_page))
  end

  defp scroll_next(s) do
    scroll_down(s, r_state(s, :events_per_page))
  end

  defp scroll_up(s) do
    scroll_up(s, calc_scroll(s))
  end

  defp scroll_up(s, expected) do
    n = queue_length(r_state(s, :events))
    eventsPerPage = r_state(s, :events_per_page)
    expected2 = adjust_expected(expected, n, eventsPerPage)
    oldEvents = queue_to_list(r_state(s, :events))

    case collect_more_events(s, r_state(s, :first_event), -expected2) do
      {_, []} ->
        s

      {s2, newEvents} ->
        newN = length(newEvents)

        cond do
          n + newN > eventsPerPage ->
            revAllEvents =
              :lists.reverse(
                oldEvents,
                :lists.reverse(newEvents)
              )

            tooMany = n + newN - eventsPerPage

            case delete_n(revAllEvents, tooMany) do
              [] ->
                s

              [lastE | _] = revEvents ->
                events = :lists.reverse(revEvents)
                s3 = replace_events(r_state(s2, last_event: lastE), events)
                refresh_main_window(s3)
            end

          true ->
            events = newEvents ++ oldEvents
            lastE = :lists.last(events)
            s3 = replace_events(r_state(s2, last_event: lastE), events)
            refresh_main_window(s3)
        end
    end
  end

  defp scroll_down(s) do
    scroll_down(s, calc_scroll(s))
  end

  defp scroll_down(s, expected) do
    n = queue_length(r_state(s, :events))
    eventsPerPage = r_state(s, :events_per_page)
    expected2 = adjust_expected(expected, n, eventsPerPage)
    oldEvents = queue_to_list(r_state(s, :events))

    case collect_more_events(s, r_state(s, :last_event), expected2) do
      {_, []} ->
        case collect_more_events(s, r_state(s, :first_event), n - eventsPerPage) do
          {_, []} ->
            s

          {s2, newEvents} ->
            events = newEvents ++ oldEvents
            [firstE | _] = events
            s3 = replace_events(r_state(s2, first_event: firstE), events)
            refresh_main_window(s3)
        end

      {s2, newEvents} ->
        allEvents = oldEvents ++ newEvents

        case delete_n(allEvents, length(newEvents)) do
          [] ->
            scroll_first(s)

          events ->
            [firstE | _] = events
            s3 = replace_events(r_state(s2, first_event: firstE), events)
            refresh_main_window(s3)
        end
    end
  end

  defp scroll_changed(s, expected) do
    cond do
      expected === 0 ->
        refresh_main_window(s)

      expected < 0 ->
        oldPos = event_pos(s)
        newPos = :lists.max([oldPos + expected, 0])

        case r_state(s, :first_event) do
          r_e(key: key, pos: ^oldPos) ->
            jump_up(s, key, oldPos, newPos)

          :first ->
            scroll_first(s)

          :last ->
            scroll_last(s)
        end

      true ->
        oldPos = event_pos(s)
        newPos = :lists.min([oldPos + expected, r_state(s, :n_events)])

        case r_state(s, :first_event) do
          r_e(key: key, pos: ^oldPos) ->
            jump_down(s, key, oldPos, newPos)

          :first = key ->
            jump_down(s, key, 0, newPos)

          :last ->
            scroll_last(s)
        end
    end
  end

  defp jump_up(s, oldKey, oldPos, newPos) do
    try = newPos - oldPos - 1
    order = r_state(s, :event_order)

    prevE =
      cond do
        newPos === 0 ->
          :first

        true ->
          fun = fn
            event, r_e(pos: p) when p >= newPos ->
              key = :et_collector.make_key(order, event)
              r_e(event: event, key: key, pos: p - 1)

            _E, acc ->
              acc
          end

          :et_collector.iterate(
            r_state(s, :collector_pid),
            oldKey,
            try,
            fun,
            r_e(key: oldKey, pos: oldPos)
          )
      end

    case collect_more_events(s, prevE, r_state(s, :events_per_page)) do
      {_, []} ->
        s

      {s2, events} ->
        [firstE | _] = events
        s3 = replace_events(r_state(s2, first_event: firstE), events)
        refresh_main_window(s3)
    end
  end

  defp jump_down(s, oldKey, oldPos, newPos) do
    try = newPos - oldPos
    order = r_state(s, :event_order)

    fun = fn
      event, r_e(pos: p) when p < newPos ->
        key = :et_collector.make_key(order, event)
        r_e(event: event, key: key, pos: p + 1)

      _, acc ->
        acc
    end

    prevE =
      :et_collector.iterate(
        r_state(s, :collector_pid),
        oldKey,
        try,
        fun,
        r_e(key: oldKey, pos: oldPos)
      )

    case collect_more_events(s, prevE, r_state(s, :events_per_page)) do
      {_, []} ->
        s

      {s2, events} ->
        [firstE | _] = events
        s3 = replace_events(r_state(s2, first_event: firstE), events)
        refresh_main_window(s3)
    end
  end

  defp adjust_expected(expected, n, eventsPerPage) do
    cond do
      n < eventsPerPage ->
        eventsPerPage - n

      expected < eventsPerPage ->
        expected

      true ->
        eventsPerPage
    end
  end

  defp calc_scroll(s) do
    :lists.max([div(r_state(s, :events_per_page), 3), 1])
  end

  defp revert_main_window(s) do
    {s2, events} = revert(s)
    s3 = replace_events(s2, events)
    refresh_main_window(s3)
  end

  defp revert(s) do
    eventsPerPage = r_state(s, :events_per_page)

    case collect_more_events(s, r_state(s, :first_event), -1) do
      {_, []} ->
        collect_more_events(s, :first, eventsPerPage)

      {s2, [_PrevEvent]} ->
        collect_more_events(s, r_state(s2, :first_event), eventsPerPage)
    end
  end

  defp delete_n(list, 0) do
    list
  end

  defp delete_n([], _) do
    []
  end

  defp delete_n([_ | tail], n) when n > 0 do
    delete_n(tail, n - 1)
  end

  defp pick_n(rest, 0, acc) do
    {:lists.reverse(acc), rest}
  end

  defp pick_n([], _N, acc) do
    {:lists.reverse(acc), []}
  end

  defp pick_n([head | tail], n, acc) when n > 0 do
    pick_n(tail, n - 1, [head | acc])
  end

  defp close_all(s) do
    _ = close_all_others(s)
    :wxFrame.destroy(r_state(s, :frame))
    opt_unlink(r_state(s, :parent_pid))
    {:stop, :shutdown, s}
  end

  defp close_all_others(s) do
    fun = fn {{:subscriber, pid}, _} ->
      cond do
        pid === self() ->
          :ok

        true ->
          :erlang.unlink(pid)
          send(pid, {:et, :close})
          :ok
      end
    end

    all =
      :et_collector.dict_match(
        r_state(s, :collector_pid),
        {{:subscriber, :_}, :_}
      )

    :lists.foreach(fun, all)
    noreply(s)
  end

  defp opt_unlink(pid) do
    cond do
      pid === :undefined ->
        :ignore

      true ->
        :erlang.unlink(pid)
    end
  end

  defp open_viewer(scale, filterName, actors, s) do
    filters =
      for f <- r_state(s, :filters) do
        {:dict_insert, {:filter, r_filter(f, :name)}, r_filter(f, :function)}
      end

    options = [
      {:parent_pid, r_state(s, :parent_pid)},
      {:title, r_state(s, :title)},
      {:collector_pid, r_state(s, :collector_pid)},
      {:detail_level, r_state(s, :detail_level)},
      {:active_filter, filterName},
      {:event_order, r_state(s, :event_order)},
      {:first_event, r_state(s, :first_event)},
      {:max_actors, r_state(s, :max_actors)},
      {:hide_actions, r_state(s, :hide_actions)},
      {:hide_actors, r_state(s, :hide_actors)},
      {:actors, actors},
      {:scale, scale},
      {:width, r_state(s, :width)},
      {:height, r_state(s, :height)}
      | filters
    ]

    case start_link(options) do
      {:ok, _ViewerPid} ->
        :ok

      {:error, reason} ->
        :ok =
          :error_logger.format('~p: Failed to start a new window: ~tp~n', [:et_wx_viewer, reason])
    end
  end

  defp create_main_window(s) do
    {normalFont, boldFont} = select_fonts(r_state(s, :scale))
    name = name_to_string(r_state(s, :active_filter))

    title =
      case r_state(s, :title) do
        :undefined ->
          :erlang.atom_to_list(:et_wx_viewer)

        explicit ->
          name_to_string(explicit)
      end

    frame =
      :wxFrame.new(:wx.null(), -1, title ++ ' (filter: ' ++ name ++ ')', [
        {:size, {r_state(s, :width), r_state(s, :height)}}
      ])

    statusBar = :wxFrame.createStatusBar(frame)
    panel = :wxPanel.new(frame, [])
    bar = :wxMenuBar.new()
    :wxFrame.setMenuBar(frame, bar)
    mainSizer = :wxBoxSizer.new(8)

    menuData =
      :lists.flatten([create_file_menu(bar), create_viewer_menu(bar), create_collector_menu(bar)])

    filterMenu = :wxMenu.new([])

    s2 =
      create_filter_menu(
        r_state(s, filter_menu: {filterMenu, []}),
        r_state(s, :active_filter),
        r_state(s, :filters)
      )

    :wxMenuBar.append(bar, filterMenu, 'Filters and scaling')
    create_help_menu(bar)
    optSizer = :wxBoxSizer.new(4)
    checkSizer = :wxBoxSizer.new(8)
    hideActions = :wxCheckBox.new(panel, -1, 'Hide From=To')
    :wxCheckBox.setValue(hideActions, r_state(s, :hide_actions))
    hideActors = :wxCheckBox.new(panel, -1, 'Hide (excluded actors)')
    :wxCheckBox.setValue(hideActors, r_state(s, :hide_actors))

    checkBoxData = [
      {:wxCheckBox.getId(hideActions), :hide_actions},
      {:wxCheckBox.getId(hideActors), :hide_actors}
    ]

    :wxPanel.connect(panel, :command_checkbox_clicked)
    _ = :wxSizer.add(checkSizer, hideActions)
    _ = :wxSizer.add(checkSizer, hideActors)
    _ = :wxSizer.add(optSizer, checkSizer, [{:border, 10}, {:flag, 64 ||| 128 ||| 32 ||| 16}])
    detailLevelBox = :wxStaticBoxSizer.new(4, panel, [{:label, 'Detail level'}])

    detailLevel =
      :wxSlider.new(panel, -1, r_state(s, :detail_level), 0, 100, [
        {:style, :wxe_util.get_const(:wxSL_LABELS)},
        {:size, {200, -1}}
      ])

    :wxStatusBar.setStatusText(statusBar, where_text(s))
    :wxFrame.connect(frame, :command_slider_updated)
    _ = :wxSizer.add(detailLevelBox, detailLevel)
    _ = :wxSizer.add(optSizer, detailLevelBox, [{:border, 10}, {:flag, 64 ||| 128 ||| 32 ||| 16}])
    _ = :wxSizer.addStretchSpacer(optSizer)
    _ = :wxSizer.add(mainSizer, optSizer)

    _ =
      :wxSizer.add(
        mainSizer,
        :wxStaticLine.new(panel, [{:style, 4}]),
        [{:flag, 8192}]
      )

    canvasSizer = :wxBoxSizer.new(4)
    canvas = :wxPanel.new(panel, [{:style, 65536}])
    {canvasW, canvasH} = :wxPanel.getSize(canvas)
    scrollBar = :wxScrollBar.new(panel, -1, [{:style, 8}])
    _ = :wxSizer.add(canvasSizer, canvas, [{:flag, 8192}, {:proportion, 1}])
    _ = :wxSizer.add(canvasSizer, scrollBar, [{:flag, 8192}])
    _ = :wxSizer.add(mainSizer, canvasSizer, [{:flag, 8192}, {:proportion, 1}])
    :wxPanel.connect(canvas, :left_down)
    :wxPanel.connect(canvas, :left_up)
    :wxPanel.connect(canvas, :right_up)
    :wxPanel.connect(canvas, :size)
    self = self()

    :wxPanel.connect(canvas, :paint, [
      {:callback,
       fn ev, _ ->
         dC = :wxPaintDC.new(canvas)
         :wxPaintDC.destroy(dC)
         send(self, ev)
       end}
    ])

    :wxPanel.connect(canvas, :key_down)
    :wxPanel.connect(canvas, :enter_window, [{:skip, true}])
    :wxFrame.connect(frame, :command_menu_selected)
    :wxFrame.connect(frame, :close_window)
    :wxFrame.connect(scrollBar, :scroll_changed)
    :wxPanel.setSize(panel, {r_state(s, :width), r_state(s, :height)})
    :wxPanel.setSizer(panel, mainSizer)
    :wxFrame.show(frame)
    :wxPanel.setFocus(canvas)
    :wxPanel.connect(canvas, :mousewheel)

    s3 =
      r_state(s2,
        title: title,
        frame: frame,
        packer: panel,
        normal_font: normalFont,
        bold_font: boldFont,
        canvas_width: canvasW,
        canvas_height: canvasH,
        canvas: canvas,
        canvas_sizer: canvasSizer,
        scroll_bar: scrollBar,
        y_pos: 15 * r_state(s, :scale),
        pen: :wxPen.new(),
        brush: :wxBrush.new(),
        print_d: :undefined,
        print_psdd: :undefined,
        menu_data: menuData,
        checkbox_data: checkBoxData,
        hide_actions_box: hideActions,
        hide_actors_box: hideActors,
        status_bar: statusBar
      )

    dC = :wxClientDC.new(canvas)
    s4 = draw_all_actors(s3, dC)
    :wxClientDC.destroy(dC)
    s4
  end

  defp where_text(r_state(n_events: n) = s) do
    pos = event_pos(s)
    :lists.concat([pos, ' (', n, ')'])
  end

  defp event_pos(r_state(first_event: e, events: events, n_events: last)) do
    case e do
      r_e(pos: pos) ->
        pos

      :first ->
        case queue_length(events) do
          0 ->
            0

          _ ->
            1
        end

      :last ->
        last
    end
  end

  defp init_printers(
         r_state(
           print_d: :undefined,
           print_psdd: :undefined
         ) = s
       ) do
    pD = :wxPrintData.new()
    pSDD = :wxPageSetupDialogData.new(pD)
    :wxPrintData.setPaperId(pD, 3)
    :wxPageSetupDialogData.setMarginTopLeft(pSDD, {15, 15})

    :wxPageSetupDialogData.setMarginBottomRight(
      pSDD,
      {15, 15}
    )

    r_state(s, print_d: pD, print_psdd: pSDD)
  end

  defp init_printers(r_state() = s) do
    s
  end

  defp select_fonts(scale) when is_integer(scale) do
    size =
      case scale do
        1 ->
          5

        2 ->
          10

        3 ->
          14

        4 ->
          20

        s ->
          s * 6
      end

    {:wxFont.new(size, 76, 90, 90, []), :wxFont.new(size, 76, 90, 92, [])}
  end

  defp get_value(key, pos, tupleList) when is_list(tupleList) do
    case :lists.keysearch(key, 1, tupleList) do
      {:value, tuple} when is_tuple(tuple) ->
        :erlang.element(pos, tuple)

      false ->
        false
    end
  end

  defp menuitem(menu, id, text, userData) do
    item = :wxMenu.append(menu, id, text)
    {:wxMenuItem.getId(item), item, userData}
  end

  defp create_file_menu(bar) do
    menu = :wxMenu.new([])

    data = [
      menuitem(menu, -1, 'Clear all events in the Collector', :clear_all),
      menuitem(menu, -1, 'Load events to the Collector from file', :load_all),
      menuitem(menu, -1, 'Save all events in the Collector to file', :save_all),
      menuitem(menu, 5011, 'Print setup', :print_setup),
      menuitem(menu, -1, 'Print current page', :print_one_page),
      menuitem(
        menu,
        5010,
        'Print all pages',
        :print_all_pages
      ),
      menuitem(
        menu,
        -1,
        'Close this Viewer',
        :close
      ),
      menuitem(
        menu,
        -1,
        'Close all other Viewers, but this (c)',
        :close_all_others
      ),
      menuitem(
        menu,
        -1,
        'Close all Viewers and the Collector)   (C) ',
        :close_all
      )
    ]

    _ = :wxMenu.insertSeparator(menu, 3)
    _ = :wxMenu.insertSeparator(menu, 7)
    :wxMenuBar.append(bar, menu, 'File')
    data
  end

  defp create_viewer_menu(bar) do
    menu = :wxMenu.new([])

    _ =
      :wxMenuItem.enable(
        :wxMenu.append(menu, -1, 'Scroll this Viewer'),
        [{:enable, false}]
      )

    _ = :wxMenu.appendSeparator(menu)

    d1 = [
      menuitem(menu, -1, 'First    (f)', :first),
      menuitem(menu, -1, 'Last     (l)', :last),
      menuitem(menu, -1, 'Prev     (p)', :prev),
      menuitem(menu, -1, 'Next     (n)', :next),
      menuitem(menu, -1, 'Refresh  (r)', :refresh)
    ]

    _ = :wxMenu.appendSeparator(menu)
    d2 = [menuitem(menu, -1, 'Up   5   (Up)', :up), menuitem(menu, -1, 'Down 5   (Down)', :down)]
    _ = :wxMenu.appendSeparator(menu)

    _ =
      :wxMenuItem.enable(
        :wxMenu.append(menu, -1, 'Actor visibility in this Viewer'),
        [{:enable, false}]
      )

    _ = :wxMenu.appendSeparator(menu)
    d3 = [menuitem(menu, -1, 'Display all actors (a)', :display_all)]
    _ = :wxMenuBar.append(bar, menu, 'Viewer')
    [d1, d2, d3]
  end

  defp create_collector_menu(bar) do
    menu = :wxMenu.new([])

    _ =
      :wxMenuItem.enable(
        :wxMenu.append(menu, -1, 'Scroll all Viewers'),
        [{:enable, false}]
      )

    _ = :wxMenu.appendSeparator(menu)

    data = [
      menuitem(menu, -1, 'First   (F)', :first_all),
      menuitem(menu, -1, 'Last    (L)', :last_all),
      menuitem(menu, -1, 'Prev    (P)', :prev_all),
      menuitem(menu, -1, 'Next    (N)', :next_all),
      menuitem(menu, -1, 'Refresh (R)', :refresh_all)
    ]

    _ = :wxMenuBar.append(bar, menu, 'Collector')
    data
  end

  defp create_filter_menu(s = r_state(filter_menu: {menu, data}), activeFilterName, filters) do
    :wx.foreach(
      fn
        {_, i, _} ->
          :wxMenu.delete(menu, i)

        i ->
          try do
            :wxMenu.delete(menu, i)
          catch
            _, reason ->
              :io.format('Could not delete item: ~tp, because ~tp.\n', [i, reason])
          end
      end,
      data
    )

    item = fn
      f, {n, acc} when r_filter(f, :name) === :all ->
        label = :lists.concat([pad_string(r_filter(f, :name), 20), '(0)'])
        {n + 1, [menuitem(menu, -1, label, {:data, f}) | acc]}

      f, {n, acc} ->
        label = :lists.concat([pad_string(r_filter(f, :name), 20), '(', n, ')'])
        {n + 1, [menuitem(menu, -1, label, {:data, f}) | acc]}
    end

    d1 = [i1 = :wxMenu.append(menu, -1, 'Same Filter New Scale'), :wxMenu.appendSeparator(menu)]
    :wxMenuItem.enable(i1, [{:enable, false}])
    {:value, filter} = :lists.keysearch(activeFilterName, r_filter(:name), filters)
    same = :lists.concat([pad_string(activeFilterName, 20), '(=) same    scale'])

    larger =
      :lists.concat([
        pad_string(
          activeFilterName,
          20
        ),
        '(+) bigger  scale'
      ])

    smaller =
      :lists.concat([
        pad_string(
          activeFilterName,
          20
        ),
        '(-) smaller scale'
      ])

    d2 = [
      menuitem(menu, -1, same, {:data, filter, 0}),
      menuitem(menu, -1, smaller, {:data, filter, -1}),
      menuitem(menu, -1, larger, {:data, filter, 1}),
      :wxMenu.appendSeparator(menu),
      i2 =
        :wxMenu.append(
          menu,
          -1,
          'New Filter Same Scale'
        ),
      :wxMenu.appendSeparator(menu)
    ]

    _ = :wxMenuItem.enable(i2, [{:enable, false}])
    {_, d3} = :lists.foldl(item, {1, []}, filters)
    r_state(s, filter_menu: {menu, :lists.flatten([d1, d2, d3])})
  end

  defp create_help_menu(bar) do
    menu = :wxMenu.new([])
    _ = menuitem(menu, 5009, 'Info', :help)
    :wxMenuBar.append(bar, menu, 'Help')
  end

  defp clear_canvas(s) do
    dC = :wxClientDC.new(r_state(s, :canvas))

    :wxDC.setBackground(
      dC,
      :wxe_util.get_const(:wxWHITE_BRUSH)
    )

    :wxDC.clear(dC)
    {canvasW, canvasH} = :wxPanel.getSize(r_state(s, :canvas))
    :wxSizer.recalcSizes(r_state(s, :canvas_sizer))

    s2 =
      r_state(s,
        refresh_needed: false,
        y_pos: 15 * r_state(s, :scale),
        canvas_width: canvasW,
        canvas_height: canvasH,
        events: queue_new()
      )

    s3 = draw_all_actors(s2, dC)
    :wxClientDC.destroy(dC)
    s3
  end

  defp replace_events(s, []) do
    r_state(s, first_event: :first, last_event: :first, events: queue_new())
  end

  defp replace_events(s, events) do
    queue =
      :lists.foldl(
        fn e, q ->
          queue_in(e, q)
        end,
        queue_new(),
        events
      )

    r_state(s, events: queue)
  end

  defp refresh_main_window(s) do
    :wx.batch(fn ->
      s2 = clear_canvas(s)
      s3 = update_scroll_bar(s2)
      display_events(s3, queue_to_list(r_state(s, :events)))
    end)
  end

  defp display_events(s, []) do
    s
  end

  defp display_events(s, events) do
    dC = :wxClientDC.new(r_state(s, :canvas))

    s2 =
      :lists.foldl(
        fn e, state ->
          display_event(e, state, dC)
        end,
        s,
        events
      )

    :wxClientDC.destroy(dC)
    s2
  end

  defp collect_more_events(s, prevKey = :first, try) do
    prevE = r_e(event: :undefined, key: prevKey, pos: 0)
    s2 = r_state(s, first_event: prevE, last_event: prevE)
    do_collect_more_events(s2, try, prevE, [])
  end

  defp collect_more_events(s, prevKey = :last, try) do
    prevE = r_e(event: :undefined, key: prevKey, pos: r_state(s, :n_events))
    s2 = r_state(s, first_event: prevE, last_event: prevE)
    do_collect_more_events(s2, try, prevE, [])
  end

  defp collect_more_events(s, r_e() = prevE, try) do
    do_collect_more_events(s, try, prevE, [])
  end

  defp do_collect_more_events(
         r_state(
           collector_pid: collector,
           event_order: order,
           active_filter: active,
           filters: filters
         ) = s,
         try,
         prevE,
         acc
       ) do
    incr =
      cond do
        try < 0 ->
          -1

        true ->
          1
      end

    prevKey = r_e(prevE, :key)
    {:value, r_filter(function: filterFun)} = :lists.keysearch(active, r_filter(:name), filters)

    {_S, _Incr, _Order, _Active, _FilterFun, lastE, newEvents} =
      :et_collector.iterate(
        collector,
        prevKey,
        try,
        &collect_event/2,
        {s, incr, order, active, filterFun, prevE, []}
      )

    expected = abs(try)
    actual = length(newEvents)
    missing = expected - actual

    {s2, acc2, try2} =
      cond do
        try < 0 ->
          {r_state(s, first_event: lastE), newEvents ++ acc, -missing}

        true ->
          tmpEvents = :lists.reverse(newEvents)
          {r_state(s, last_event: lastE), acc ++ tmpEvents, missing}
      end

    cond do
      missing !== 0 and prevKey !== r_e(lastE, :key) ->
        do_collect_more_events(s2, try2, lastE, acc2)

      true ->
        {s2, acc2}
    end
  end

  defp collect_event(
         event,
         {s, incr, order, active, filterFun, r_e(pos: prevPos), events}
       ) do
    key = :et_collector.make_key(order, event)
    e = r_e(event: event, key: key, pos: prevPos + incr)

    {lastE, events2} =
      case (try do
              filterFun.(event)
            catch
              :error, e -> {:EXIT, {e, __STACKTRACE__}}
              :exit, e -> {:EXIT, e}
              e -> e
            end) do
        true ->
          case is_hidden(r_event(event, :from), r_event(event, :to), s) do
            true ->
              {e, events}

            false ->
              {e, [e | events]}
          end

        {true, event2} ->
          key2 = :et_collector.make_key(order, event2)
          e2 = r_e(e, event: event2, key: key2)

          case is_hidden(r_event(event2, :from), r_event(event2, :to), s) do
            true ->
              {e2, events}

            false ->
              {e2, [e2 | events]}
          end

        false ->
          {e, events}

        bad ->
          contents = {:bad_filter, r_state(s, :active_filter), bad, event}

          event2 =
            r_event(event,
              contents: contents,
              from: :bad_filter,
              to: :bad_filter
            )

          e2 = r_e(e, event: event2)
          {e2, [e2 | events]}
      end

    {s, incr, order, active, filterFun, lastE, events2}
  end

  defp display_event(r_e(event: event) = e, s, dC)
       when r_event(event, :detail_level) <= r_state(s, :detail_level) do
    {fromRefresh, from} = ensure_actor(r_event(event, :from), s, dC)
    {fromName, fromPos, s2} = from
    {toRefresh, to} = ensure_actor(r_event(event, :to), s2, dC)
    {toName, toPos, s3} = to

    s4 =
      cond do
        fromRefresh !== false and toRefresh !== false ->
          r_state(s3,
            refresh_needed: true,
            events: queue_in(e, r_state(s3, :events))
          )

        fromName === toName ->
          case r_state(s, :hide_actions) do
            true ->
              s3

            false ->
              label = name_to_string(r_event(event, :label))
              draw_named_arrow(label, fromName, toName, fromPos, toPos, e, s3, dC)
          end

        true ->
          label = name_to_string(r_event(event, :label))
          draw_named_arrow(label, fromName, toName, fromPos, toPos, e, s3, dC)
      end

    s4
  end

  defp display_event(r_e(), s, _DC) do
    s
  end

  defp draw_named_arrow(label, fromName, toName, fromPos, toPos, e, s, dC) do
    case r_state(s, :y_pos) + 15 * r_state(s, :scale) do
      _ when r_state(s, :hide_actors) === true and fromName === 'UNKNOWN' ->
        s

      _ when r_state(s, :hide_actors) === true and toName === 'UNKNOWN' ->
        s

      y when y > r_state(s, :canvas_height) ->
        r_state(s,
          refresh_needed: true,
          events: queue_in(e, r_state(s, :events))
        )

      y ->
        s2 = r_state(s, y_pos: y, events: queue_in(e, r_state(s, :events)))
        s3 = draw_arrow(fromPos, toPos, s2, dC)
        draw_label(label, fromName, toName, fromPos, toPos, s3, dC)
    end
  end

  defp draw_arrow(pos, pos, s, _DC) do
    s
  end

  defp draw_arrow(fromPos, toPos, s, dC) do
    y = r_state(s, :y_pos)

    :wxPen.setColour(
      r_state(s, :pen),
      :wxe_util.get_const(:wxBLACK)
    )

    :wxDC.setPen(dC, r_state(s, :pen))
    :wxDC.drawLine(dC, {fromPos, y}, {toPos, y})
    radians = calc_angle({fromPos, y}, {toPos, y})
    len = 5
    radians2 = radians + 3.665191429188092
    radians3 = radians + 2.617993877991494
    {x3, y3} = calc_point({toPos, y}, len, radians2)
    {x4, y4} = calc_point({toPos, y}, len, radians3)
    points = [{round(toPos), round(y)}, {round(x3), round(y3)}, {round(x4), round(y4)}]

    :wxBrush.setColour(
      r_state(s, :brush),
      :wxe_util.get_const(:wxBLACK)
    )

    :wxDC.setBrush(dC, r_state(s, :brush))
    :wxDC.drawPolygon(dC, points, [])
    s
  end

  defp calc_angle({x1, y1}, {x2, y2}) do
    :math.atan2(y2 - y1, x2 - x1)
  end

  defp calc_point({x, y}, length, radians) do
    x2 = round(x + length * :math.cos(radians))
    y2 = round(y + length * :math.sin(radians))
    {x2, y2}
  end

  defp draw_label(label, fromName, toName, fromPos, toPos, s, dC) do
    color =
      cond do
        fromName === 'UNKNOWN' and toName === 'UNKNOWN' ->
          {2, 71, 254}

        fromName === 'UNKNOWN' ->
          {255, 126, 0}

        toName === 'UNKNOWN' ->
          {255, 126, 0}

        fromPos === toPos ->
          {2, 71, 254}

        true ->
          {227, 38, 54}
      end

    scale = r_state(s, :scale)
    x = :lists.min([fromPos, toPos]) + 6 * scale
    y = r_state(s, :y_pos)
    write_text(label, x, y, color, r_state(s, :normal_font), s, dC)
    s
  end

  defp draw_all_actors(s, dC) do
    scale = r_state(s, :scale)

    fun = fn a, x ->
      case draw_actor(a, x, s, dC) do
        true ->
          x + 60 * scale

        false ->
          x
      end
    end

    :lists.foldl(fun, 10 * scale, r_state(s, :actors))
    s
  end

  defp ensure_actor(name, s, dC) do
    do_ensure_actor(name, s, r_state(s, :actors), 0, dC)
  end

  defp do_ensure_actor(name, s, [h | _], n, _DC)
       when r_actor(h, :name) === name do
    pos = (10 + n * 60) * r_state(s, :scale)
    {false, {name, pos, s}}
  end

  defp do_ensure_actor(name, s, [h | t], n, dC) do
    cond do
      r_state(s, :hide_actors) and r_actor(h, :exclude) ->
        do_ensure_actor(name, s, t, n, dC)

      true ->
        do_ensure_actor(name, s, t, n + 1, dC)
    end
  end

  defp do_ensure_actor(name, s, [], n, dC) do
    pos = (10 + n * 60) * r_state(s, :scale)
    maxActors = r_state(s, :max_actors)

    cond do
      is_integer(maxActors) and n > maxActors ->
        ensure_actor('UNKNOWN', s, dC)

      pos > r_state(s, :canvas_width) - (10 - 15) * r_state(s, :scale) ->
        a = create_actor(name)
        draw_actor(a, pos, s, dC)
        {true, {name, pos, r_state(s, actors: r_state(s, :actors) ++ [a])}}

      true ->
        a = create_actor(name)
        draw_actor(a, pos, s, dC)
        {false, {name, pos, r_state(s, actors: r_state(s, :actors) ++ [a])}}
    end
  end

  defp draw_actor(a, lineX, s, dC) do
    cond do
      r_state(s, :hide_actors) and r_actor(a, :exclude) ->
        false

      true ->
        scale = r_state(s, :scale)
        textX = lineX - 5 * scale
        {textY, lineTopY, lineBotY} = calc_y(s)

        color =
          case r_actor(a, :name) do
            'UNKNOWN' ->
              {255, 126, 0}

            _ ->
              {227, 38, 54}
          end

        {string, font} =
          cond do
            r_state(s, :context) === :display and r_actor(a, :exclude) ->
              {'(' ++ r_actor(a, :string) ++ ')', r_state(s, :normal_font)}

            r_state(s, :context) === :display and r_actor(a, :include) ->
              {'[' ++ r_actor(a, :string) ++ ']', r_state(s, :bold_font)}

            true ->
              {r_actor(a, :string), r_state(s, :normal_font)}
          end

        write_text(string, textX, textY, color, font, s, dC)
        :wxPen.setColour(r_state(s, :pen), color)
        :wxDC.setPen(dC, r_state(s, :pen))

        :wxDC.drawLines(
          dC,
          [{lineX, lineTopY}, {lineX, lineBotY}]
        )

        true
    end
  end

  defp calc_y(r_state(canvas_height: height, scale: scale)) do
    textY = 15 * scale
    lineTopY = round(textY + 15 / 2 * scale)
    lineBotY = height
    {textY, lineTopY, lineBotY}
  end

  defp display_all(s) do
    actors = r_state(s, :actors)

    actors2 =
      for a <- actors do
        r_actor(a, include: false, exclude: false)
      end

    s2 = r_state(s, actors: actors2, display_all: true, hide_actions: false, hide_actors: false)

    :wxCheckBox.setValue(
      r_state(s2, :hide_actions_box),
      r_state(s2, :hide_actions)
    )

    :wxCheckBox.setValue(
      r_state(s2, :hide_actors_box),
      r_state(s2, :hide_actors)
    )

    revert_main_window(s2)
  end

  defp is_hidden(a, s) do
    case r_state(s, :display_all) do
      true ->
        r_actor(a, :exclude)

      false ->
        r_actor(a, :exclude) or not r_actor(a, :include)
    end
  end

  defp is_hidden(from, to, s) do
    actors = r_state(s, :actors)
    displayAll = r_state(s, :display_all)
    fromMatch = :lists.keysearch(from, r_actor(:name), actors)
    toMatch = :lists.keysearch(to, r_actor(:name), actors)

    case {fromMatch, toMatch} do
      {false, false} ->
        not displayAll

      {false, {:value, t}} ->
        is_hidden(t, s)

      {{:value, f}, false} ->
        is_hidden(f, s)

      {{:value, f}, {:value, t}} when displayAll ->
        is_hidden(f, s) or is_hidden(t, s)

      {{:value, f}, {:value, t}}
      when r_actor(f, :include) or
             r_actor(t, :include) ->
        r_actor(f, :exclude) or r_actor(t, :exclude)

      {{:value, _F}, {:value, _T}} ->
        true
    end
  end

  defp move_actor(from, to, actors, s) do
    pos = r_actor(:name)
    toName = r_actor(to, :name)
    fromName = r_actor(from, :name)
    toIx = actor_index(toName, pos, actors)
    fromIx = actor_index(fromName, pos, actors)

    cond do
      fromIx !== 0 and toIx !== 0 and toIx > fromIx ->
        actors2 = :lists.keydelete(fromName, pos, actors)
        actors3 = insert_actor_after(from, to, actors2)
        s2 = r_state(s, actors: actors3)
        refresh_main_window(s2)

      fromIx !== 0 and toIx !== 0 ->
        actors2 = :lists.keydelete(fromName, pos, actors)
        actors3 = insert_actor_before(from, to, actors2)
        s2 = r_state(s, actors: actors3)
        refresh_main_window(s2)

      true ->
        s
    end
  end

  defp insert_actor_after(from, to, [h | t]) do
    case r_actor(to, :name) === r_actor(h, :name) do
      true ->
        [h, from | t]

      false ->
        [h | insert_actor_after(from, to, t)]
    end
  end

  defp insert_actor_after(_From, _To, []) do
    []
  end

  defp insert_actor_before(from, to, [h | t]) do
    case r_actor(to, :name) === r_actor(h, :name) do
      true ->
        [from, h | t]

      false ->
        [h | insert_actor_before(from, to, t)]
    end
  end

  defp insert_actor_before(_From, _To, []) do
    []
  end

  defp actor_index(_Key, _Pos, []) do
    0
  end

  defp actor_index(key, pos, [h | t]) do
    case key === :erlang.element(pos, h) do
      false ->
        actor_index(key, pos, t) + 1

      true ->
        1
    end
  end

  defp y_to_n(y, s) do
    y2 = y / r_state(s, :scale) - 15 + 15 / 2
    n = round(y2 / 15 - 0.2)
    maxN = queue_length(r_state(s, :events))

    cond do
      n <= 0 ->
        :actor

      n > maxN ->
        :actor

      true ->
        {:event, n}
    end
  end

  defp x_to_n(x, s) do
    scale = r_state(s, :scale)
    len = length(r_state(s, :actors))
    x2 = x - 10 * scale
    n = x2 / (60 * scale)
    n2 = trunc(n + 1.5)

    cond do
      n2 > len ->
        len

      n2 < 1 ->
        1

      true ->
        n2
    end
  end

  defp write_text(text, x, y, color, font, s, dC) do
    :wxDC.setFont(dC, font)
    :wxDC.setTextForeground(dC, color)
    :wxDC.drawText(dC, text, {x, round(y - 15 * r_state(s, :scale) / 2) - 3})
  end

  defp do_open_event(s, n) do
    events = queue_to_list(r_state(s, :events))
    s2 = r_state(s, events: list_to_queue(events))

    case (try do
            :lists.nth(n, events)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        {:error, {:no_such_event, n}}

      r_e(key: key) ->
        pid = r_state(s, :collector_pid)
        fun = &create_contents_window/2
        prev = :et_collector.iterate(pid, key, -1)

        {^s2, res} =
          cond do
            prev === key ->
              :et_collector.iterate(pid, :first, 1, fun, {s2, []})

            true ->
              :et_collector.iterate(pid, prev, 1, fun, {s2, []})
          end

        case res do
          [] ->
            {:error, :no_contents_viewer_started}

          [single] ->
            single

          multi ->
            {:error, {:too_many, multi}}
        end
    end
  end

  defp create_contents_window(event, {s, res}) do
    options = [
      {:viewer_pid, self()},
      {:event, event},
      {:event_order, r_state(s, :event_order)},
      {:active_filter, r_state(s, :active_filter)},
      {:wx_debug, r_state(s, :wx_debug)}
      | r_state(s, :filters)
    ]

    case (try do
            :et_wx_contents_viewer.start_link(options)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, pid} ->
        {s, [{:ok, pid} | res]}

      {:error, reason} ->
        :ok =
          :error_logger.format(
            '~p(~p): create_contents_window(~tp) ->~n     ~tp~n',
            [:et_wx_viewer, self(), options, reason]
          )

        {s, [{:error, reason} | res]}

      stuff ->
        {s, [{:error, {:stuff, stuff}} | res]}
    end
  end

  defp print_setup(s) do
    s2 =
      r_state(
        print_psdd: pSDD0,
        print_d: pD0
      ) = init_printers(s)

    :wxPageSetupDialogData.setPrintData(pSDD0, pD0)

    pSD =
      :wxPageSetupDialog.new(
        r_state(s, :frame),
        [{:data, pSDD0}]
      )

    :wxPageSetupDialog.showModal(pSD)
    pSDD1 = :wxPageSetupDialog.getPageSetupData(pSD)
    pD1 = :wxPageSetupDialogData.getPrintData(pSDD1)
    pD = :wxPrintData.new(pD1)
    psDD = :wxPageSetupDialogData.new(pSDD1)
    :wxPageSetupDialog.destroy(pSD)
    :wxPageSetupDialogData.destroy(pSDD0)
    :wxPrintData.destroy(pD0)
    r_state(s2, print_psdd: psDD, print_d: pD)
  end

  defp print(
         r_state(
           print_d: :undefined,
           print_psdd: :undefined
         ) = s,
         scope
       ) do
    s2 = print_setup(s)
    print(s2, scope)
  end

  defp print(r_state(print_psdd: pSDD, print_d: pD) = s, scope) do
    pDD = :wxPrintDialogData.new(pD)
    :wxPrintDialogData.enablePrintToFile(pDD, true)
    :wxPrintDialogData.enablePageNumbers(pDD, true)
    :wxPrintDialogData.enableSelection(pDD, true)
    tab = :ets.new(:et_wx_viewer, [:public])

    getPageInfo = fn this ->
      {_, _, pW, pH} = :wxPrintout.getPaperRectPixels(this)
      printerS = r_state(s, context: :printer, canvas_width: pW, canvas_height: pH)
      eventsPerPage = events_per_page(printerS, pH)
      pagedEvents = paged_events(printerS, scope, eventsPerPage)

      for pE <- pagedEvents do
        :ets.insert(tab, pE)
      end

      :ets.insert(tab, printerS)
      numPages = length(pagedEvents)
      {1, numPages, 1, numPages}
    end

    hasPage = fn _This, page ->
      size = :ets.info(tab, :size)
      numPages = size - 1
      page >= 1 and page <= numPages
    end

    onPrintPage = fn this, page ->
      :wxPrintout.mapScreenSizeToPageMargins(this, pSDD)
      [printerS] = :ets.lookup(tab, :state)
      events = :ets.lookup_element(tab, page, 2)
      dC = :wxPrintout.getDC(this)
      printerS2 = draw_all_actors(printerS, dC)
      printerS3 = r_state(printerS2, y_pos: 15 * r_state(printerS2, :scale))

      :lists.foldl(
        fn e, state ->
          display_event(e, state, dC)
        end,
        printerS3,
        events
      )

      true
    end

    printout1 =
      :wxPrintout.new('Print', onPrintPage, [{:getPageInfo, getPageInfo}, {:hasPage, hasPage}])

    printout2 =
      :wxPrintout.new('Print', onPrintPage, [{:getPageInfo, getPageInfo}, {:hasPage, hasPage}])

    preview =
      :wxPrintPreview.new(
        printout1,
        [{:printoutForPrinting, printout2}, {:data, pDD}]
      )

    case :wxPrintPreview.isOk(preview) do
      true ->
        pF = :wxPreviewFrame.new(preview, r_state(s, :frame), [])
        :wxPreviewFrame.centre(pF, [{:dir, 8 ||| 4}])
        :wxPreviewFrame.initialize(pF)
        :wxPreviewFrame.centre(pF)
        :wxPreviewFrame.show(pF)

        onClose = fn _Wx, eventRef ->
          :ets.delete(tab)
          :wxEvent.skip(eventRef)
        end

        :wxPreviewFrame.connect(pF, :close_window, [{:callback, onClose}])

      false ->
        :io.format(
          'Could not create preview window.\nPerhaps your current printer is not set correctly?~n',
          []
        )

        :wxPrintPreview.destroy(preview)
        :ets.delete(tab)
    end

    s
  end

  defp paged_events(s, scope, eventsPerPage) do
    {_, events} =
      case scope do
        :print_one_page ->
          revert(r_state(s, events_per_page: eventsPerPage))

        :print_all_pages ->
          collect_more_events(s, :first, r_state(s, :n_events))
      end

    split_list(events, eventsPerPage)
  end

  defp split_list(list, n) when is_integer(n) and n > 0 do
    do_split_list(list, n, 1, [])
  end

  defp do_split_list([], _N, _Page, acc) do
    :lists.reverse(acc)
  end

  defp do_split_list(list, n, page, acc) do
    {items, rest} = pick_n(list, n, [])
    do_split_list(rest, n, page + 1, [{page, items} | acc])
  end

  defp get_latest_resize(r_wx(obj: objRef, event: r_wxSize()) = wx) do
    receive do
      r_wx(obj: ^objRef, event: r_wxSize()) = wx2 ->
        get_latest_resize(wx2)
    after
      100 ->
        wx
    end
  end

  defp get_latest_scroll(
         r_wx(
           obj: objRef,
           event: r_wxScroll(type: :scroll_changed)
         ) = wx
       ) do
    receive do
      r_wx(
        obj: ^objRef,
        event: r_wxScroll(type: :scroll_changed)
      ) = wx2 ->
        get_latest_scroll(wx2)
    after
      100 ->
        wx
    end
  end

  defp update_scroll_bar(
         r_state(
           scroll_bar: scrollBar,
           status_bar: statusBar,
           events_per_page: eventsPerPage,
           n_events: n
         ) = s
       ) do
    opts = [{:refresh, true}]
    {_, lineTopY, lineBotY} = calc_y(s)
    range = lineBotY - lineTopY

    eventPos =
      case event_pos(s) do
        1 ->
          0

        p ->
          p
      end

    cond do
      n !== 0 and eventsPerPage !== 0 ->
        pixelsPerEvent = range / eventsPerPage
        share = eventsPerPage / n

        :wxScrollBar.setScrollbar(
          scrollBar,
          trunc(eventPos * share * pixelsPerEvent),
          round(share * range),
          range,
          round(share * range),
          opts
        )

      true ->
        :wxScrollBar.setScrollbar(scrollBar, 0, range, range, range, opts)
    end

    :wxStatusBar.setStatusText(statusBar, where_text(s))
    s
  end

  defp events_per_page(s, pageHeight) do
    eventsPerPage = div(pageHeight - 15 * r_state(s, :scale), 15 * r_state(s, :scale))
    :lists.max([1, eventsPerPage])
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

  defp opt_create_actor(name, tag, s) do
    actors = r_state(s, :actors)

    new =
      case :lists.keysearch(name, r_actor(:name), actors) do
        {:value, old} ->
          old

        false ->
          create_actor(name)
      end

    case tag do
      :include ->
        r_actor(new, include: true)

      :exclude ->
        r_actor(new, exclude: true)
    end
  end

  defp create_actor(name) do
    string = name_to_string(name)
    r_actor(name: name, string: string, include: false, exclude: false)
  end

  defp name_to_string(name) do
    case (try do
            :io_lib.format('~ts', [name])
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        :lists.flatten(:io_lib.format('~tw', [name]))

      goodString ->
        :lists.flatten(goodString)
    end
  end

  defp pad_string(atom, minLen) when is_atom(atom) do
    pad_string(:erlang.atom_to_list(atom), minLen)
  end

  defp pad_string(string, minLen)
       when is_integer(minLen) and
              minLen >= 0 do
    len = :string.length(string)

    case len >= minLen do
      true ->
        string

      false ->
        string ++ :lists.duplicate(minLen - len, ?\s)
    end
  end

  defp queue_new() do
    {0, [], []}
  end

  defp queue_in(x, {size, in__, out}) do
    {size + 1, [x | in__], out}
  end

  defp queue_to_list({_Size, [], out}) do
    out
  end

  defp queue_to_list({_Size, in__, out}) do
    out ++ :lists.reverse(in__)
  end

  defp queue_length({size, _In, _Out}) do
    size
  end

  defp list_to_queue(list) when is_list(list) do
    {length(list), [], list}
  end
end
