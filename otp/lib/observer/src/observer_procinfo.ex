defmodule :m_observer_procinfo do
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
    parent: :undefined,
    frame: :undefined,
    notebook: :undefined,
    pid: :undefined,
    pages: [],
    expand_table: :undefined,
    expand_wins: []
  )

  Record.defrecord(:r_worker, :worker,
    panel: :undefined,
    callback: :undefined
  )

  Record.defrecord(:r_io, :io, rdata: '')

  def start(process, parentFrame, parent) do
    :wx_object.start_link(:observer_procinfo, [process, parentFrame, parent], [])
  end

  def init([pid, parentFrame, parent]) do
    try do
      table = :ets.new(:observer_expand, [:set, :public])

      title =
        case :observer_wx.try_rpc(node(pid), :erlang, :process_info, [pid, :registered_name]) do
          [] ->
            :io_lib.format('~p', [pid])

          {:registered_name, registered} ->
            :io_lib.format('~tp (~p)', [registered, pid])

          :undefined ->
            throw(:process_undefined)
        end

      scale = :observer_wx.get_scale()

      frame =
        :wxFrame.new(parentFrame, -1, [:erlang.atom_to_list(node(pid)), ?:, title], [
          {:style, 2048 ||| 64 ||| 1024 ||| 512 ||| 4096 ||| 536_870_912 ||| 4_194_304},
          {:size, {scale * 850, scale * 600}}
        ])

      menuBar = :wxMenuBar.new()
      create_menus(menuBar)
      :wxFrame.setMenuBar(frame, menuBar)
      notebook = :wxNotebook.new(frame, 604, [{:style, 0}])
      processPage = init_panel(notebook, 'Process Information', [pid], &init_process_page/2)
      messagePage = init_panel(notebook, 'Messages', [pid, table], &init_message_page/3)
      dictPage = init_panel(notebook, 'Dictionary', [pid, table], &init_dict_page/3)
      stackPage = init_panel(notebook, 'Stack Trace', [pid], &init_stack_page/2)
      statePage = init_panel(notebook, 'State', [pid, table], &init_state_page/3)

      ps =
        case :gen_server.call(:observer, :log_status) do
          true ->
            [init_panel(notebook, 'Log', [pid, table], &init_log_page/3)]

          false ->
            []
        end

      :wxFrame.connect(frame, :close_window)
      :wxMenu.connect(frame, :command_menu_selected)
      :wxFrame.show(frame)

      {frame,
       r_state(
         parent: parent,
         pid: pid,
         frame: frame,
         notebook: notebook,
         pages: [
           [processPage, messagePage, dictPage, stackPage, statePage]
           | ps
         ],
         expand_table: table
       )}
    catch
      :error, {:badrpc, _} ->
        :observer_wx.return_to_localnode(parentFrame, node(pid))
        {:stop, :badrpc}

      :process_undefined ->
        :observer_lib.display_info_dialog(parentFrame, 'No such alive process')
        {:stop, :normal}
    end
  end

  defp init_panel(notebook, str, funArgs, fun) do
    panel = :wxPanel.new(notebook)
    sizer = :wxBoxSizer.new(4)
    {window, callback} = apply(fun, [panel | funArgs])

    :wxSizer.add(sizer, window, [
      {:flag, 8192 ||| (64 ||| 128 ||| 32 ||| 16)},
      {:proportion, 1},
      {:border, 5}
    ])

    :wxPanel.setSizer(panel, sizer)
    true = :wxNotebook.addPage(notebook, panel, str)
    r_worker(panel: panel, callback: callback)
  end

  def handle_event(r_wx(event: r_wxClose(type: :close_window)), state) do
    {:stop, :normal, state}
  end

  def handle_event(
        r_wx(
          id: 5001,
          event: r_wxCommand(type: :command_menu_selected)
        ),
        state
      ) do
    {:stop, :normal, state}
  end

  def handle_event(
        r_wx(id: 601),
        r_state(frame: frame, pid: pid, pages: pages, expand_table: t) = state
      ) do
    :ets.delete_all_objects(t)

    try do
      for w <- pages do
        r_worker(w, :callback).()
      end
    catch
      :process_undefined ->
        :wxFrame.setTitle(frame, :io_lib.format('*DEAD* ~p', [pid]))
    end

    {:noreply, state}
  end

  def handle_event(
        r_wx(obj: moreEntry, event: r_wxMouse(type: :left_down), userData: {:more, more}),
        state
      ) do
    :observer_lib.add_scroll_entries(moreEntry, more)
    {:noreply, state}
  end

  def handle_event(
        r_wx(
          event: r_wxMouse(type: :left_down),
          userData: targetPid
        ),
        state
      ) do
    send(:observer, {:open_link, targetPid})
    {:noreply, state}
  end

  def handle_event(
        r_wx(obj: obj, event: r_wxMouse(type: :enter_window)),
        state
      ) do
    :wxStaticText.setForegroundColour(obj, {0, 0, 100, 255})
    {:noreply, state}
  end

  def handle_event(
        r_wx(obj: obj, event: r_wxMouse(type: :leave_window)),
        state
      ) do
    :wxStaticText.setForegroundColour(
      obj,
      :wxe_util.get_const(:wxBLUE)
    )

    {:noreply, state}
  end

  def handle_event(
        r_wx(event: r_wxHtmlLink(linkInfo: r_wxHtmlLinkInfo(href: href))),
        r_state(frame: frame, expand_table: t, expand_wins: opened0) = state
      ) do
    {type, rest} =
      case href do
        '#Term?' ++ keys ->
          {:cdv_term_cb, keys}

        '#OBSBinary?' ++ keys ->
          {:cdv_bin_cb, keys}

        _ ->
          {:other, href}
      end

    case type do
      :other ->
        send(:observer, {:open_link, href})
        {:noreply, state}

      callback ->
        [{'key1', key1}, {'key2', key2}, {'key3', key3}] = :uri_string.dissect_query(rest)

        id =
          {:obs,
           {t,
            {:erlang.list_to_integer(key1), :erlang.list_to_integer(key2),
             :erlang.list_to_integer(key3)}}}

        opened =
          case :lists.keyfind(id, 1, opened0) do
            false ->
              win = :cdv_detail_wx.start_link(id, [], frame, callback, :obs)
              [{id, win} | opened0]

            {_, win} ->
              :wxFrame.raise(win)
              opened0
          end

        {:noreply, r_state(state, expand_wins: opened)}
    end
  end

  def handle_event(r_wx(event: r_wxHtmlLink(linkInfo: r_wxHtmlLinkInfo(href: info))), state) do
    send(:observer, {:open_link, info})
    {:noreply, state}
  end

  def handle_event(event, _State) do
    :erlang.error({:unhandled_event, event})
  end

  def handle_info(
        {:get_debug_info, from},
        state = r_state(notebook: notebook)
      ) do
    send(from, {:procinfo_debug, notebook})
    {:noreply, state}
  end

  def handle_info(_Info, state) do
    {:noreply, state}
  end

  def handle_call(call, from, _State) do
    :erlang.error({:unhandled_call, call, from})
  end

  def handle_cast(
        {:detail_win_closed, id},
        r_state(expand_wins: opened0) = state
      ) do
    opened = :lists.keydelete(id, 1, opened0)
    {:noreply, r_state(state, expand_wins: opened)}
  end

  def handle_cast(cast, _State) do
    :erlang.error({:unhandled_cast, cast})
  end

  def terminate(
        _Reason,
        r_state(parent: parent, pid: pid, frame: frame, expand_table: t)
      ) do
    t !== :undefined and :ets.delete(t)
    send(parent, {:procinfo_menu_closed, pid})

    case frame do
      :undefined ->
        :ok

      _ ->
        :wxFrame.destroy(frame)
    end

    :ok
  end

  def code_change(_, _, state) do
    {:ok, state}
  end

  defp init_process_page(panel, pid) do
    wSz = :observer_wx.try_rpc(node(pid), :erlang, :system_info, [:wordsize])
    fields0 = process_info_fields(pid, wSz)
    {fPanel, _, upFields} = :observer_lib.display_info(panel, fields0)

    {fPanel,
     fn ->
       fields = process_info_fields(pid, wSz)
       :observer_lib.update_info(upFields, fields)
     end}
  end

  defp init_message_page(parent, pid, table) do
    win = :observer_lib.html_window(parent)
    cs = :observer_lib.colors(parent)

    update = fn ->
      case :observer_wx.try_rpc(node(pid), :erlang, :process_info, [pid, :messages]) do
        {:messages, messages} ->
          html = :observer_html_lib.expandable_term('Message Queue', messages, table, cs)
          :wxHtmlWindow.setPage(win, html)

        _ ->
          throw(:process_undefined)
      end
    end

    update.()
    {win, update}
  end

  defp init_dict_page(parent, pid, table) do
    win = :observer_lib.html_window(parent)
    cs = :observer_lib.colors(parent)

    update = fn ->
      case :observer_wx.try_rpc(node(pid), :erlang, :process_info, [pid, :dictionary]) do
        {:dictionary, dict} ->
          html = :observer_html_lib.expandable_term('Dictionary', dict, table, cs)
          :wxHtmlWindow.setPage(win, html)

        _ ->
          throw(:process_undefined)
      end
    end

    update.()
    {win, update}
  end

  defp init_stack_page(parent, pid) do
    lCtrl = :wxListCtrl.new(parent, [{:style, 32 ||| 2}])
    li = :wxListItem.new()
    scale = :observer_wx.get_scale()
    :wxListItem.setText(li, 'Module:Function/Arg')
    :wxListCtrl.insertColumn(lCtrl, 0, li)
    :wxListCtrl.setColumnWidth(lCtrl, 0, scale * 300)
    :wxListItem.setText(li, 'File:LineNumber')
    :wxListCtrl.insertColumn(lCtrl, 1, li)
    :wxListCtrl.setColumnWidth(lCtrl, 1, scale * 300)
    :wxListItem.destroy(li)
    even = :wxSystemSettings.getColour(1 + 14 + 1 + 4 + 5)
    odd = :observer_lib.mix(even, :wxSystemSettings.getColour(1 + 12), 0.8)

    update = fn ->
      case :observer_wx.try_rpc(node(pid), :erlang, :process_info, [pid, :current_stacktrace]) do
        {:current_stacktrace, rawBt} ->
          :wxListCtrl.deleteAllItems(lCtrl)

          :wx.foldl(
            fn {m, f, a, info}, row ->
              _Item = :wxListCtrl.insertItem(lCtrl, row, '')

              rem(row, 2) === 0 or
                :wxListCtrl.setItemBackgroundColour(
                  lCtrl,
                  row,
                  odd
                )

              :wxListCtrl.setItem(lCtrl, row, 0, :observer_lib.to_str({m, f, a}))

              fileLine =
                case info do
                  [{:file, file}, {:line, line}] ->
                    :io_lib.format(
                      '~ts:~w',
                      [file, line]
                    )

                  _ ->
                    []
                end

              :wxListCtrl.setItem(lCtrl, row, 1, fileLine)
              row + 1
            end,
            0,
            rawBt
          )

        _ ->
          throw(:process_undefined)
      end
    end

    resize = fn r_wx(event: r_wxSize(size: {w, _})), ev ->
      :wxEvent.skip(ev)
      :observer_lib.set_listctrl_col_size(lCtrl, w)
    end

    :wxListCtrl.connect(lCtrl, :size, [{:callback, resize}])
    update.()
    {lCtrl, update}
  end

  defp init_state_page(parent, pid, table) do
    win = :observer_lib.html_window(parent)
    cs = :observer_lib.colors(parent)

    update = fn ->
      stateInfo = fetch_state_info(pid)
      html = :observer_html_lib.expandable_term('ProcState', stateInfo, table, cs)
      :wxHtmlWindow.setPage(win, html)
    end

    update.()
    {win, update}
  end

  defp fetch_state_info(pid) do
    case :rpc.call(node(pid), :proc_lib, :translate_initial_call, [pid]) do
      {:proc_lib, :init_p, 5} ->
        []

      {m, _F, _A} ->
        fetch_state_info2(pid, m)

      _ ->
        throw(:process_undefined)
    end
  end

  defp fetch_state_info2(pid, m) do
    i = :rpc.call(node(pid), m, :module_info, [:attributes])

    case :lists.keyfind(:behaviour, 1, i) do
      false ->
        case :lists.keyfind(:behavior, 1, i) do
          false ->
            b = :undefined

          {:behavior, [b]} ->
            b
        end

      {:behaviour, [b]} ->
        b
    end

    case :rpc.call(node(pid), :sys, :get_status, [pid, 200]) do
      {:status, _, {:module, _},
       [_PDict, _SysState, _Parent, _Dbg, [[header, {:data, first}, {:data, second}] | _]]} ->
        [{'Behaviour', b}, header] ++ first ++ second

      {:status, _, {:module, _},
       [_PDict, _SysState, _Parent, _Dbg, [header, {:data, first}, otherFormat]]} ->
        [{'Behaviour', b}, header] ++ first ++ [{'State', otherFormat}]

      {:status, _, {:module, _}, [_PDict, _SysState, _Parent, _Dbg, otherFormat]} ->
        case :lists.keyfind(:format_status, 1, :rpc.call(node(pid), m, :module_info, [:exports])) do
          false ->
            opt = {'Format', :unknown}

          _ ->
            opt = {'Format', :overriden}
        end

        [{'Behaviour', b}, opt, {'State', otherFormat}]

      {:badrpc, {:EXIT, {:timeout, _}}} ->
        []
    end
  end

  defp init_log_page(parent, pid, table) do
    win = :observer_lib.html_window(parent)
    cs = :observer_lib.colors(parent)

    update = fn ->
      fd =
        spawn_link(fn ->
          io_server()
        end)

      :rpc.call(node(pid), :rb, :rescan, [[{:start_log, fd}]])
      :rpc.call(node(pid), :rb, :grep, [local_pid_str(pid)])
      logs = io_get_data(fd)
      pref = global_pid_node_pref(pid)
      expPid = :re.replace(logs, '<0.', '<' ++ pref ++ '.', [:global, {:return, :list}])
      nbBlanks = length(pref) - 1
      re = '(<' ++ pref ++ '.[^>]{1,}>)[ ]{' ++ :erlang.integer_to_list(nbBlanks) ++ '}'
      look = :re.replace(expPid, re, '\\1', [:global, {:return, :list}])
      html = :observer_html_lib.expandable_term('SaslLog', look, table, cs)
      :wxHtmlWindow.setPage(win, html)
    end

    update.()
    {win, update}
  end

  defp create_menus(menuBar) do
    menus = [
      {'File', [r_create_menu(id: 5001, text: 'Close')]},
      {'View', [r_create_menu(id: 601, text: 'Refresh\tCtrl-R')]}
    ]

    :observer_lib.create_menus(menus, menuBar, :new_window)
  end

  defp process_info_fields(pid, wSz) do
    struct = [
      {'Overview',
       [
         {'Initial Call', :initial_call},
         {'Current Function', :current_function},
         {'Registered Name', :registered_name},
         {'Status', :status},
         {'Message Queue Len', :message_queue_len},
         {'Group Leader', {:click, :group_leader}},
         {'Priority', :priority},
         {'Trap Exit', :trap_exit},
         {'Reductions', :reductions},
         {'Binary',
          fn data ->
            stringify_bins(data)
          end},
         {'Last Calls', :last_calls},
         {'Catch Level', :catchlevel},
         {'Trace', :trace},
         {'Suspending', :suspending},
         {'Sequential Trace Token', :sequential_trace_token},
         {'Error Handler', :error_handler}
       ]},
      {:scroll_boxes,
       [
         {'Links', {:click, :links}},
         {'Monitors', {:click, filter_monitor_info()}},
         {'Monitored by', {:click, :monitored_by}}
       ]},
      {'Memory and Garbage Collection', :right,
       [
         {'Memory', {:bytes, :memory}},
         {'Stack and Heaps', {{:words, wSz}, :total_heap_size}},
         {'Heap Size', {{:words, wSz}, :heap_size}},
         {'Stack Size', {{:words, wSz}, :stack_size}},
         {'GC Min Heap Size', {{:words, wSz}, get_gc_info(:min_heap_size)}},
         {'GC FullSweep After', get_gc_info(:fullsweep_after)}
       ]}
    ]

    case :observer_wx.try_rpc(node(pid), :erlang, :process_info, [pid, item_list()]) do
      rawInfo when is_list(rawInfo) ->
        :observer_lib.fill_info(struct, rawInfo)

      _ ->
        throw(:process_undefined)
    end
  end

  defp item_list() do
    [
      :binary,
      :catchlevel,
      :current_function,
      :error_handler,
      :garbage_collection,
      :group_leader,
      :heap_size,
      :initial_call,
      :last_calls,
      :links,
      :memory,
      :message_queue_len,
      :monitored_by,
      :monitors,
      :priority,
      :reductions,
      :registered_name,
      :sequential_trace_token,
      :stack_size,
      :status,
      :suspending,
      :total_heap_size,
      :trace,
      :trap_exit
    ]
  end

  defp get_gc_info(arg) do
    fn data ->
      gC = :proplists.get_value(:garbage_collection, data)
      :proplists.get_value(arg, gC)
    end
  end

  defp filter_monitor_info() do
    fn data ->
      ms = :proplists.get_value(:monitors, data)

      for {_Type, id} <- ms do
        id
      end
    end
  end

  defp stringify_bins(data) do
    bins = :proplists.get_value(:binary, data)

    for {_Ptr, sz, refc} <- bins do
      :lists.flatten(
        :io_lib.format(
          '<< ~s, refc ~w>>',
          [:observer_lib.to_str({:bytes, sz}), refc]
        )
      )
    end
  end

  defp local_pid_str(pid) do
    '<0' ++ :re.replace(:erlang.pid_to_list(pid), '<([0-9]{1,})', '', [{:return, :list}])
  end

  defp global_pid_node_pref(pid) do
    [
      nodePrefix
      | _
    ] = :string.lexemes(:erlang.pid_to_list(pid), '<.')

    nodePrefix
  end

  defp io_get_data(pid) do
    send(pid, {self(), :get_data_and_close})

    receive do
      {^pid, :data, data} ->
        :lists.flatten(data)
    end
  end

  defp io_server() do
    io_server(r_io())
  end

  defp io_server(state) do
    receive do
      {:io_request, from, replyAs, request} ->
        {_, reply, newState} = io_request(request, state)
        send(from, {:io_reply, replyAs, reply})
        io_server(newState)

      {pid, :get_data_and_close} ->
        send(pid, {self(), :data, :lists.reverse(r_io(state, :rdata))})
        :normal

      _Unknown ->
        io_server(state)
    end
  end

  defp io_request(
         {:put_chars, _Encoding, chars},
         state = r_io(rdata: data)
       ) do
    {:ok, :ok, r_io(state, rdata: [chars | data])}
  end

  defp io_request(
         {:put_chars, encoding, module, function, args},
         state
       ) do
    try do
      io_request(
        {:put_chars, encoding, apply(module, function, args)},
        state
      )
    catch
      _, _ ->
        {:error, {:error, function}, state}
    end
  end

  defp io_request(_Req, state) do
    {:ok, {:error, :request}, state}
  end
end
