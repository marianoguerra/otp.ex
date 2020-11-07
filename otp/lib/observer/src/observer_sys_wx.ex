defmodule :m_observer_sys_wx do
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

  Record.defrecord(:r_sys_wx_state, :sys_wx_state,
    parent: :undefined,
    node: :undefined,
    parent_notebook: :undefined,
    panel: :undefined,
    sizer: :undefined,
    menubar: :undefined,
    fields: :undefined,
    timer: :undefined
  )

  def start_link(notebook, parent, config) do
    :wx_object.start_link(:observer_sys_wx, [notebook, parent, config], [])
  end

  def init([notebook, parent, config]) do
    sysInfo = :observer_backend.sys_info()
    {sys, mem, cpu, stats, limits} = info_fields()
    panel = :wxPanel.new(notebook)
    sizer = :wxBoxSizer.new(8)
    hSizer0 = :wxBoxSizer.new(4)

    {fPanel0, _FSizer0, fields0} =
      :observer_lib.display_info(
        panel,
        :observer_lib.fill_info(
          sys,
          sysInfo
        )
      )

    {fPanel1, _FSizer1, fields1} =
      :observer_lib.display_info(
        panel,
        :observer_lib.fill_info(
          mem,
          sysInfo
        )
      )

    :wxSizer.add(hSizer0, fPanel0, [{:flag, 8192}, {:proportion, 1}])
    :wxSizer.add(hSizer0, fPanel1, [{:flag, 8192}, {:proportion, 1}])
    hSizer1 = :wxBoxSizer.new(4)

    {fPanel2, _FSizer2, fields2} =
      :observer_lib.display_info(
        panel,
        :observer_lib.fill_info(
          cpu,
          sysInfo
        )
      )

    {fPanel3, _FSizer3, fields3} =
      :observer_lib.display_info(
        panel,
        :observer_lib.fill_info(
          stats,
          sysInfo
        )
      )

    :wxSizer.add(hSizer1, fPanel2, [{:flag, 8192}, {:proportion, 1}])
    :wxSizer.add(hSizer1, fPanel3, [{:flag, 8192}, {:proportion, 1}])
    hSizer2 = :wxBoxSizer.new(4)

    {fPanel4, _FSizer4, fields4} =
      :observer_lib.display_info(
        panel,
        :observer_lib.fill_info(
          limits,
          sysInfo
        )
      )

    :wxSizer.add(hSizer2, fPanel4, [{:flag, 8192}, {:proportion, 1}])
    borderFlags = 16 ||| 32

    :wxSizer.add(sizer, hSizer0, [
      {:flag, 8192 ||| borderFlags ||| 64},
      {:proportion, 0},
      {:border, 5}
    ])

    :wxSizer.add(sizer, hSizer1, [
      {:flag, 8192 ||| borderFlags ||| 128},
      {:proportion, 0},
      {:border, 5}
    ])

    :wxSizer.add(sizer, hSizer2, [
      {:flag, 8192 ||| borderFlags ||| 128},
      {:proportion, 0},
      {:border, 5}
    ])

    :wxPanel.setSizer(panel, sizer)
    timer = :observer_lib.start_timer(config, 10)

    {panel,
     r_sys_wx_state(
       parent: parent,
       parent_notebook: notebook,
       panel: panel,
       sizer: sizer,
       timer: timer,
       fields: fields0 ++ fields1 ++ fields2 ++ fields3 ++ fields4
     )}
  end

  defp create_sys_menu(parent) do
    view =
      {'View',
       [
         r_create_menu(id: 101, text: 'Refresh\tCtrl-R'),
         r_create_menu(id: 102, text: 'Refresh interval')
       ]}

    :observer_wx.create_menus(parent, [view])
  end

  defp update_syspage(r_sys_wx_state(node: :undefined)) do
    :ignore
  end

  defp update_syspage(r_sys_wx_state(node: node, fields: fields, sizer: sizer)) do
    sysInfo = :observer_wx.try_rpc(node, :observer_backend, :sys_info, [])
    {sys, mem, cpu, stats, limits} = info_fields()

    :observer_lib.update_info(
      fields,
      :observer_lib.fill_info(
        sys,
        sysInfo
      ) ++
        :observer_lib.fill_info(
          mem,
          sysInfo
        ) ++
        :observer_lib.fill_info(
          cpu,
          sysInfo
        ) ++
        :observer_lib.fill_info(
          stats,
          sysInfo
        ) ++
        :observer_lib.fill_info(
          limits,
          sysInfo
        )
    )

    :wxSizer.layout(sizer)
  end

  defp maybe_convert(:undefined) do
    'Not available'
  end

  defp maybe_convert(v) do
    :observer_lib.to_str(v)
  end

  defp get_dist_buf_busy_limit_info() do
    fn data ->
      maybe_convert(
        :proplists.get_value(
          :dist_buf_busy_limit,
          data
        )
      )
    end
  end

  defp get_limit_count_info(count, limit) do
    fn data ->
      c = :proplists.get_value(count, data)
      l = :proplists.get_value(limit, data)

      :lists.flatten(
        :io_lib.format(
          '~s / ~s ~s',
          [
            maybe_convert(c),
            maybe_convert(l),
            cond do
              c === :undefined ->
                ''

              l === :undefined ->
                ''

              true ->
                :io_lib.format(
                  '(~s % used)',
                  [:observer_lib.to_str({:trunc, c / l * 100})]
                )
            end
          ]
        )
      )
    end
  end

  defp info_fields() do
    sys = [
      {'System and Architecture',
       [
         {'System Version', :otp_release},
         {'ERTS Version', :version},
         {'Compiled for', :system_architecture},
         {'Emulator Wordsize', :wordsize_external},
         {'Process Wordsize', :wordsize_internal},
         {'SMP Support', :smp_support},
         {'Thread Support', :threads},
         {'Async thread pool size', :thread_pool_size}
       ]}
    ]

    cpu = [
      {'CPU\'s and Threads',
       [
         {'Logical CPU\'s', :logical_processors},
         {'Online Logical CPU\'s', :logical_processors_online},
         {'Available Logical CPU\'s', :logical_processors_available},
         {'Schedulers', :schedulers},
         {'Online schedulers', :schedulers_online},
         {'Available schedulers', :schedulers_available}
       ]}
    ]

    mem = [
      {'Memory Usage', :right,
       [
         {'Total', {:bytes, :total}},
         {'Processes', {:bytes, :processes}},
         {'Atoms', {:bytes, :atom}},
         {'Binaries', {:bytes, :binary}},
         {'Code', {:bytes, :code}},
         {'ETS', {:bytes, :ets}}
       ]}
    ]

    stats = [
      {'Statistics', :right,
       [
         {'Up time', {:time_ms, :uptime}},
         {'Run Queue', :run_queue},
         {'IO Input', {:bytes, :io_input}},
         {'IO Output', {:bytes, :io_output}}
       ]}
    ]

    limits = [
      {'System statistics / limit',
       [
         {'Atoms', get_limit_count_info(:atom_count, :atom_limit)},
         {'Processes', get_limit_count_info(:process_count, :process_limit)},
         {'Ports', get_limit_count_info(:port_count, :port_limit)},
         {'ETS', get_limit_count_info(:ets_count, :ets_limit)},
         {'Distribution buffer busy limit', get_dist_buf_busy_limit_info()}
       ]}
    ]

    {sys, mem, cpu, stats, limits}
  end

  def handle_info(
        :refresh_interval,
        r_sys_wx_state(panel: panel, node: node) = state
      ) do
    try do
      update_syspage(state)
    catch
      :error, {:badrpc, _} ->
        :observer_wx.return_to_localnode(panel, node)
    end

    {:noreply, state}
  end

  def handle_info(
        {:active, node},
        r_sys_wx_state(parent: parent, panel: panel, timer: timer) = state
      ) do
    updState = r_sys_wx_state(state, node: node)
    create_sys_menu(parent)

    try do
      update_syspage(updState)
      {:noreply, r_sys_wx_state(updState, timer: :observer_lib.start_timer(timer))}
    catch
      :error, {:badrpc, _} ->
        :observer_wx.return_to_localnode(panel, node)
        {:noreply, state}
    end
  end

  def handle_info(:not_active, r_sys_wx_state(timer: timer) = state) do
    {:noreply, r_sys_wx_state(state, timer: :observer_lib.stop_timer(timer))}
  end

  def handle_info(info, state) do
    :io.format('~p:~p: Unhandled info: ~tp~n', [:observer_sys_wx, 202, info])
    {:noreply, state}
  end

  def terminate(_Reason, _State) do
    :ok
  end

  def code_change(_, _, state) do
    {:ok, state}
  end

  def handle_call(:get_config, _, r_sys_wx_state(timer: timer) = state) do
    {:reply, :observer_lib.timer_config(timer), state}
  end

  def handle_call(msg, _From, state) do
    :io.format('~p~p: Unhandled Call ~tp~n', [:observer_sys_wx, 215, msg])
    {:reply, :ok, state}
  end

  def handle_cast(msg, state) do
    :io.format('~p~p: Unhandled cast ~tp~n', [:observer_sys_wx, 219, msg])
    {:noreply, state}
  end

  def handle_event(
        r_wx(
          id: 101,
          event: r_wxCommand(type: :command_menu_selected)
        ),
        r_sys_wx_state(node: node, panel: panel) = state
      ) do
    try do
      update_syspage(state)
    catch
      :error, {:badrpc, _} ->
        :observer_wx.return_to_localnode(panel, node)
    end

    {:noreply, state}
  end

  def handle_event(
        r_wx(
          id: 102,
          event: r_wxCommand(type: :command_menu_selected)
        ),
        r_sys_wx_state(timer: timer0, parent_notebook: notebook) = state
      ) do
    timer = :observer_lib.interval_dialog(notebook, timer0, 1, 5 * 60)
    {:noreply, r_sys_wx_state(state, timer: timer)}
  end

  def handle_event(event, state) do
    :io.format('~p:~p: Unhandled event ~tp\n', [:observer_sys_wx, 238, event])
    {:noreply, state}
  end
end
