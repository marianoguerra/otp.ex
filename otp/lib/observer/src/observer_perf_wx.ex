defmodule :m_observer_perf_wx do
  use Bitwise
  import Kernel, except: [to_string: 1]
  @behaviour :wx_object
  require Record
  Record.defrecord(:r_wx, :wx, id: :undefined,
                              obj: :undefined, userData: :undefined,
                              event: :undefined)
  Record.defrecord(:r_wxActivate, :wxActivate, type: :undefined,
                                      active: :undefined)
  Record.defrecord(:r_wxAuiManager, :wxAuiManager, type: :undefined,
                                        manager: :undefined, pane: :undefined,
                                        button: :undefined,
                                        veto_flag: :undefined,
                                        canveto_flag: :undefined,
                                        dc: :undefined)
  Record.defrecord(:r_wxAuiNotebook, :wxAuiNotebook, type: :undefined,
                                         old_selection: :undefined,
                                         selection: :undefined,
                                         drag_source: :undefined)
  Record.defrecord(:r_wxCalendar, :wxCalendar, type: :undefined,
                                      wday: :undefined, date: :undefined)
  Record.defrecord(:r_wxChildFocus, :wxChildFocus, type: :undefined)
  Record.defrecord(:r_wxClipboardText, :wxClipboardText, type: :undefined)
  Record.defrecord(:r_wxClose, :wxClose, type: :undefined)
  Record.defrecord(:r_wxColourPicker, :wxColourPicker, type: :undefined,
                                          colour: :undefined)
  Record.defrecord(:r_wxCommand, :wxCommand, type: :undefined,
                                     cmdString: :undefined,
                                     commandInt: :undefined,
                                     extraLong: :undefined)
  Record.defrecord(:r_wxContextMenu, :wxContextMenu, type: :undefined,
                                         pos: :undefined)
  Record.defrecord(:r_wxDate, :wxDate, type: :undefined,
                                  date: :undefined)
  Record.defrecord(:r_wxDisplayChanged, :wxDisplayChanged, type: :undefined)
  Record.defrecord(:r_wxDropFiles, :wxDropFiles, type: :undefined,
                                       noFiles: :undefined, pos: :undefined,
                                       files: :undefined)
  Record.defrecord(:r_wxErase, :wxErase, type: :undefined,
                                   dc: :undefined)
  Record.defrecord(:r_wxFileDirPicker, :wxFileDirPicker, type: :undefined,
                                           path: :undefined)
  Record.defrecord(:r_wxFocus, :wxFocus, type: :undefined,
                                   win: :undefined)
  Record.defrecord(:r_wxFontPicker, :wxFontPicker, type: :undefined,
                                        font: :undefined)
  Record.defrecord(:r_wxGrid, :wxGrid, type: :undefined,
                                  row: :undefined, col: :undefined,
                                  x: :undefined, y: :undefined,
                                  selecting: :undefined, control: :undefined,
                                  meta: :undefined, shift: :undefined,
                                  alt: :undefined)
  Record.defrecord(:r_wxHelp, :wxHelp, type: :undefined)
  Record.defrecord(:r_wxHtmlLink, :wxHtmlLink, type: :undefined,
                                      linkInfo: :undefined)
  Record.defrecord(:r_wxIconize, :wxIconize, type: :undefined,
                                     iconized: :undefined)
  Record.defrecord(:r_wxIdle, :wxIdle, type: :undefined)
  Record.defrecord(:r_wxInitDialog, :wxInitDialog, type: :undefined)
  Record.defrecord(:r_wxJoystick, :wxJoystick, type: :undefined,
                                      pos: :undefined, zPosition: :undefined,
                                      buttonChange: :undefined,
                                      buttonState: :undefined,
                                      joyStick: :undefined)
  Record.defrecord(:r_wxKey, :wxKey, type: :undefined,
                                 x: :undefined, y: :undefined,
                                 keyCode: :undefined, controlDown: :undefined,
                                 shiftDown: :undefined, altDown: :undefined,
                                 metaDown: :undefined, scanCode: :undefined,
                                 uniChar: :undefined, rawCode: :undefined,
                                 rawFlags: :undefined)
  Record.defrecord(:r_wxList, :wxList, type: :undefined,
                                  code: :undefined, oldItemIndex: :undefined,
                                  itemIndex: :undefined, col: :undefined,
                                  pointDrag: :undefined)
  Record.defrecord(:r_wxMaximize, :wxMaximize, type: :undefined)
  Record.defrecord(:r_wxMenu, :wxMenu, type: :undefined,
                                  menuId: :undefined, menu: :undefined)
  Record.defrecord(:r_wxMouseCaptureChanged, :wxMouseCaptureChanged, type: :undefined)
  Record.defrecord(:r_wxMouseCaptureLost, :wxMouseCaptureLost, type: :undefined)
  Record.defrecord(:r_wxMouse, :wxMouse, type: :undefined,
                                   x: :undefined, y: :undefined,
                                   leftDown: :undefined, middleDown: :undefined,
                                   rightDown: :undefined,
                                   controlDown: :undefined,
                                   shiftDown: :undefined, altDown: :undefined,
                                   metaDown: :undefined,
                                   wheelRotation: :undefined,
                                   wheelDelta: :undefined,
                                   linesPerAction: :undefined)
  Record.defrecord(:r_wxMove, :wxMove, type: :undefined,
                                  pos: :undefined, rect: :undefined)
  Record.defrecord(:r_wxNavigationKey, :wxNavigationKey, type: :undefined,
                                           flags: :undefined, focus: :undefined)
  Record.defrecord(:r_wxNotebook, :wxNotebook, type: :undefined,
                                      nSel: :undefined, nOldSel: :undefined)
  Record.defrecord(:r_wxPaint, :wxPaint, type: :undefined)
  Record.defrecord(:r_wxPaletteChanged, :wxPaletteChanged, type: :undefined)
  Record.defrecord(:r_wxQueryNewPalette, :wxQueryNewPalette, type: :undefined)
  Record.defrecord(:r_wxSash, :wxSash, type: :undefined,
                                  edge: :undefined, dragRect: :undefined,
                                  dragStatus: :undefined)
  Record.defrecord(:r_wxScroll, :wxScroll, type: :undefined,
                                    commandInt: :undefined,
                                    extraLong: :undefined)
  Record.defrecord(:r_wxScrollWin, :wxScrollWin, type: :undefined,
                                       commandInt: :undefined,
                                       extraLong: :undefined)
  Record.defrecord(:r_wxSetCursor, :wxSetCursor, type: :undefined,
                                       x: :undefined, y: :undefined,
                                       cursor: :undefined)
  Record.defrecord(:r_wxShow, :wxShow, type: :undefined,
                                  show: :undefined)
  Record.defrecord(:r_wxSize, :wxSize, type: :undefined,
                                  size: :undefined, rect: :undefined)
  Record.defrecord(:r_wxSpin, :wxSpin, type: :undefined,
                                  commandInt: :undefined)
  Record.defrecord(:r_wxSplitter, :wxSplitter, type: :undefined)
  Record.defrecord(:r_wxStyledText, :wxStyledText, type: :undefined,
                                        position: :undefined, key: :undefined,
                                        modifiers: :undefined,
                                        modificationType: :undefined,
                                        text: :undefined, length: :undefined,
                                        linesAdded: :undefined,
                                        line: :undefined,
                                        foldLevelNow: :undefined,
                                        foldLevelPrev: :undefined,
                                        margin: :undefined, message: :undefined,
                                        wParam: :undefined, lParam: :undefined,
                                        listType: :undefined, x: :undefined,
                                        y: :undefined, dragText: :undefined,
                                        dragAllowMove: :undefined,
                                        dragResult: :undefined)
  Record.defrecord(:r_wxSysColourChanged, :wxSysColourChanged, type: :undefined)
  Record.defrecord(:r_wxTaskBarIcon, :wxTaskBarIcon, type: :undefined)
  Record.defrecord(:r_wxTree, :wxTree, type: :undefined,
                                  item: :undefined, itemOld: :undefined,
                                  pointDrag: :undefined)
  Record.defrecord(:r_wxUpdateUI, :wxUpdateUI, type: :undefined)
  Record.defrecord(:r_wxWindowCreate, :wxWindowCreate, type: :undefined)
  Record.defrecord(:r_wxWindowDestroy, :wxWindowDestroy, type: :undefined)
  Record.defrecord(:r_wxMouseState, :wxMouseState, x: :undefined,
                                        y: :undefined, leftDown: :undefined,
                                        middleDown: :undefined,
                                        rightDown: :undefined,
                                        controlDown: :undefined,
                                        shiftDown: :undefined,
                                        altDown: :undefined,
                                        metaDown: :undefined,
                                        cmdDown: :undefined)
  Record.defrecord(:r_wxHtmlLinkInfo, :wxHtmlLinkInfo, href: :undefined,
                                          target: :undefined)
  Record.defrecord(:r_match_spec, :match_spec, name: '', term: [],
                                      str: [], func: '')
  Record.defrecord(:r_tpattern, :tpattern, m: :undefined,
                                    fa: :undefined, ms: :undefined)
  Record.defrecord(:r_traced_func, :traced_func, func_name: :undefined,
                                       arity: :undefined,
                                       match_spec: :EFE_TODO_NESTED_RECORD)
  Record.defrecord(:r_create_menu, :create_menu, id: :undefined,
                                       text: :undefined, help: [],
                                       type: :append, check: false)
  Record.defrecord(:r_colors, :colors, fg: :undefined,
                                  even: :undefined, odd: :undefined)
  Record.defrecord(:r_attrs, :attrs, even: :undefined,
                                 odd: :undefined, searched: :undefined,
                                 deleted: :undefined, changed_odd: :undefined,
                                 changed_even: :undefined, new_odd: :undefined,
                                 new_even: :undefined)
  Record.defrecord(:r_ti, :ti, tick: 0, disp: 10 / 2,
                              fetch: 2, secs: 60)
  Record.defrecord(:r_win, :win, name: :undefined,
                               panel: :undefined, size: :undefined,
                               geom: :undefined, graphs: [], no_samples: 0,
                               max: :undefined, state: :undefined, info: [])
  Record.defrecord(:r_state, :state, time: :EFE_TODO_NESTED_RECORD,
                                 active: false, parent: :undefined,
                                 samples: :undefined, wins: [],
                                 panel: :undefined, paint: :undefined,
                                 appmon: :undefined)
  Record.defrecord(:r_paint, :paint, font: :undefined,
                                 small: :undefined, fg: :undefined,
                                 pen: :undefined, pen2: :undefined,
                                 pens: :undefined, dot_pens: :undefined,
                                 usegc: false)
  def start_link(notebook, parent, config) do
    :wx_object.start_link(:observer_perf_wx,
                            [notebook, parent, config], [])
  end

  def init([notebook, parent, config]) do
    try do
      panel = :wxPanel.new(notebook)
      main = :wxBoxSizer.new(8)
      memIO = :wxBoxSizer.new(4)
      cPU = make_win(:runq, panel, main,
                       64 ||| 128 ||| 32 ||| 16)
      mEM = make_win(:memory, panel, memIO, 16)
      iO = make_win(:io, panel, memIO, 16 ||| 32)
      :wxSizer.add(main, memIO,
                     [{:flag, 8192 ||| 128}, {:proportion, 1}, {:border, 5}])
      :wxWindow.setSizer(panel, main)
      windows = [cPU, mEM, iO]
      paintInfo = setup_graph_drawing(windows)
      :erlang.process_flag(:trap_exit, true)
      state0 = r_state(parent: parent, panel: panel, wins: windows,
                   paint: paintInfo, samples: reset_data(),
                   time: r_ti(fetch: :maps.get(:fetch, config, 2),
                             secs: :maps.get(:secs, config, 60)))
      {panel, state0}
    catch
      _, err ->
        :io.format('~p crashed ~tp: ~tp~n', [:observer_perf_wx, err, __STACKTRACE__])
        {:stop, err}
    end
  end

  def make_win(name, parent, sizer, border) do
    style = 65536 ||| 4194304
    panel = :wxPanel.new(parent, [{:style, style}])
    opts = [{:flag, 8192 ||| border}, {:proportion, 1},
              {:border, 5}]
    :wxSizer.add(sizer, panel, opts)
    r_win(name: name, panel: panel)
  end

  def setup_graph_drawing(panels) do
    do__ = fn r_win(panel: panel) ->
                :wxWindow.setBackgroundStyle(panel,
                                               :wxe_util.get_const(:wxBG_STYLE_PAINT))
                :wxPanel.connect(panel, :paint, [:callback])
           end
    _ = (for panel <- panels do
           do__.(panel)
         end)
    useGC = haveGC()
    version28 = :wxe_util.get_const(:wxMAJOR_VERSION) === 2 and :wxe_util.get_const(:wxMINOR_VERSION) === 8
    scale = :observer_wx.get_scale()
    {font, smallFont} = (cond do
                           (useGC and version28) ->
                             f = :wxFont.new(scale * 12, 71, 90,
                                               :wxe_util.get_const(:wxFONTWEIGHT_BOLD))
                             sF = :wxFont.new(scale * 10, 71, 90,
                                                :wxe_util.get_const(:wxFONTWEIGHT_NORMAL))
                             {f, sF}
                           true ->
                             defFont = :wxSystemSettings.getFont(17)
                             defSize = :wxFont.getPointSize(defFont)
                             defFamily = :wxFont.getFamily(defFont)
                             f = :wxFont.new(scale * (defSize - 1), defFamily,
                                               90,
                                               :wxe_util.get_const(:wxFONTWEIGHT_BOLD))
                             sF = :wxFont.new(scale * (defSize - 2), defFamily,
                                                90,
                                                :wxe_util.get_const(:wxFONTWEIGHT_NORMAL))
                             {f, sF}
                         end)
    bG = :wxWindow.getBackgroundColour(r_win(hd(panels), :panel))
    fg = (case (:observer_lib.is_darkmode(bG)) do
            false ->
              {0, 0, 0}
            true ->
              :wxSystemSettings.getColour(1 + 14 + 1 + 2)
          end)
    penColor = (case (:observer_lib.is_darkmode(bG)) do
                  false ->
                    {0, 0, 0}
                  true ->
                    {0, 0, 0}
                end)
    pens = (for col <- :erlang.tuple_to_list(colors()) do
              :wxPen.new(col, [{:width, scale}, {:style, 100}])
            end)
    dotPens = (for col <- :erlang.tuple_to_list(colors()) do
                 :wxPen.new(col, [{:width, scale}, {:style, 101}])
               end)
    r_paint(usegc: useGC, font: font, small: smallFont, fg: fg,
        pen: :wxPen.new(penColor),
        pen2: :wxPen.new(penColor, [{:width, scale}]),
        pens: :erlang.list_to_tuple(pens),
        dot_pens: :erlang.list_to_tuple(dotPens))
  end

  def handle_event(r_wx(id: 102,
             event: r_wxCommand(type: :command_menu_selected)),
           r_state(panel: panel, appmon: old, wins: wins0,
               time: r_ti(fetch: f0) = ti0) = state) do
    case (interval_dialog(panel, ti0)) do
      ^ti0 ->
        {:noreply, state}
      r_ti(fetch: ^f0) = ti ->
        wins = (for w <- wins0 do
                  r_win(w, max: :undefined)
                end)
        {:noreply, precalc(r_state(state, time: ti,  wins: wins))}
      ti when old === :undefined ->
        {:noreply, r_state(state, time: ti)}
      ti ->
        {:noreply,
           restart_fetcher(node(old), r_state(state, time: ti))}
    end
  end

  def handle_event(r_wx(event: r_wxCommand(type: :command_menu_selected)),
           state = r_state()) do
    {:noreply, state}
  end

  def handle_event(event, _State) do
    :erlang.error({:unhandled_event, event})
  end

  def handle_sync_event(r_wx(obj: panel, event: r_wxPaint()), _,
           r_state(active: active, time: ti, paint: paint,
               wins: windows)) do
    win = :lists.keyfind(panel, r_win(:panel), windows)
    refresh_panel(active, win, ti, paint)
    :ok
  end

  def refresh_panel(active, r_win(name: _Id, panel: panel) = win, ti,
           r_paint(usegc: useGC) = paint) do
    gC = make_gc(panel, useGC)
    cond do
      active ->
        draw_win(gC, win, ti, paint)
      true ->
        :ignore
    end
    destroy_gc(gC)
  end

  def handle_call(:get_config, _, r_state(time: ti) = state) do
    r_ti(fetch: fetch, secs: range) = ti
    {:reply, %{:fetch => fetch, :secs => range}, state}
  end

  def handle_call(event, from, _State) do
    :erlang.error({:unhandled_call, event, from})
  end

  def handle_cast(event, _State) do
    :erlang.error({:unhandled_cast, event})
  end

  def handle_info({:stats, 1, _, _, _} = stats,
           r_state(panel: panel, samples: data, active: active,
               wins: wins0, appmon: node,
               time: r_ti(tick: tick, disp: disp0) = ti) = state0) do
    cond do
      active ->
        disp = trunc(disp0)
        next = max(tick - disp, 0)
        :erlang.send_after(div(1000, 10), self(),
                             {:refresh, next})
        {wins, samples} = add_data(stats, data, wins0, ti,
                                     active, node)
        state = precalc(r_state(state0, time: r_ti(ti, tick: next), 
                                    wins: wins,  samples: samples))
        :wxWindow.refresh(panel)
        {:noreply, state}
      true ->
        {wins1, samples} = add_data(stats, data, wins0, ti,
                                      active, node)
        wins = (for w <- wins1 do
                  r_win(w, max: :undefined)
                end)
        {:noreply,
           r_state(state0, samples: samples,  wins: wins, 
                       time: r_ti(ti, tick: 0))}
    end
  end

  def handle_info({:refresh, seq},
           r_state(panel: panel,
               time: r_ti(tick: seq, disp: dispF) = ti) = state0)
      when seq + 1 < dispF * 1.5 do
    next = seq + 1
    :erlang.send_after(div(1000, 10), self(),
                         {:refresh, next})
    state = precalc(r_state(state0, time: r_ti(ti, tick: next)))
    (try do
      :wxWindow.refresh(panel)
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end)
    {:noreply, state}
  end

  def handle_info({:refresh, _}, state) do
    {:noreply, state}
  end

  def handle_info({:active, node},
           r_state(parent: parent, panel: panel, appmon: old) = state) do
    create_menus(parent, [])
    try do
      ^node = node(old)
      :wxWindow.refresh(panel)
      :erlang.send_after(div(1000, 10), self(), {:refresh, 0})
      {:noreply, r_state(state, active: true)}
    catch
      _, _ ->
        {:noreply, restart_fetcher(node, state)}
    end
  end

  def handle_info(:not_active, state = r_state(appmon: _Pid)) do
    {:noreply, r_state(state, active: false)}
  end

  def handle_info({:EXIT, old, _}, state = r_state(appmon: old)) do
    {:noreply, r_state(state, active: false,  appmon: :undefined)}
  end

  def handle_info(_Event, state) do
    {:noreply, state}
  end

  def terminate(_Event, r_state(appmon: pid)) do
    (try do
      send(pid, :exit)
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end)
    :ok
  end

  def code_change(_, _, state) do
    state
  end

  defp restart_fetcher(node,
            r_state(appmon: old, panel: panel, time: r_ti(fetch: freq) = ti,
                wins: wins0) = state) do
    (try do
      send(old, :exit)
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end)
    me = self()
    pid = :erlang.spawn_link(node, :observer_backend,
                               :fetch_stats, [me, round(1000 / freq)])
    :wxWindow.refresh(panel)
    wins = (for w <- wins0 do
              r_win(w, state: :undefined)
            end)
    precalc(r_state(state, active: true,  appmon: pid, 
                       samples: reset_data(),  wins: wins, 
                       time: r_ti(ti, tick: 0)))
  end

  defp reset_data() do
    {0, :queue.new()}
  end

  def add_data(stats, q, wins, ti, active) do
    add_data(stats, q, wins, ti, active, :ignore)
  end

  defp add_data(stats, {n, q0}, wins,
            r_ti(fetch: fetch, secs: secs), active, node)
      when n > secs * fetch + 1 do
    {{:value, drop}, q} = :queue.out(q0)
    add_data_1(wins, stats, n, {drop, q}, active, node)
  end

  defp add_data(stats, {n, q}, wins, _, active, node) do
    add_data_1(wins, stats, n + 1, {:empty, q}, active,
                 node)
  end

  defp add_data_1([r_win(state: {_, st}) | _] = wins0, last, n,
            {drop, q}, active, node)
      when st != :undefined do
    try do
      {wins, stat} = :lists.mapfoldl(fn win0, entry ->
                                          {win1, stat} = add_data_2(win0, last,
                                                                      entry)
                                          case (active) do
                                            true ->
                                              win = add_data_3(win1, n, drop,
                                                                 stat, q)
                                              {win, stat}
                                            false ->
                                              {win1, stat}
                                          end
                                     end,
                                       %{}, wins0)
      {wins, {n, :queue.in(%{stat | }, q)}}
    catch
      :no_scheduler_change ->
        {for (r_win(name: id) = win) <- wins0 do
           r_win(win, state: init_data(id, last), 
                    info: info(id, last, node))
         end,
           {0, :queue.new()}}
    end
  end

  defp add_data_1(wins, stats, 1, {_, q}, _, node) do
    {for (r_win(name: id) = win) <- wins do
       r_win(win, state: init_data(id, stats), 
                info: info(id, stats, node))
     end,
       {0, q}}
  end

  defp add_data_2(r_win(name: id, state: s0) = win, stats, map) do
    {v1, s1} = collect_data(id, stats, s0)
    {r_win(win, state: s1), %{map | id => v1}}
  end

  defp add_data_3(r_win(name: id, max: {{oldMax, oldEntry}, _, _, _},
              geom: %{:scale => {wS, hS}}, state: {max, _},
              graphs: graphs) = win,
            n, drop0, last, q1)
      when n > 3 do
    drop = (case (drop0) do
              %{^id => d} ->
                d
              _ ->
                drop0
            end)
    case ({max_value(max), drop === oldEntry}) do
      {^oldMax, false} ->
        %{^id => v4} = last
        {{:value, %{^id => v3}}, q2} = :queue.out_r(q1)
        {{:value, %{^id => v2}}, q3} = :queue.out_r(q2)
        {{:value, %{^id => v1}}, _} = :queue.out_r(q3)
        vals = [v1, v2, v3, v4]
        gs = tuple_size(v1)
        info = :lists.zip(:lists.seq(gs, 1, - 1), graphs)
        lines = (for {i, prev} <- info do
                   add_lines(vals, drop, prev, i, wS, hS)
                 end)
        r_win(win, graphs: lines,  no_samples: n)
      _W ->
        r_win(win, max: :undefined)
    end
  end

  defp add_data_3(win, _, _, _, _) do
    r_win(win, max: :undefined)
  end

  defp create_menus(parent, _) do
    view = {'View', [r_create_menu(id: 102, text: 'Graph Settings')]}
    :observer_wx.create_menus(parent, [{'File', []}, view])
  end

  def interval_dialog(parent0, r_ti(fetch: fetch0, secs: secs0) = ti) do
    parent = :observer_lib.get_wx_parent(parent0)
    dialog = :wxDialog.new(parent, - 1, 'Load Chart Settings',
                             [{:style, 536870912 ||| 2048 ||| 4096 ||| 64}])
    {sl1, fetchSl} = slider(dialog, 'Sample (ms)', trunc(1000 / fetch0),
                              100, 10000)
    {sl2, secsSl} = slider(dialog, 'Length (min)', div(secs0, 60), 1, 10)
    topSizer = :wxBoxSizer.new(8)
    flags = [{:flag, 8192 ||| 64 ||| 16 ||| 32},
               {:border, 5}, {:proportion, 1}]
    :wxSizer.add(topSizer, sl1, flags)
    :wxSizer.add(topSizer, sl2, flags)
    :wxSizer.add(topSizer,
                   :wxDialog.createButtonSizer(dialog, 4 ||| 16), flags)
    :wxWindow.setSizerAndFit(dialog, topSizer)
    :wxSizer.setSizeHints(topSizer, dialog)
    res = (case (:wxDialog.showModal(dialog)) do
             5100 ->
               fetch = 1000 / :wxSlider.getValue(fetchSl)
               secs = :wxSlider.getValue(secsSl) * 60
               r_ti(ti, fetch: fetch,  secs: secs,  disp: 10 / fetch)
             5101 ->
               ti
           end)
    :wxDialog.destroy(dialog)
    res
  end

  defp slider(parent, str, value, min, max) do
    sz = :wxBoxSizer.new(4)
    center = [{:flag, 2048}]
    :wxSizer.add(sz, :wxStaticText.new(parent, - 1, str),
                   [{:proportion, 1} | center])
    opt = [{:style,
              4 ||| :wxe_util.get_const(:wxSL_LABELS)}]
    slider = :wxSlider.new(parent, - 1, value, min, max,
                             opt)
    :wxSizer.add(sz, slider, [{:proportion, 2} | center])
    case (min > 1) do
      false ->
        {sz, slider}
      true ->
        cB = fn r_wx(event: ev), _ ->
                  step(ev, slider, min)
             end
        :wxSlider.connect(slider, :scroll_thumbtrack,
                            [{:callback, cB}])
        :wxSlider.connect(slider, :scroll_changed,
                            [{:callback, cB}])
        {sz, slider}
    end
  end

  defp step(_Ev = r_wxScroll(commandInt: value), slider, min) do
    val = min * round((value / min))
    :wxSlider.setValue(slider, val)
    :ok
  end

  defp mk_max() do
    {0, :undefined}
  end

  defp max_value({max, _}) do
    max
  end

  defp lmax(mState, tuple, tuple) when is_tuple(tuple) do
    lmax(mState, :erlang.tuple_to_list(tuple), tuple)
  end

  defp lmax(mState, values, state) do
    max = max_value(mState)
    new = :lists.max([max | values])
    case (new >= max) do
      false ->
        mState
      true ->
        {new, state}
    end
  end

  defp init_data(:runq, {:stats, _, t0, _, _}) do
    {mk_max(), :lists.sort(t0)}
  end

  defp init_data(:io,
            {:stats, _, _, {{_, in0}, {_, out0}}, _}) do
    {mk_max(), {in0, out0}}
  end

  defp init_data(:memory, _) do
    {mk_max(), info(:memory, :undefined, :undefined)}
  end

  defp init_data(:alloc, _) do
    {mk_max(), :unused}
  end

  defp init_data(:utilz, _) do
    {mk_max(), :unused}
  end

  defp info(:runq, {:stats, _, t0, _, _}, node) do
    dirty = get_dirty_cpu(node)
    {:lists.seq(1, length(t0) - dirty), dirty}
  end

  defp info(:memory, _, _) do
    [:total, :processes, :atom, :binary, :code, :ets]
  end

  defp info(:io, _, _) do
    [:input, :output]
  end

  defp info(:alloc, first, _) do
    for {type, _, _} <- first do
      type
    end
  end

  defp info(:utilz, first, _) do
    for {type, _, _} <- first do
      type
    end
  end

  defp info(_, [], _) do
    []
  end

  defp get_dirty_cpu(node) do
    case (:rpc.call(node(node), :erlang, :system_info,
                      [:dirty_cpu_schedulers])) do
      {:badrpc, _R} ->
        0
      n ->
        n
    end
  end

  defp collect_data(:runq, {:stats, _, t0, _, _}, {max, s0}) do
    s1 = :lists.sort(t0)
    delta = calc_delta(s1, s0)
    sample = :erlang.list_to_tuple(delta)
    {sample, {lmax(max, delta, sample), s1}}
  end

  defp collect_data(:io, {:stats, _, _, {{_, in0}, {_, out0}}, _},
            {max, {pIn, pOut}}) do
    in__ = in0 - pIn
    out = out0 - pOut
    sample = {in__, out}
    {sample, {lmax(max, [in__, out], sample), {in0, out0}}}
  end

  defp collect_data(:memory, {:stats, _, _, _, memInfo},
            {max, memTypes}) do
    vs = (for {type, value} <- memInfo,
                :lists.member(type, memTypes) do
            value
          end)
    sample = :erlang.list_to_tuple(vs)
    {sample, {lmax(max, vs, sample), memTypes}}
  end

  defp collect_data(:alloc, memInfo, max) do
    vs = (for {_Type, _Block, carrier} <- memInfo do
            carrier
          end)
    sample = :erlang.list_to_tuple(vs)
    {sample, {lmax(max, vs, sample), :unused}}
  end

  defp collect_data(:utilz, memInfo, max) do
    vs = (for {_Type, block, carrier} <- memInfo do
            round(100 * block / carrier)
          end)
    sample = :erlang.list_to_tuple(vs)
    {sample, {lmax(max, vs, sample), :unused}}
  end

  defp calc_delta([{id, wN, tN} | ss], [{id, wP, tP} | ps]) do
    [div(100 * (wN - wP), tN - tP) | calc_delta(ss, ps)]
  end

  defp calc_delta([], []) do
    []
  end

  defp calc_delta(_, _) do
    throw(:no_scheduler_change)
  end

  defp precalc(r_state(samples: data0, paint: paint, time: ti,
              wins: wins0) = state) do
    wins = (for win <- wins0 do
              precalc(ti, data0, paint, win)
            end)
    r_state(state, wins: wins)
  end

  def precalc(ti, {noSamples, q}, paint,
           r_win(name: id, panel: panel) = win) do
    size = :wxWindow.getClientSize(panel)
    case (win) do
      r_win(max: max, no_samples: ^noSamples, size: ^size)
          when is_tuple(max) ->
        win
      _SomeThingChanged ->
        hs = (for %{^id => vals} <- :queue.to_list(q) do
                vals
              end)
        max = :lists.foldl(fn vals, max ->
                                lmax(max, vals, vals)
                           end,
                             mk_max(), hs)
        maxDisp = calc_max(id, max)
        %{:scale => {wS, hS}} = (props = window_geom(size,
                                                       maxDisp, ti, panel,
                                                       paint))
        noGraphs = (try do
                      tuple_size(hd(hs))
                    catch
                      _, _ ->
                        0
                    end)
        graphs = (for i <- :lists.seq(noGraphs, 1, - 1) do
                    make_lines(hs, i, wS, hS)
                  end)
        state = (case (r_win(win, :state)) do
                   :undefined ->
                     {max, :undefined}
                   {_, st} ->
                     {max, st}
                 end)
        r_win(win, geom: props,  size: size,  max: maxDisp, 
                 graphs: graphs,  no_samples: noSamples,  state: state)
    end
  end

  defp window_geom({w, h}, {_, max, _Unit, maxUnit},
            r_ti(secs: secs, fetch: fetchFreq), panel,
            r_paint(font: font)) do
    str1 = :observer_lib.to_str(maxUnit)
    str2 = :observer_lib.to_str(div(maxUnit, 2))
    str3 = :observer_lib.to_str(0)
    {tW, tH, _, _} = :wxWindow.getTextExtent(panel, str1,
                                               [{:theFont, font}])
    {spaceW, _, _, _} = :wxWindow.getTextExtent(panel, 'W',
                                                  [{:theFont, font}])
    x0 = 5 + tW + 5
    x1 = w - 5 * 4
    maxTextY = tH + 5
    bottomTextY = h - 5 - tH
    y0 = maxTextY + tH / 2
    y1 = bottomTextY - tH - 5
    scaleW = (x1 - x0 - 1) / (secs * fetchFreq)
    scaleH = (y1 - y0 - 1) / max
    %{:p0 => {x0, y0}, :p1 => {x1, y1},
        :scale => {scaleW, scaleH}, :txsz => {tW, tH, spaceW},
        :txt => {bottomTextY, maxTextY},
        :strs => {str1, str2, str3}}
  end

  defp draw_win(dC,
            r_win(name: name, no_samples: samples,
                geom: %{:scale => {wS, hS}}, graphs: graphs,
                max: {_, max, _, _}, info: info) = win,
            r_ti(tick: tick, fetch: fetchFreq, secs: secs,
                disp: dispFreq) = ti,
            paint = r_paint(pens: pens, dot_pens: dots))
      when (samples >= 2 and graphs !== []) do
    {x0, y0, drawBs} = draw_borders(dC, ti, win, paint)
    offset = tick / dispFreq
    full = (case (samples > 1 + secs * fetchFreq) do
              true ->
                1
              false ->
                2
            end)
    start = x0 + (max(secs * fetchFreq + full - samples,
                        0) - offset) * wS
    last = secs * fetchFreq * wS + x0
    dirty = (case ({name, info}) do
               {:runq, {_, dCpu}} ->
                 dCpu
               _ ->
                 0
             end)
    noGraphs = length(graphs)
    noCpu = noGraphs - dirty
    draw = fn lines0, n ->
                case (dirty > 0 and n > noCpu) do
                  true ->
                    setPen(dC,
                             :erlang.element(1 + rem((n - noCpu - 1), tuple_size(dots)),
                                               dots))
                  false ->
                    setPen(dC,
                             :erlang.element(1 + rem((n - 1), tuple_size(pens)),
                                               pens))
                end
                order = :lists.reverse(lines0)
                [{_, y} | lines] = translate(order, {start, y0}, 0, wS,
                                               {x0, max * hS, last}, [])
                strokeLines(dC, [{last, y} | lines])
                n - 1
           end
    :lists.foldl(draw, noGraphs, graphs)
    drawBs.()
    :ok
  end

  defp draw_win(dC, r_win(no_samples: samples) = win, ti,
            r_paint(fg: fg, small: small) = paint) do
    try do
      draw_borders(dC, ti, win, paint)
    catch
      _, _ ->
        :ok
    else
      {x0, _Y0, drawBs} ->
        text = (case (samples <= 1) do
                  true ->
                    'Waiting for data'
                  false ->
                    'Information not available'
                end)
        setFont(dC, small, fg)
        {_, wW} = getSize(dC)
        drawText(dC, text, x0 + 100, div(wW, 2))
        drawBs.()
        :ok
    end
  end

  defp translate([{x0, y} | rest], {sx, sy} = start, n, wS,
            {cx, cy, cw} = clip, acc) do
    x = min((n - x0) * wS + sx, cw)
    next = (cond do
              x0 > 0 ->
                n
              true ->
                n + 1
            end)
    case (x <= cx) do
      true ->
        translate(rest, start, next, wS, clip,
                    [{cx, sy - min(cy, y)}])
      false ->
        translate(rest, start, next, wS, clip,
                    [{x, sy - min(cy, y)} | acc])
    end
  end

  defp translate([], _, _, _, _, acc) do
    acc
  end

  defp add_lines(vals, drop, oldLines, i, wS, hS) do
    lines = strip(oldLines, drop, 2)
    new = make_lines(vals, i, wS, hS)
    new ++ lines
  end

  defp strip([{x, _} | rest], drop, n) when (x > 0.0001 and
                                            n > 0) do
    strip(rest, drop, n)
  end

  defp strip([_ | rest], drop, n) when n > 0 do
    strip(rest, drop, n - 1)
  end

  defp strip(list, :empty, _) do
    list
  end

  defp strip(list, _, _) do
    :lists.reverse(strip(:lists.reverse(list), :empty, 1))
  end

  defp make_lines(ds = [data | _], n, wS, hS) do
    y = :erlang.element(n, data)
    make_lines(ds, n, wS, hS, y, [])
  end

  defp make_lines([d1 | ds = [d2 | rest]], n, wS, hS, y0, acc0) do
    y1 = :erlang.element(n, d1)
    y2 = :erlang.element(n, d2)
    y3 = (case (rest) do
            [d3 | _] ->
              :erlang.element(n, d3)
            [] ->
              y2
          end)
    this = {0, y1 * hS}
    acc = (cond do
             abs((y1 - y2)) * hS < 3.0 ->
               [this | acc0]
             wS < 3.0 ->
               [this | acc0]
             true ->
               make_splines(y0, y1, y2, y3, wS, hS, [this | acc0])
           end)
    make_lines(ds, n, wS, hS, y1, acc)
  end

  defp make_lines([_D1], _N, _WS, _HS, _Y0, acc) do
    acc
  end

  defp make_splines(y00, y10, y20, y30, wS, hS, acc) do
    y1 = y10 * hS
    y2 = y20 * hS
    steps = min(abs(y1 - y2), wS / 2)
    cond do
      steps > 2 ->
        y0 = y00 * hS
        y3 = y30 * hS
        tan = spline_tan(y0, y1, y2, y3)
        delta = 1 / steps
        splines(steps - 1, 0.0, delta, tan, y1, y2, acc)
      true ->
        acc
    end
  end

  defp splines(n, xD, xD0, tan, y1, y2, acc) when n > 0 do
    delta = xD + xD0
    y = max(0, spline(delta, tan, y1, y2))
    splines(n - 1, delta, xD0, tan, y1, y2,
              [{1.0 - delta, y} | acc])
  end

  defp splines(_N, _XD, _XD0, _Tan, _Y1, _Y2, acc) do
    acc
  end

  defp spline(t, {m1, m2}, y1, y2) do
    t2 = t * t
    t3 = t * t * t
    h1 = 2 * t3 - 3 * t2 + 1
    h2 = - 2 * t3 + 3 * t2
    h3 = t3 - 2 * t2 + t
    h4 = t3 - t2
    m1 * h3 + y1 * h1 + y2 * h2 + m2 * h4
  end

  defp spline_tan(y0, y1, y2, y3) do
    s = 1.0
    c = 0.5
    m1 = s * c * (y2 - y0)
    m2 = s * c * (y3 - y1)
    {m1, m2}
  end

  defp draw_borders(dC, r_ti(secs: secs, fetch: fetchFreq),
            r_win(name: type, geom: geom, info: info,
                max: {_, _, unit, _}),
            r_paint(pen: pen, pen2: pen2, fg: fg, font: font,
                small: small)) do
    %{:p0 => {graphX0, graphY0}, :p1 => {graphX1, graphY1},
        :scale => {scaleW0, _}, :txsz => {tW, tH, spaceW},
        :txt => {bottomTextY, maxTextY},
        :strs => {str1, str2, str3}} = geom
    scaleW = scaleW0 * fetchFreq
    topTextX = 5 * 3 + tW
    secondsY = bottomTextY - tH
    graphY25 = graphY0 + (graphY1 - graphY0) / 4
    graphY50 = graphY0 + (graphY1 - graphY0) / 2
    graphY75 = graphY0 + 3 * (graphY1 - graphY0) / 4
    setFont(dC, small, fg)
    align = fn str, y ->
                 {strW, _} = getTextExtent(dC, str)
                 drawText(dC, str, graphX0 - strW - 5, y)
            end
    align.(str1, maxTextY)
    align.(str2, graphY50 - tH / 2)
    align.(str3, graphY1 - tH / 2 + 1)
    setPen(dC, pen)
    drawSecs = fn sec, {pos, prev} ->
                    str = :observer_lib.to_str(sec) ++ 's'
                    x = graphX0 + pos
                    strokeLine(dC, x, graphY0, x, graphY1 + 5)
                    txtX = x - spaceW
                    case (txtX > prev) do
                      true ->
                        drawText(dC, str, txtX, secondsY)
                        txtW = spaceW * length(str)
                        {pos + 10 * scaleW, txtX + txtW}
                      false ->
                        {pos + 10 * scaleW, prev}
                    end
               end
    :lists.foldl(drawSecs, {0, 0},
                   :lists.seq(secs, 0, - 10))
    strokeLine(dC, graphX0 - 3, graphY25, graphX1, graphY25)
    strokeLine(dC, graphX0 - 3, graphY50, graphX1, graphY50)
    strokeLine(dC, graphX0 - 3, graphY75, graphX1, graphY75)
    setFont(dC, font, fg)
    text = fn x, y, str, penId ->
                cond do
                  penId == 0 ->
                    setFont(dC, font, fg)
                  penId > 0 ->
                    id = 1 + rem((penId - 1), tuple_size(colors()))
                    setFont(dC, font, :erlang.element(id, colors()))
                end
                drawText(dC, str, x, y)
                {strW, _} = getTextExtent(dC, str)
                strW + x + 5 * 2
           end
    case (type) do
      :runq ->
        {textInfo, dirtyCpus} = info
        drawText(dC, 'Scheduler Utilization (%) ', topTextX, 5)
        tN0 = text.(topTextX, bottomTextY, 'Scheduler: ', 0)
        id = fn id, pos0 ->
                  text.(pos0, bottomTextY, :erlang.integer_to_list(id),
                          id)
             end
        tN1 = :lists.foldl(id, tN0, textInfo)
        tN2 = text.(tN1, bottomTextY, 'Dirty cpu: ', 0)
        tN3 = :lists.foldl(id, tN2, :lists.seq(1, dirtyCpus))
        _ = text.(tN3, bottomTextY, '(dotted)', 0)
        :ok
      :memory ->
        drawText(dC, 'Memory Usage ' ++ unit, topTextX, 5)
        :lists.foldl(fn mType, {penId, pos0} ->
                          str = to_string(mType)
                          pos = text.(pos0, bottomTextY, str, penId)
                          {penId + 1, pos}
                     end,
                       {1, topTextX}, info)
      :io ->
        drawText(dC, 'IO Usage ' ++ unit, topTextX, 5)
        :lists.foldl(fn mType, {penId, pos0} ->
                          str = to_string(mType)
                          pos = text.(pos0, bottomTextY, str, penId)
                          {penId + 1, pos}
                     end,
                       {1, topTextX}, info)
      :alloc ->
        drawText(dC, 'Carrier Size ' ++ unit, topTextX, 5)
      :utilz ->
        drawText(dC, 'Carrier Utilization (%)' ++ unit, topTextX, 5)
        :lists.foldl(fn mType, {penId, pos0} ->
                          str = to_string(mType)
                          pos = text.(pos0, bottomTextY, str, penId)
                          {penId + 1, pos}
                     end,
                       {1, topTextX}, info)
    end
    drawBorder = fn () ->
                      setPen(dC, pen2)
                      strokeLines(dC,
                                    [{graphX0, graphY0 - 1},
                                       {graphX0, graphY1 + 1},
                                       {graphX1, graphY1 + 1},
                                       {graphX1, graphY0 - 1},
                                       {graphX0, graphY0 - 1}])
                 end
    {graphX0 + 1, graphY1, drawBorder}
  end

  defp to_string(atom) do
    name = :erlang.atom_to_list(atom)
    case (:lists.reverse(name)) do
      'colla_' ++ rev ->
        uppercase(:lists.reverse(rev))
      _ ->
        uppercase(name)
    end
  end

  defp uppercase([c | rest]) do
    [c - ?a + ?A | rest]
  end

  defp calc_max(type, max) do
    bytes(type, max)
  end

  defp bytes(:runq, max) do
    upper = calc_max1(max_value(max))
    {max, upper, '', upper}
  end

  defp bytes(:utilz, max) do
    upper = calc_max1(max_value(max))
    {max, upper, '', upper}
  end

  defp bytes(_, max) do
    b = max_value(max)
    kB = div(b, 1024)
    mB = div(kB, 1024)
    gB = div(mB, 1024)
    cond do
      gB > 10 ->
        upper = calc_max1(gB)
        {max, upper * 1024 * 1024 * 1024, '(GB)', upper}
      mB > 10 ->
        upper = calc_max1(mB)
        {max, upper * 1024 * 1024, '(MB)', upper}
      kB > 0 ->
        upper = calc_max1(kB)
        {max, upper * 1024, '(KB)', upper}
      true ->
        upper = calc_max1(b)
        {max, upper, '(B)', upper}
    end
  end

  defp calc_max1(max) when max < 10 do
    10
  end

  defp calc_max1(max) do
    case (div(max, 10)) do
      x when x < 10 ->
        case (rem(max, 10)) do
          0 ->
            max
          _ ->
            (x + 1) * 10
        end
      x ->
        10 * calc_max1(x)
    end
  end

  defp colors() do
    {{240, 100, 100}, {0, 128, 0}, {25, 45, 170},
       {255, 165, 0}, {220, 220, 40}, {100, 240, 240},
       {240, 100, 240}, {160, 40, 40}, {100, 100, 240},
       {140, 140, 0}, {25, 200, 100}, {120, 25, 240},
       {255, 140, 163}, {25, 120, 120}, {120, 25, 120},
       {110, 90, 60}}
  end

  def make_gc(panel, useGC) do
    dC = (case (:os.type()) do
            {:win32, _} ->
              dC0 = :wx.typeCast(:wxBufferedPaintDC.new(panel),
                                   :wxPaintDC)
              :wxDC.clear(dC0)
              dC0
            _ ->
              :wxPaintDC.new(panel)
          end)
    cond do
      useGC ->
        {:wxGraphicsContext.create(dC), dC}
      true ->
        {false, dC}
    end
  end

  def destroy_gc({gC, dC}) do
    gC !== false and :wxGraphicsContext.destroy(gC)
    case (dC !== false and :wx.getObjectType(dC)) do
      false ->
        :ok
      type ->
        type.destroy(dC)
    end
  end

  def haveGC() do
    try do
      :wxGraphicsRenderer.getDefaultRenderer()
      true
    catch
      _, _ ->
        false
    end
  end

  defp getSize({_, dC}) do
    :wxDC.getSize(dC)
  end

  def setPen({false, dC}, pen) do
    :wxDC.setPen(dC, pen)
  end

  def setPen({gC, _}, pen) do
    :wxGraphicsContext.setPen(gC, pen)
  end

  def setFont({false, dC}, font, color) do
    :wxDC.setTextForeground(dC, color)
    :wxDC.setFont(dC, font)
  end

  def setFont({gC, _}, font, color) do
    :wxGraphicsContext.setFont(gC, font, color)
  end

  def setBrush({false, dC}, brush) do
    :wxDC.setBrush(dC, brush)
  end

  def setBrush({gC, _}, brush) do
    :wxGraphicsContext.setBrush(gC, brush)
  end

  def strokeLine({false, dC}, x0, y0, x1, y1) do
    :wxDC.drawLine(dC, {round(x0), round(y0)},
                     {round(x1), round(y1)})
  end

  def strokeLine({gC, _}, x0, y0, x1, y1) do
    :wxGraphicsContext.strokeLine(gC, x0, y0, x1, y1)
  end

  def strokeLines(_, [_]) do
    :ok
  end

  def strokeLines({false, dC}, lines) do
    :wxDC.drawLines(dC,
                      for {x, y} <- lines do
                        {round(x), round(y)}
                      end)
  end

  def strokeLines({gC, _}, lines) do
    :wxGraphicsContext.strokeLines(gC, lines)
  end

  def drawRoundedRectangle({false, dC}, x0, y0, x1, y1, r) do
    :wxDC.drawRoundedRectangle(dC, {round(x0), round(y0)},
                                 {round(x1), round(y1)}, round(r))
  end

  def drawRoundedRectangle({gC, _}, x0, y0, x1, y1, r) do
    :wxGraphicsContext.drawRoundedRectangle(gC, x0, y0, x1,
                                              y1, r)
  end

  def drawText({false, dC}, str, x, y) do
    :wxDC.drawText(dC, str, {round(x), round(y)})
  end

  def drawText({gC, _}, str, x, y) do
    :wxGraphicsContext.drawText(gC, str, x, y)
  end

  def getTextExtent({false, dC}, str) do
    :wxDC.getTextExtent(dC, str)
  end

  def getTextExtent({gC, _}, str) do
    {w, h, _, _} = :wxGraphicsContext.getTextExtent(gC, str)
    {w, h}
  end

end