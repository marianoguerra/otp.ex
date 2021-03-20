defmodule :m_wxe_server do
  use Bitwise
  @behaviour :gen_server
  require Record

  Record.defrecord(:r_state, :state,
    port: :undefined,
    cb_port: :undefined,
    users: :undefined,
    cleaners: [],
    cb: :undefined,
    cb_cnt: :undefined
  )

  Record.defrecord(:r_user, :user, events: [])
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

  def start(silentStart) do
    case :erlang.get(:wx_env) do
      :undefined ->
        case :gen_server.start(:wxe_server, [silentStart], []) do
          {:ok, pid} ->
            {:ok, port} = :gen_server.call(pid, :get_port, :infinity)
            :wx.set_env(env = r_wx_env(port: port, sv: pid))
            env

          {:error, {reason, _Stack}} ->
            :erlang.error(reason)
        end

      env = r_wx_env(sv: pid) ->
        case :erlang.is_process_alive(pid) do
          true ->
            env

          false ->
            :erlang.erase(:wx_env)
            start(silentStart)
        end
    end
  end

  def stop() do
    r_wx_env(sv: pid) = :erlang.get(:wx_env)

    try do
      :gen_server.call(pid, :stop, :infinity)
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end

    :ok
  end

  def register_me(pid) do
    :ok = :gen_server.call(pid, :register_me, :infinity)
  end

  def set_debug(pid, level) do
    :gen_server.cast(pid, {:debug, level})
  end

  def init([silentStart]) do
    {port, cBPort} = :wxe_master.init_port(silentStart)
    :erlang.put(:wx_env, r_wx_env(port: port, sv: self()))

    {:ok,
     r_state(
       port: port,
       cb_port: cBPort,
       users: :gb_trees.empty(),
       cb: :gb_trees.empty(),
       cb_cnt: 1
     )}
  end

  def handle_call(:register_me, {from, _}, state = r_state(users: users)) do
    :erlang.monitor(:process, from)

    case :gb_trees.is_defined(from, users) do
      true ->
        {:reply, :ok, state}

      false ->
        new = :gb_trees.insert(from, r_user(), users)
        {:reply, :ok, r_state(state, users: new)}
    end
  end

  def handle_call(:get_port, _, state = r_state(port: port)) do
    {:reply, {:ok, port}, state}
  end

  def handle_call({:connect_cb, obj, msg}, {from, _}, state) do
    handle_connect(obj, msg, from, state)
  end

  def handle_call({:disconnect_cb, obj, msg}, {from, _}, state) do
    handle_disconnect(obj, msg, from, state)
  end

  def handle_call(:stop, {_From, _}, state = r_state(users: users0, cleaners: cs0)) do
    env = :erlang.get(:wx_env)
    users = :gb_trees.to_list(users0)

    cs =
      :lists.map(
        fn {_Pid, user} ->
          spawn_link(fn ->
            cleanup(env, [user])
          end)
        end,
        users
      )

    {:noreply,
     r_state(state,
       users: :gb_trees.empty(),
       cleaners: cs ++ cs0
     )}
  end

  def handle_call({:register_cb, fun}, _, state0) do
    {funId, state} = attach_fun(fun, state0)
    {:reply, funId, state}
  end

  def handle_call(_Request, _From, state) do
    log(:wxe_server, 133, 'Unknown request ~p sent to ~p from ~p ~n', [
      _Request,
      :wxe_server,
      _From
    ])

    reply = :ok
    {:reply, reply, state}
  end

  def handle_cast(
        {:cleaned, from},
        state = r_state(users: users, cleaners: cs0)
      ) do
    cs = :lists.delete(from, cs0)

    case cs === [] and :gb_trees.is_empty(users) do
      true ->
        {:stop, :normal, r_state(state, cleaners: cs)}

      false ->
        {:noreply, r_state(state, cleaners: cs)}
    end
  end

  def handle_cast({:debug, level}, state) do
    env = :erlang.get(:wx_env)
    :erlang.put(:wx_env, r_wx_env(env, debug: level))
    {:noreply, state}
  end

  def handle_cast(_Msg, state) do
    log(:wxe_server, 152, 'Unknown message ~p sent to ~p~n', [_Msg, :wxe_server])
    {:noreply, state}
  end

  def handle_info(cb = {_, _, :_wx_invoke_cb_}, state) do
    invoke_cb(cb, state)
    {:noreply, state}
  end

  def handle_info({:wx_delete_cb, funId}, state)
      when is_integer(funId) do
    {:noreply, delete_fun(funId, state)}
  end

  def handle_info(
        {:wx_delete_cb, id, evtListener, obj},
        state = r_state(users: users)
      ) do
    from = :erlang.erase(evtListener)

    case :gb_trees.lookup(from, users) do
      :none ->
        {:noreply, delete_fun(id, state)}

      {:value, user0} ->
        user = cleanup_evt_listener(user0, evtListener, obj)

        {:noreply,
         delete_fun(
           id,
           r_state(state, users: :gb_trees.update(from, user, users))
         )}
    end
  end

  def handle_info(
        {:DOWN, _, :process, pid, _},
        state = r_state(users: users0, cleaners: cs)
      ) do
    try do
      user = :gb_trees.get(pid, users0)
      users = :gb_trees.delete(pid, users0)
      env = :wx.get_env()

      case user do
        r_user(events: []) ->
          case cs === [] and :gb_trees.is_empty(users) do
            true ->
              {:stop, :normal, r_state(state, users: users)}

            false ->
              {:noreply, r_state(state, users: users)}
          end

        _ ->
          cleaner =
            spawn_link(fn ->
              cleanup(env, [user])
            end)

          {:noreply, r_state(state, users: users, cleaners: [cleaner | cs])}
      end
    catch
      _E, _R ->
        {:noreply, state}
    end
  end

  def handle_info(_Info, state) do
    log(:wxe_server, 197, 'Unknown message ~p sent to ~p~n', [_Info, :wxe_server])
    {:noreply, state}
  end

  def terminate(_Reason, _State) do
    :shutdown
  end

  def code_change(_OldVsn, state, _Extra) do
    {:ok, state}
  end

  defp log(mod, line, str, args) do
    :error_logger.format('~p:~p: ' ++ str, [mod, line | args])
  end

  defp handle_connect(object, r_evh(handler: :undefined, cb: callback) = evData0, from, state0) do
    {funId, state} = attach_fun(callback, state0)
    evData1 = r_evh(evData0, cb: funId)

    case :wxEvtHandler.connect_impl(object, evData1) do
      {:ok, handler} ->
        evData =
          r_evh(evData1,
            handler: handler,
            userdata: :undefined
          )

        handle_connect(object, evData, from, state)

      error ->
        {:reply, error, state0}
    end
  end

  defp handle_connect(
         object,
         evData = r_evh(handler: handler),
         from,
         state0 = r_state(users: users)
       ) do
    :erlang.put(handler, from)

    case :gb_trees.lookup(from, users) do
      {:value, user0 = r_user(events: listeners0)} ->
        user = r_user(user0, events: [{object, evData} | listeners0])
        state = r_state(state0, users: :gb_trees.update(from, user, users))
        {:reply, :ok, state}

      :none ->
        {:reply, {:error, :terminating}, state0}
    end
  end

  defp invoke_cb({{ev = r_wx(), ref = r_wx_ref()}, funId, _}, _S) do
    case :erlang.get(funId) do
      {{:nospawn, fun}, _} when is_function(fun) ->
        invoke_callback_fun(fn ->
          fun.(ev, ref)
          <<>>
        end)

      {fun, _} when is_function(fun) ->
        invoke_callback(fn ->
          fun.(ev, ref)
          <<>>
        end)

      {pid, _} when is_pid(pid) ->
        invoke_callback(pid, ev, ref)

      err ->
        log(:wxe_server, 250, 'Internal Error ~p~n', [err])
    end
  end

  defp invoke_cb({funId, args, _}, _S)
       when is_list(args) and
              is_integer(funId) do
    case :erlang.get(funId) do
      {fun, _} when is_function(fun) ->
        invoke_callback(fn ->
          fun.(args)
        end)

      err ->
        log(:wxe_server, 258, 'Internal Error ~p ~p ~p~n', [err, funId, args])
    end
  end

  def invoke_callback(fun) do
    env = :erlang.get(:wx_env)

    spawn(fn ->
      :wx.set_env(env)
      invoke_callback_fun(fun)
    end)

    :ok
  end

  defp invoke_callback(pid, ev, ref) do
    env = :erlang.get(:wx_env)

    cB = fn ->
      :wx.set_env(env)
      :wxe_util.cast(8, <<>>)

      try do
        case get_wx_object_state(pid, 5) do
          :ignore ->
            :wxEvent.skip(ref)

          {mod, state} ->
            case mod.handle_sync_event(ev, ref, state) do
              :ok ->
                :ok

              :noreply ->
                :ok

              return ->
                exit({:bad_return, return})
            end
        end
      catch
        _, reason ->
          :wxEvent.skip(ref)

          log(:wxe_server, 288, 'Callback fun crashed with {\'EXIT, ~p, ~p}~n', [
            reason,
            __STACKTRACE__
          ])
      end

      :wxe_util.cast(5, <<>>)
    end

    spawn(cB)
    :ok
  end

  defp invoke_callback_fun(fun) do
    :wxe_util.cast(8, <<>>)

    res =
      try do
        return = fun.()
        true = is_binary(return)
        return
      catch
        _, reason ->
          log(:wxe_server, 303, 'Callback fun crashed with {\'EXIT, ~p, ~p}~n', [
            reason,
            __STACKTRACE__
          ])

          <<>>
      end

    :wxe_util.cast(5, res)
  end

  defp get_wx_object_state(pid, n) when n > 0 do
    case :erlang.process_info(pid, :dictionary) do
      {:dictionary, dict} ->
        case :lists.keysearch(:_wx_object_, 1, dict) do
          {:value, {:_wx_object_, {_Mod, :_wx_init_}}} ->
            :timer.sleep(50)
            get_wx_object_state(pid, n - 1)

          {:value, {:_wx_object_, value}} ->
            value

          _ ->
            :ignore
        end

      _ ->
        :ignore
    end
  end

  defp get_wx_object_state(_, _) do
    :ignore
  end

  defp attach_fun(fun, s = r_state(cb: cB, cb_cnt: next)) do
    case :gb_trees.lookup(fun, cB) do
      {:value, iD} ->
        {^fun, n} = :erlang.get(iD)
        :erlang.put(iD, {fun, n + 1})
        {iD, s}

      :none ->
        :erlang.put(next, {fun, 1})

        {next,
         r_state(s,
           cb: :gb_trees.insert(fun, next, cB),
           cb_cnt: next + 1
         )}
    end
  end

  defp delete_fun(0, state) do
    state
  end

  defp delete_fun(funId, state = r_state(cb: cB)) do
    case :erlang.get(funId) do
      :undefined ->
        state

      {fun, n} when n < 2 ->
        :erlang.erase(funId)
        r_state(state, cb: :gb_trees.delete(fun, cB))

      {fun, n} ->
        :erlang.put(funId, {fun, n - 1})
        state
    end
  end

  defp cleanup_evt_listener(u = r_user(events: evs0), evtListener, object) do
    filter = fn {obj, r_evh(handler: evl)} ->
      not (object === obj and evl === evtListener)
    end

    r_user(u, events: :lists.filter(filter, evs0))
  end

  defp handle_disconnect(
         object,
         evh = r_evh(cb: fun),
         from,
         state0 = r_state(users: users0, cb: callbacks)
       ) do
    r_user(events: evs0) = :gb_trees.get(from, users0)
    funId = :gb_trees.lookup(fun, callbacks)
    handlers = find_handler(evs0, object, r_evh(evh, cb: funId))
    {:reply, {:try_in_order, handlers}, state0}
  end

  defp find_handler([{object, evh} | evs], object, match) do
    case match_handler(match, evh) do
      false ->
        find_handler(evs, object, match)

      res ->
        [res | find_handler(evs, object, match)]
    end
  end

  defp find_handler([_ | evs], object, match) do
    find_handler(evs, object, match)
  end

  defp find_handler([], _, _) do
    []
  end

  defp match_handler(
         m = r_evh(et: mET, cb: mCB),
         r_evh(et: eT, cb: cB, handler: handler)
       ) do
    match = match_et(mET, eT) and match_cb(mCB, cB)
    match and r_evh(m, handler: handler)
  end

  defp match_et(:null, _) do
    true
  end

  defp match_et(met, et) do
    met === et
  end

  defp match_cb(:none, _) do
    true
  end

  defp match_cb({:value, mId}, id) do
    mId === id
  end

  defp cleanup(env, data) do
    :erlang.put(:wx_env, env)

    disconnect = fn {object, ev} ->
      try do
        :wxEvtHandler.disconnect_impl(object, ev)
      catch
        _, _ ->
          :ok
      end
    end

    :lists.foreach(
      fn r_user(events: evs) ->
        for ev <- evs do
          disconnect.(ev)
        end
      end,
      data
    )

    :gen_server.cast(r_wx_env(env, :sv), {:cleaned, self()})
    :normal
  end
end
