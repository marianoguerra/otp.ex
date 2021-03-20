defmodule :m_wx_object do
  use Bitwise
  import :error_logger, only: [format: 2]
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

  def start(mod, args, options) do
    gen_response(:gen.start(:wx_object, :nolink, mod, args, [:erlang.get(:wx_env) | options]))
  end

  def start(name, mod, args, options) do
    gen_response(
      :gen.start(:wx_object, :nolink, name, mod, args, [:erlang.get(:wx_env) | options])
    )
  end

  def start_link(mod, args, options) do
    gen_response(:gen.start(:wx_object, :link, mod, args, [:erlang.get(:wx_env) | options]))
  end

  def start_link(name, mod, args, options) do
    gen_response(:gen.start(:wx_object, :link, name, mod, args, [:erlang.get(:wx_env) | options]))
  end

  defp gen_response({:ok, pid}) do
    receive do
      {:ack, ^pid, ref = r_wx_ref()} ->
        ref
    end
  end

  defp gen_response(reply) do
    reply
  end

  def stop(ref = r_wx_ref(state: pid)) when is_pid(pid) do
    try do
      :gen.stop(pid)
    catch
      _, exitReason ->
        :erlang.error({exitReason, {:wx_object, :stop, [ref]}})
    end
  end

  def stop(name) when is_atom(name) or is_pid(name) do
    try do
      :gen.stop(name)
    catch
      _, exitReason ->
        :erlang.error({exitReason, {:wx_object, :stop, [name]}})
    end
  end

  def stop(ref = r_wx_ref(state: pid), reason, timeout)
      when is_pid(pid) do
    try do
      :gen.stop(pid, reason, timeout)
    catch
      _, exitReason ->
        :erlang.error({exitReason, {:wx_object, :stop, [ref, reason, timeout]}})
    end
  end

  def stop(name, reason, timeout)
      when is_atom(name) or is_pid(name) do
    try do
      :gen.stop(name, reason, timeout)
    catch
      _, exitReason ->
        :erlang.error({exitReason, {:wx_object, :stop, [name, reason, timeout]}})
    end
  end

  def call(ref = r_wx_ref(state: pid), request) when is_pid(pid) do
    try do
      {:ok, res} = :gen.call(pid, :"$gen_call", request, :infinity)
      res
    catch
      _, reason ->
        :erlang.error({reason, {:wx_object, :call, [ref, request]}})
    end
  end

  def call(name, request)
      when is_atom(name) or is_pid(name) do
    try do
      {:ok, res} = :gen.call(name, :"$gen_call", request, :infinity)
      res
    catch
      _, reason ->
        :erlang.error({reason, {:wx_object, :call, [name, request]}})
    end
  end

  def call(ref = r_wx_ref(state: pid), request, timeout)
      when is_pid(pid) do
    try do
      {:ok, res} = :gen.call(pid, :"$gen_call", request, timeout)
      res
    catch
      _, reason ->
        :erlang.error({reason, {:wx_object, :call, [ref, request, timeout]}})
    end
  end

  def call(name, request, timeout)
      when is_atom(name) or is_pid(name) do
    try do
      {:ok, res} = :gen.call(name, :"$gen_call", request, timeout)
      res
    catch
      _, reason ->
        :erlang.error({reason, {:wx_object, :call, [name, request, timeout]}})
    end
  end

  def send_request(r_wx_ref(state: pid), request) do
    :gen.send_request(pid, :"$gen_call", request)
  end

  def send_request(pid, request) when is_atom(pid) or is_pid(pid) do
    :gen.send_request(pid, :"$gen_call", request)
  end

  def wait_response(requestId) do
    :gen.wait_response(requestId, :infinity)
  end

  def wait_response(requestId, timeout) do
    :gen.wait_response(requestId, timeout)
  end

  def check_response(msg, requestId) do
    :gen.check_response(msg, requestId)
  end

  def cast(r_wx_ref(state: pid), request) when is_pid(pid) do
    send(pid, {:"$gen_cast", request})
    :ok
  end

  def cast(name, request)
      when is_atom(name) or is_pid(name) do
    send(name, {:"$gen_cast", request})
    :ok
  end

  def get_pid(r_wx_ref(state: pid)) when is_pid(pid) do
    pid
  end

  def set_pid(r_wx_ref() = r, pid) when is_pid(pid) do
    r_wx_ref(r, state: pid)
  end

  def reply({to, tag}, reply) do
    try do
      send(to, {tag, reply})
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end
  end

  def init_it(starter, :self, name, mod, args, options) do
    init_it(starter, self(), name, mod, args, options)
  end

  def init_it(starter, parent, name, mod, args, [wxEnv | options]) do
    case wxEnv do
      :undefined ->
        :ok

      _ ->
        :wx.set_env(wxEnv)
    end

    :erlang.put(:_wx_object_, {mod, :_wx_init_})
    debug = debug_options(name, options)

    case (try do
            mod.init(args)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {r_wx_ref() = ref, state} ->
        init_it2(ref, starter, parent, name, state, mod, :infinity, debug)

      {r_wx_ref() = ref, state, timeout} ->
        init_it2(ref, starter, parent, name, state, mod, timeout, debug)

      {:stop, reason} ->
        :proc_lib.init_ack(starter, {:error, reason})
        exit(reason)

      :ignore ->
        :proc_lib.init_ack(starter, :ignore)
        exit(:normal)

      {:EXIT, reason} ->
        :proc_lib.init_ack(starter, {:error, reason})
        exit(reason)

      else__ ->
        error = {:bad_return_value, else__}
        :proc_lib.init_ack(starter, {:error, error})
        exit(error)
    end
  end

  defp init_it2(ref, starter, parent, name, state, mod, timeout, debug) do
    :ok = :wxe_util.register_pid(ref)

    case (try do
            r_wx_ref(ref, :type) === :wxWindow or r_wx_ref(ref, :type).parent_class(:wxWindow)
          catch
            _, _ ->
              false
          end) do
      false ->
        reason = {ref, 'not a wxWindow subclass'}
        :proc_lib.init_ack(starter, {:error, reason})
        exit(reason)

      true ->
        :proc_lib.init_ack(starter, {:ok, self()})
        :proc_lib.init_ack(starter, r_wx_ref(ref, state: self()))
        loop(parent, name, state, mod, timeout, debug)
    end
  end

  defp loop(parent, name, state, mod, time, debug) do
    :erlang.put(:_wx_object_, {mod, state})

    msg =
      receive do
        input ->
          input
      after
        time ->
          :timeout
      end

    case msg do
      {:system, from, req} ->
        :sys.handle_system_msg(req, from, parent, :wx_object, debug, [name, state, mod, time])

      {:EXIT, ^parent, reason} ->
        terminate(reason, name, msg, mod, state, debug)

      {:_wxe_destroy_, _Me} ->
        terminate(:wx_deleted, name, msg, mod, state, debug)

      _Msg when debug === [] ->
        handle_msg(msg, parent, name, state, mod)

      _Msg ->
        debug1 = :sys.handle_debug(debug, &print_event/3, name, {:in, msg})
        handle_msg(msg, parent, name, state, mod, debug1)
    end
  end

  defp dispatch({:"$gen_cast", msg}, mod, state) do
    mod.handle_cast(msg, state)
  end

  defp dispatch(msg = r_wx(), mod, state) do
    mod.handle_event(msg, state)
  end

  defp dispatch(info, mod, state) do
    mod.handle_info(info, state)
  end

  defp handle_msg({:"$gen_call", from, msg}, parent, name, state, mod) do
    case (try do
            mod.handle_call(msg, from, state)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:reply, reply, nState} ->
        reply(from, reply)
        loop(parent, name, nState, mod, :infinity, [])

      {:reply, reply, nState, time1} ->
        reply(from, reply)
        loop(parent, name, nState, mod, time1, [])

      {:noreply, nState} ->
        loop(parent, name, nState, mod, :infinity, [])

      {:noreply, nState, time1} ->
        loop(parent, name, nState, mod, time1, [])

      {:stop, reason, reply, nState} ->
        {:EXIT, r} =
          try do
            terminate(reason, name, msg, mod, nState, [])
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end

        reply(from, reply)
        exit(r)

      other ->
        handle_common_reply(other, name, msg, mod, state, [])
    end
  end

  defp handle_msg(msg, parent, name, state, mod) do
    case (try do
            dispatch(msg, mod, state)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, {:undef, [{^mod, :handle_info, [_, _], _} | _]}} ->
        handle_no_reply({:noreply, state}, parent, name, msg, mod, state, [])

      reply ->
        handle_no_reply(reply, parent, name, msg, mod, state, [])
    end
  end

  defp handle_msg({:"$gen_call", from, msg}, parent, name, state, mod, debug) do
    case (try do
            mod.handle_call(msg, from, state)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:reply, reply, nState} ->
        debug1 = reply(name, from, reply, nState, debug)
        loop(parent, name, nState, mod, :infinity, debug1)

      {:reply, reply, nState, time1} ->
        debug1 = reply(name, from, reply, nState, debug)
        loop(parent, name, nState, mod, time1, debug1)

      {:noreply, nState} ->
        debug1 = :sys.handle_debug(debug, &print_event/3, name, {:noreply, nState})
        loop(parent, name, nState, mod, :infinity, debug1)

      {:noreply, nState, time1} ->
        debug1 = :sys.handle_debug(debug, &print_event/3, name, {:noreply, nState})
        loop(parent, name, nState, mod, time1, debug1)

      {:stop, reason, reply, nState} ->
        {:EXIT, r} =
          try do
            terminate(reason, name, msg, mod, nState, debug)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end

        _ = reply(name, from, reply, nState, debug)
        exit(r)

      other ->
        handle_common_reply(other, name, msg, mod, state, debug)
    end
  end

  defp handle_msg(msg, parent, name, state, mod, debug) do
    reply =
      try do
        dispatch(msg, mod, state)
      catch
        :error, e -> {:EXIT, {e, __STACKTRACE__}}
        :exit, e -> {:EXIT, e}
        e -> e
      end

    handle_no_reply(reply, parent, name, msg, mod, state, debug)
  end

  defp handle_no_reply({:noreply, nState}, parent, name, _Msg, mod, _State, []) do
    loop(parent, name, nState, mod, :infinity, [])
  end

  defp handle_no_reply({:noreply, nState, time1}, parent, name, _Msg, mod, _State, []) do
    loop(parent, name, nState, mod, time1, [])
  end

  defp handle_no_reply({:noreply, nState}, parent, name, _Msg, mod, _State, debug) do
    debug1 = :sys.handle_debug(debug, &print_event/3, name, {:noreply, nState})
    loop(parent, name, nState, mod, :infinity, debug1)
  end

  defp handle_no_reply({:noreply, nState, time1}, parent, name, _Msg, mod, _State, debug) do
    debug1 = :sys.handle_debug(debug, &print_event/3, name, {:noreply, nState})
    loop(parent, name, nState, mod, time1, debug1)
  end

  defp handle_no_reply(reply, _Parent, name, msg, mod, state, debug) do
    handle_common_reply(reply, name, msg, mod, state, debug)
  end

  defp handle_common_reply(reply, name, msg, mod, state, debug) do
    case reply do
      {:stop, reason, nState} ->
        terminate(reason, name, msg, mod, nState, debug)

      {:EXIT, what} ->
        terminate(what, name, msg, mod, state, debug)

      _ ->
        terminate({:bad_return_value, reply}, name, msg, mod, state, debug)
    end
  end

  defp reply(name, {to, tag}, reply, state, debug) do
    reply({to, tag}, reply)
    :sys.handle_debug(debug, &print_event/3, name, {:out, reply, to, state})
  end

  def system_continue(parent, debug, [name, state, mod, time]) do
    loop(parent, name, state, mod, time, debug)
  end

  def system_terminate(reason, _Parent, debug, [name, state, mod, _Time]) do
    terminate(reason, name, [], mod, state, debug)
  end

  def system_code_change([name, state, mod, time], _Module, oldVsn, extra) do
    case (try do
            mod.code_change(oldVsn, state, extra)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, newState} ->
        {:ok, [name, newState, mod, time]}

      else__ ->
        else__
    end
  end

  defp print_event(dev, {:in, msg}, name) do
    case msg do
      {:"$gen_call", {from, _Tag}, call} ->
        :io.format(dev, '*DBG* ~tp got call ~tp from ~w~n', [name, call, from])

      {:"$gen_cast", cast} ->
        :io.format(dev, '*DBG* ~tp got cast ~tp~n', [name, cast])

      _ ->
        :io.format(dev, '*DBG* ~tp got ~tp~n', [name, msg])
    end
  end

  defp print_event(dev, {:out, msg, to, state}, name) do
    :io.format(dev, '*DBG* ~tp sent ~tp to ~w, new state ~tp~n', [name, msg, to, state])
  end

  defp print_event(dev, {:noreply, state}, name) do
    :io.format(dev, '*DBG* ~tp new state ~tp~n', [name, state])
  end

  defp print_event(dev, event, name) do
    :io.format(dev, '*DBG* ~tp dbg  ~tp~n', [name, event])
  end

  defp terminate(reason, name, msg, mod, state, debug) do
    case try_terminate(mod, reason, state) do
      {:EXIT, r} ->
        error_info(r, name, msg, state, debug)
        exit(r)

      _ ->
        case reason do
          :normal ->
            exit(:normal)

          :shutdown ->
            exit(:shutdown)

          :wx_deleted ->
            exit(:normal)

          _ ->
            error_info(reason, name, msg, state, debug)
            exit(reason)
        end
    end
  end

  defp try_terminate(mod, reason, state) do
    case :erlang.function_exported(mod, :terminate, 2) do
      true ->
        try do
          mod.terminate(reason, state)
        catch
          :error, e -> {:EXIT, {e, __STACKTRACE__}}
          :exit, e -> {:EXIT, e}
          e -> e
        end

      _ ->
        :ok
    end
  end

  defp error_info(_Reason, :application_controller, _Msg, _State, _Debug) do
    :ok
  end

  defp error_info(reason, name, msg, state, debug) do
    reason1 =
      case reason do
        {:undef, [{m, f, a, l} | mFAs]} ->
          case :code.is_loaded(m) do
            false ->
              {:"module could not be loaded", [{m, f, a, l} | mFAs]}

            _ ->
              case :erlang.function_exported(m, f, length(a)) do
                true ->
                  reason

                false ->
                  {:"function not exported", [{m, f, a, l} | mFAs]}
              end
          end

        _ ->
          reason
      end

    format(
      '** wx object server ~tp terminating \n** Last message in was ~tp~n** When Server state == ~tp~n** Reason for termination == ~n** ~tp~n',
      [name, msg, state, reason1]
    )

    :sys.print_log(debug)
    :ok
  end

  defp opt(op, [{op, value} | _]) do
    {:ok, value}
  end

  defp opt(op, [_ | options]) do
    opt(op, options)
  end

  defp opt(_, []) do
    false
  end

  defp debug_options(name, opts) do
    case opt(:debug, opts) do
      {:ok, options} ->
        dbg_opts(name, options)

      _ ->
        []
    end
  end

  defp dbg_opts(name, opts) do
    case (try do
            :sys.debug_options(opts)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        format('~tp: ignoring erroneous debug options - ~tp~n', [name, opts])
        []

      dbg ->
        dbg
    end
  end

  def format_status(opt, statusData) do
    [pDict, sysState, parent, debug, [name, state, mod, _Time]] = statusData
    header = :gen.format_status_header('Status for wx object ', name)
    log = :sys.get_log(debug)

    specific =
      case format_status(opt, mod, pDict, state) do
        s when is_list(s) ->
          s

        s ->
          [s]
      end

    [
      {:header, header},
      {:data,
       [
         {'Status', sysState},
         {'Parent', parent},
         {'Logged events',
          format_log_state(
            mod,
            log
          )}
       ]}
      | specific
    ]
  end

  defp format_log_state(mod, log) do
    for event <- log do
      case event do
        {:out, msg, from, state} ->
          {:out, msg, from, format_status(:terminate, mod, :erlang.get(), state)}

        {:noreply, state} ->
          {:noreply, format_status(:terminate, mod, :erlang.get(), state)}

        _ ->
          event
      end
    end
  end

  defp format_status(opt, mod, pDict, state) do
    defStatus =
      case opt do
        :terminate ->
          state

        _ ->
          [{:data, [{'State', state}]}]
      end

    case :erlang.function_exported(mod, :format_status, 2) do
      true ->
        case (try do
                mod.format_status(opt, [pDict, state])
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          {:EXIT, _} ->
            defStatus

          else__ ->
            else__
        end

      _ ->
        defStatus
    end
  end
end
