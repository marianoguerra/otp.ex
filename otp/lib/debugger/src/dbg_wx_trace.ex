defmodule :m_dbg_wx_trace do
  use Bitwise
  require Record

  Record.defrecord(:r_state, :state,
    win: :undefined,
    coords: :undefined,
    pid: :undefined,
    meta: :undefined,
    status: :undefined,
    cm: :undefined,
    cm_obsolete: false,
    stack: :undefined,
    trace: :undefined,
    stack_trace: :undefined,
    backtrace: :undefined,
    strings: :undefined
  )

  def start(pid) do
    start(pid, [:"Button Area", :"Evaluator Area", :"Bindings Area"], 100, [:str_on])
  end

  def start(pid, traceWin, backTrace) do
    start(pid, traceWin, backTrace, [:str_on])
  end

  def start(pid, traceWin, backTrace, strings) do
    case :erlang.whereis(:dbg_wx_mon) do
      :undefined ->
        parent = :wx.new()
        env = :wx.get_env()
        start(pid, env, parent, traceWin, backTrace, strings)

      monitor when is_pid(monitor) ->
        send(monitor, {:dbg_wx_trace, self(), :get_env})

        receive do
          {:env, ^monitor, env, parent} ->
            start(pid, env, parent, traceWin, backTrace, strings)
        end
    end
  end

  defp start(pid, env, parent, traceWin, backTrace, strings) do
    case :int.attached(pid) do
      {:ok, meta} ->
        try do
          :wx.set_env(env)
          init(pid, parent, meta, traceWin, backTrace, strings)
        catch
          _, :stop ->
            exit(:stop)

          e, r ->
            :io.format('TraceWin Crashed ~p~n', [e])
            :io.format(' ~p in ~p~n', [r, __STACKTRACE__])
            exit(r)
        end

      :error ->
        :ignore
    end
  end

  def title(pid) do
    'Attach Process ' ++ :erlang.pid_to_list(pid)
  end

  defp init(pid, parent, meta, traceWin, backTrace, strings) do
    :dbg_wx_trace_win.init()
    title = title(pid)
    win = :dbg_wx_trace_win.create_win(parent, title, traceWin, menus())
    window = :dbg_wx_trace_win.get_window(win)
    :dbg_wx_winman.insert(title, window)

    state1 =
      r_state(
        win: win,
        coords: {-1, -1},
        pid: pid,
        meta: meta,
        status: {:idle, :null, :null},
        stack: {1, 1},
        strings: [:str_on]
      )

    state2 = init_options(traceWin, :int.stack_trace(), backTrace, strings, state1)
    state3 = init_contents(:int.all_breaks(), state2)
    :int.meta(meta, :trace, r_state(state3, :trace))
    gui_enable_updown(r_state(state3, :stack_trace), {1, 1})
    gui_enable_btrace(false, false)
    :dbg_wx_trace_win.display(win, :idle)
    loop(state3)
  end

  defp init_options(traceWin, stackTrace, backTrace, strings, state) do
    :lists.foreach(
      fn area ->
        :dbg_wx_trace_win.select(area, true)
      end,
      traceWin
    )

    trace = :lists.member(:"Trace Area", traceWin)
    :dbg_wx_trace_win.select(map(stackTrace), true)

    :lists.foreach(
      fn flag ->
        :dbg_wx_trace_win.select(map(flag), true)
      end,
      strings
    )

    :dbg_wx_trace_win.update_strings(strings)
    r_state(state, trace: trace, stack_trace: stackTrace, backtrace: backTrace, strings: strings)
  end

  defp init_contents(breaks, state) do
    win =
      :lists.foldl(
        fn break, win ->
          :dbg_wx_trace_win.add_break(win, :Break, break)
        end,
        r_state(state, :win),
        breaks
      )

    r_state(state, win: win)
  end

  defp loop(r_state(meta: meta, win: win) = state) do
    receive do
      guiEvent when :erlang.element(1, guiEvent) === :wx ->
        cmd =
          :wx.batch(fn ->
            :dbg_wx_trace_win.handle_event(guiEvent, win)
          end)

        state2 = gui_cmd(cmd, state)
        loop(state2)

      {:gui, cmd} ->
        state2 = gui_cmd(cmd, state)
        loop(state2)

      {:int, cmd} ->
        state2 = int_cmd(cmd, state)
        loop(state2)

      {^meta, cmd} ->
        state2 = meta_cmd(cmd, state)
        loop(state2)

      {newMeta, {:exit_at, where, reason, cur}} ->
        state2 =
          meta_cmd(
            {:exit_at, where, reason, cur},
            r_state(state, meta: newMeta)
          )

        loop(state2)

      {:dbg_ui_winman, :update_windows_menu, data} ->
        window = :dbg_wx_trace_win.get_window(win)
        :dbg_wx_winman.update_windows_menu(window, data)
        loop(state)

      {:dbg_ui_winman, :destroy} ->
        :dbg_wx_trace_win.stop(win)
        exit(:stop)
    end
  end

  defp gui_cmd(:ignore, state) do
    state
  end

  defp gui_cmd({:win, win}, state) do
    r_state(state, win: win)
  end

  defp gui_cmd(:stopped, state) do
    :dbg_wx_trace_win.stop(r_state(state, :win))
    exit(:stop)
  end

  defp gui_cmd({:coords, coords}, state) do
    r_state(state, coords: coords)
  end

  defp gui_cmd({:shortcut, key}, state) do
    case shortcut(key) do
      {:always, cmd} ->
        gui_cmd(cmd, state)

      {:if_enabled, cmd} ->
        case :dbg_wx_trace_win.is_enabled(cmd) do
          true ->
            gui_cmd(cmd, state)

          false ->
            state
        end

      false ->
        state
    end
  end

  defp gui_cmd(:Close, state) do
    gui_cmd(:stopped, state)
  end

  defp gui_cmd(:"Go To Line", state) do
    win =
      :dbg_wx_trace_win.helpwin(
        :gotoline,
        r_state(state, :win)
      )

    r_state(state, win: win)
  end

  defp gui_cmd(:Search, state) do
    win = :dbg_wx_trace_win.helpwin(:search, r_state(state, :win))
    r_state(state, win: win)
  end

  defp gui_cmd({:gotoline, line}, state) do
    win =
      :dbg_wx_trace_win.select_line(
        r_state(state, :win),
        line
      )

    r_state(state, win: win)
  end

  defp gui_cmd(:Step, state) do
    :int.meta(r_state(state, :meta), :step)
    state
  end

  defp gui_cmd(:Next, state) do
    :int.meta(r_state(state, :meta), :next)
    state
  end

  defp gui_cmd(:Continue, state) do
    :int.meta(r_state(state, :meta), :continue)
    {status, mod, line} = r_state(state, :status)

    cond do
      status === :wait_break ->
        win = :dbg_wx_trace_win.unmark_line(r_state(state, :win))
        gui_enable_functions(:wait_running)
        r_state(state, win: win, status: {:wait_running, mod, line})

      true ->
        :dbg_wx_trace_win.enable([:Stop], true)
        :dbg_wx_trace_win.enable([:Continue], false)
        state
    end
  end

  defp gui_cmd(:Finish, state) do
    :int.meta(r_state(state, :meta), :finish)
    state
  end

  defp gui_cmd(:Skip, state) do
    :int.meta(r_state(state, :meta), :skip)
    state
  end

  defp gui_cmd(:"Time Out", state) do
    :int.meta(r_state(state, :meta), :timeout)
    state
  end

  defp gui_cmd(:Stop, state) do
    :int.meta(r_state(state, :meta), :stop)
    {status, mod, line} = r_state(state, :status)

    cond do
      status === :wait_running ->
        win = :dbg_wx_trace_win.mark_line(r_state(state, :win), line, :break)
        gui_enable_functions(:wait_break)

        gui_enable_updown(
          r_state(state, :stack_trace),
          r_state(state, :stack)
        )

        gui_enable_btrace(
          r_state(state, :trace),
          r_state(state, :stack_trace)
        )

        :dbg_wx_trace_win.display(
          r_state(state, :win),
          {:wait, mod, line}
        )

        r_state(state, win: win, status: {:wait_break, mod, line})

      true ->
        :dbg_wx_trace_win.enable([:Stop], false)
        :dbg_wx_trace_win.enable([:Continue], true)
        state
    end
  end

  defp gui_cmd(:Where, state) do
    {_Cur, max} = r_state(state, :stack)
    stack = {max, max}
    {_Status, mod, line} = r_state(state, :status)

    win =
      gui_show_module(
        r_state(state, :win),
        mod,
        line,
        r_state(state, :cm),
        r_state(state, :pid),
        :break
      )

    gui_update_bindings(r_state(state, :win), r_state(state, :meta))
    gui_enable_updown(r_state(state, :stack_trace), stack)

    :dbg_wx_trace_win.display(
      r_state(state, :win),
      r_state(state, :status)
    )

    r_state(state, win: win, cm: mod, stack: stack)
  end

  defp gui_cmd(:Kill, state) do
    :erlang.exit(r_state(state, :pid), :kill)
    state
  end

  defp gui_cmd(:Messages, state) do
    _ =
      case :int.meta(r_state(state, :meta), :messages) do
        [] ->
          :dbg_wx_trace_win.eval_output(r_state(state, :win), '< No Messages!\n', :bold)

        messages ->
          :dbg_wx_trace_win.eval_output(
            r_state(state, :win),
            '< --- Current Messages ---\n',
            :bold
          )

          :lists.foldl(
            fn msg, n ->
              str1 = :io_lib.format(' ~w:', [n])
              :dbg_wx_trace_win.eval_output(r_state(state, :win), str1, :bold)
              str2 = pretty(msg, state)
              str3 = :io_lib.format(' ~ts~n', [str2])
              :dbg_wx_trace_win.eval_output(r_state(state, :win), str3, :normal)
              n + 1
            end,
            1,
            messages
          )
      end

    state
  end

  defp gui_cmd(:"Back Trace", state) do
    :dbg_wx_trace_win.trace_output(r_state(state, :win), '\nBACK TRACE\n----------\n')
    p = p(state)

    :lists.foreach(
      fn
        {le, {mod, func, args}} ->
          str =
            :io_lib.format(
              '~p > ~w:~tw~ts\n',
              [
                le,
                mod,
                func,
                format_args(
                  args,
                  p
                )
              ]
            )

          :dbg_wx_trace_win.trace_output(r_state(state, :win), str)

        {le, {fun, args}} ->
          str = :io_lib.format('~p > ~p~ts~n', [le, fun, format_args(args, p)])
          :dbg_wx_trace_win.trace_output(r_state(state, :win), str)

        _ ->
          :ignore
      end,
      :int.meta(r_state(state, :meta), :backtrace, r_state(state, :backtrace))
    )

    :dbg_wx_trace_win.trace_output(r_state(state, :win), '\n')
    state
  end

  defp gui_cmd(:Up, state) do
    {cur, max} = r_state(state, :stack)

    case :int.meta(r_state(state, :meta), :stack_frame, {:up, cur}) do
      {new, {:undefined, -1}, _Bs} ->
        stack = {new, max}
        win = :dbg_wx_trace_win.show_no_code(r_state(state, :win))
        :dbg_wx_trace_win.update_bindings(r_state(state, :win), [])
        gui_enable_updown(r_state(state, :stack_trace), stack)

        :dbg_wx_trace_win.display(
          r_state(state, :win),
          {new, :null, :null}
        )

        r_state(state, win: win, cm: :null, stack: stack)

      {new, {mod, line}, bs} ->
        stack = {new, max}

        win =
          gui_show_module(
            r_state(state, :win),
            mod,
            line,
            r_state(state, :cm),
            r_state(state, :pid),
            :where
          )

        :dbg_wx_trace_win.update_bindings(r_state(state, :win), bs)
        gui_enable_updown(r_state(state, :stack_trace), stack)

        :dbg_wx_trace_win.display(
          r_state(state, :win),
          {new, mod, line}
        )

        r_state(state, win: win, cm: mod, stack: stack)

      :top ->
        :dbg_wx_trace_win.enable([:Up], false)
        state
    end
  end

  defp gui_cmd(:Down, state) do
    {cur, max} = r_state(state, :stack)

    case :int.meta(r_state(state, :meta), :stack_frame, {:down, cur}) do
      {new, {:undefined, -1}, _Bs} ->
        stack = {new, max}
        win = :dbg_wx_trace_win.show_no_code(r_state(state, :win))
        :dbg_wx_trace_win.update_bindings(r_state(state, :win), [])
        gui_enable_updown(r_state(state, :stack_trace), stack)

        :dbg_wx_trace_win.display(
          r_state(state, :win),
          {new, :null, :null}
        )

        r_state(state, win: win, cm: :null, stack: stack)

      {new, {mod, line}, bs} ->
        stack = {new, max}

        win =
          gui_show_module(
            r_state(state, :win),
            mod,
            line,
            r_state(state, :cm),
            r_state(state, :pid),
            :where
          )

        :dbg_wx_trace_win.update_bindings(r_state(state, :win), bs)
        gui_enable_updown(r_state(state, :stack_trace), stack)

        :dbg_wx_trace_win.display(
          r_state(state, :win),
          {new, mod, line}
        )

        r_state(state, win: win, cm: mod, stack: stack)

      :bottom ->
        gui_cmd(:Where, state)
    end
  end

  defp gui_cmd(:"Line Break...", state) do
    add_break(
      r_state(state, :win),
      r_state(state, :coords),
      :line,
      r_state(state, :cm),
      :dbg_wx_trace_win.selected_line(r_state(state, :win))
    )

    state
  end

  defp gui_cmd(:"Conditional Break...", state) do
    add_break(
      r_state(state, :win),
      r_state(state, :coords),
      :conditional,
      r_state(state, :cm),
      :dbg_wx_trace_win.selected_line(r_state(state, :win))
    )

    state
  end

  defp gui_cmd(:"Function Break...", state) do
    add_break(
      r_state(state, :win),
      r_state(state, :coords),
      :function,
      r_state(state, :cm),
      :undefined
    )

    state
  end

  defp gui_cmd(:"Enable All", state) do
    breaks = :int.all_breaks()
    thisMod = r_state(state, :cm)

    :lists.foreach(
      fn
        {{mod, line}, _Options}
        when mod === thisMod ->
          :int.enable_break(mod, line)

        _Break ->
          :ignore
      end,
      breaks
    )

    state
  end

  defp gui_cmd(:"Disable All", state) do
    breaks = :int.all_breaks()
    thisMod = r_state(state, :cm)

    :lists.foreach(
      fn
        {{mod, line}, _Options}
        when mod === thisMod ->
          :int.disable_break(mod, line)

        _Break ->
          :ignore
      end,
      breaks
    )

    state
  end

  defp gui_cmd(:"Delete All", state) do
    :int.no_break(r_state(state, :cm))
    state
  end

  defp gui_cmd({:break, {mod, line}, what}, state) do
    case what do
      :add ->
        :int.break(mod, line)

      :delete ->
        :int.delete_break(mod, line)

      {:status, :inactive} ->
        :int.disable_break(mod, line)

      {:status, :active} ->
        :int.enable_break(mod, line)

      {:trigger, action} ->
        :int.action_at_break(mod, line, action)
    end

    state
  end

  defp gui_cmd({:"Trace Window", traceWin}, state) do
    trace = :lists.member(:"Trace Area", traceWin)
    :int.meta(r_state(state, :meta), :trace, trace)

    win =
      :dbg_wx_trace_win.configure(
        r_state(state, :win),
        traceWin
      )

    {status, _, _} = r_state(state, :status)

    cond do
      status === :break or status === :wait_break ->
        gui_enable_btrace(trace, r_state(state, :stack_trace))

      true ->
        :ignore
    end

    r_state(state, win: win, trace: trace)
  end

  defp gui_cmd({:"Stack Trace", [name]}, state) do
    :int.meta(r_state(state, :meta), :stack_trace, map(name))
    {status, _, _} = r_state(state, :status)

    cond do
      status === :break or status === :wait_break ->
        gui_enable_btrace(r_state(state, :trace), map(name))

      true ->
        :ignore
    end

    state
  end

  defp gui_cmd(:"Back Trace Size...", state) do
    win = :dbg_wx_trace_win.get_window(r_state(state, :win))
    val = :erlang.integer_to_list(r_state(state, :backtrace))

    case :dbg_wx_win.entry(win, 'Backtrace', :"Backtrace:", {:integer, val}) do
      :cancel ->
        state

      {_, backTrace} ->
        r_state(state, backtrace: backTrace)
    end
  end

  defp gui_cmd({:Strings, flags}, state) do
    names =
      for flag <- flags do
        map(flag)
      end

    :dbg_wx_trace_win.update_strings(names)
    r_state(state, strings: names)
  end

  defp gui_cmd(:Debugger, state) do
    window = :dbg_wx_trace_win.get_window(r_state(state, :win))
    helpFile = :filename.join([:code.lib_dir(:debugger), 'doc', 'html', 'part_frame.html'])
    :dbg_wx_win.open_help(window, helpFile)
    state
  end

  defp gui_cmd({:user_command, cmd}, state) do
    {status, _Mod, _Line} = r_state(state, :status)

    cond do
      status === :break or status === :wait_break or
          status === :wait_running ->
        cm = r_state(state, :cm)

        arg =
          case r_state(state, :stack) do
            {cur, max} when cur < max ->
              {cm, cmd, cur}

            _Stack ->
              {cm, cmd}
          end

        :int.meta(r_state(state, :meta), :eval, arg)

      true ->
        str = 'Commands not allowed'
        :dbg_wx_trace_win.eval_output(r_state(state, :win), [?<, str, 10], :normal)
    end

    state
  end

  defp gui_cmd({:edit, {var, value}}, state) do
    window = :dbg_wx_trace_win.get_window(r_state(state, :win))

    val =
      case r_state(state, :strings) do
        [] ->
          :dbg_wx_win.to_string('~0ltp', [value])

        [:str_on] ->
          :dbg_wx_win.to_string('~0tp', [value])
      end

    case :dbg_wx_win.entry(window, 'Edit variable', var, {:term, val}) do
      :cancel ->
        state

      {^var, term} ->
        cmd =
          :erlang.atom_to_list(var) ++
            '= ' ++
            :io_lib.format(
              '~w',
              [term]
            )

        gui_cmd({:user_command, :lists.flatten(cmd)}, state)
    end
  end

  defp add_break(wI, coords, type, :undefined, _Line) do
    win = :dbg_wx_trace_win.get_window(wI)
    :dbg_wx_break.start(win, coords, type)
  end

  defp add_break(wI, coords, type, mod, :undefined) do
    win = :dbg_wx_trace_win.get_window(wI)
    :dbg_wx_break.start(win, coords, type, mod)
  end

  defp add_break(wI, coords, type, mod, line) do
    win = :dbg_wx_trace_win.get_window(wI)
    :dbg_wx_break.start(win, coords, type, mod, line)
  end

  defp format_args(as, p) when is_list(as) do
    [?(, format_args1(as, p), ?)]
  end

  defp format_args(a, p) do
    [?/, :io_lib.format(p, [a])]
  end

  defp format_args1([a], p) do
    [:io_lib.format(p, [a])]
  end

  defp format_args1([a | as], p) do
    [:io_lib.format(p, [a]), ?, | format_args1(as, p)]
  end

  defp format_args1([], _) do
    []
  end

  defp int_cmd({:interpret, mod}, state) do
    cond do
      mod === r_state(state, :cm) ->
        r_state(state, cm_obsolete: true)

      true ->
        state
    end
  end

  defp int_cmd({:no_interpret, mod}, state) do
    cond do
      mod === r_state(state, :cm) ->
        r_state(state, cm_obsolete: true)

      true ->
        win = :dbg_wx_trace_win.remove_code(r_state(state, :win), mod)
        r_state(state, win: win)
    end
  end

  defp int_cmd({:new_break, break}, state) do
    win = :dbg_wx_trace_win.add_break(r_state(state, :win), :Break, break)
    r_state(state, win: win)
  end

  defp int_cmd({:delete_break, point}, state) do
    win =
      :dbg_wx_trace_win.delete_break(
        r_state(state, :win),
        point
      )

    r_state(state, win: win)
  end

  defp int_cmd({:break_options, break}, state) do
    win =
      :dbg_wx_trace_win.update_break(
        r_state(state, :win),
        break
      )

    r_state(state, win: win)
  end

  defp int_cmd(:no_break, state) do
    win = :dbg_wx_trace_win.clear_breaks(r_state(state, :win))
    r_state(state, win: win)
  end

  defp int_cmd({:no_break, mod}, state) do
    win =
      :dbg_wx_trace_win.clear_breaks(
        r_state(state, :win),
        mod
      )

    r_state(state, win: win)
  end

  defp meta_cmd({:attached, mod, line, _Trace}, state) do
    win =
      cond do
        mod != :undefined ->
          gui_enable_functions(:init)

          gui_show_module(
            r_state(state, :win),
            mod,
            line,
            r_state(state, :cm),
            r_state(state, :pid),
            :break
          )

        true ->
          r_state(state, :win)
      end

    r_state(state, win: win, status: {:init, mod, line}, cm: mod)
  end

  defp meta_cmd({:re_entry, :dbg_ieval, :eval_fun}, state) do
    state
  end

  defp meta_cmd({:re_entry, mod, _Func}, state) do
    obs = r_state(state, :cm_obsolete)

    case r_state(state, :cm) do
      ^mod when obs === true ->
        win = gui_load_module(r_state(state, :win), mod, r_state(state, :pid))
        r_state(state, win: win, cm_obsolete: false)

      ^mod ->
        state

      cm ->
        win = gui_show_module(r_state(state, :win), mod, 0, cm, r_state(state, :pid), :break)
        r_state(state, win: win, cm: mod)
    end
  end

  defp meta_cmd({:exit_at, :null, reason, cur}, state) do
    stack = {cur, cur}
    gui_enable_functions(:exit)
    gui_enable_updown(false, stack)

    :dbg_wx_trace_win.display(
      r_state(state, :win),
      {:exit, :null, reason}
    )

    r_state(state, status: {:exit, :null, reason}, stack: stack)
  end

  defp meta_cmd({:exit_at, {mod, line}, reason, cur}, state) do
    stack = {cur + 1, cur + 1}

    win =
      gui_show_module(
        r_state(state, :win),
        mod,
        line,
        r_state(state, :cm),
        r_state(state, :pid),
        :break
      )

    gui_enable_functions(:exit)
    gui_enable_updown(r_state(state, :stack_trace), stack)

    gui_enable_btrace(
      r_state(state, :trace),
      r_state(state, :stack_trace)
    )

    gui_update_bindings(r_state(state, :win), r_state(state, :meta))

    :dbg_wx_trace_win.display(
      r_state(state, :win),
      {:exit, {mod, line}, reason}
    )

    r_state(state, win: win, cm: mod, status: {:exit, {mod, line}, reason}, stack: stack)
  end

  defp meta_cmd({:break_at, mod, line, cur}, state) do
    stack = {cur, cur}

    win =
      gui_show_module(
        r_state(state, :win),
        mod,
        line,
        r_state(state, :cm),
        r_state(state, :pid),
        :break
      )

    gui_enable_functions(:break)
    gui_enable_updown(r_state(state, :stack_trace), stack)

    gui_enable_btrace(
      r_state(state, :trace),
      r_state(state, :stack_trace)
    )

    gui_update_bindings(r_state(state, :win), r_state(state, :meta))

    :dbg_wx_trace_win.display(
      r_state(state, :win),
      {:break, mod, line}
    )

    r_state(state, win: win, cm: mod, status: {:break, mod, line}, stack: stack)
  end

  defp meta_cmd({:func_at, mod, line, cur}, state) do
    stack = {cur, cur}

    win =
      gui_show_module(
        r_state(state, :win),
        mod,
        line,
        r_state(state, :cm),
        r_state(state, :pid),
        :where
      )

    gui_enable_functions(:idle)
    :dbg_wx_trace_win.display(r_state(state, :win), :idle)
    r_state(state, win: win, cm: mod, status: {:idle, mod, line}, stack: stack)
  end

  defp meta_cmd(
         {:wait_at, mod, line, cur},
         r_state(status: {status, _, _}, win: win) = state
       )
       when status !== :init and status !== :break do
    stack = {cur, cur}
    gui_enable_functions(:wait_running)
    :dbg_wx_trace_win.display(win, {:wait, mod, line})

    r_state(state,
      status: {:wait_running, mod, line},
      stack: stack
    )
  end

  defp meta_cmd({:wait_at, mod, line, cur}, state) do
    stack = {cur, cur}

    win =
      gui_show_module(
        r_state(state, :win),
        mod,
        line,
        r_state(state, :cm),
        r_state(state, :pid),
        :break
      )

    gui_enable_functions(:wait_break)
    gui_enable_updown(r_state(state, :stack_trace), stack)

    gui_enable_btrace(
      r_state(state, :trace),
      r_state(state, :stack_trace)
    )

    gui_update_bindings(r_state(state, :win), r_state(state, :meta))

    :dbg_wx_trace_win.display(
      r_state(state, :win),
      {:wait, mod, line}
    )

    r_state(state, win: win, cm: mod, status: {:wait_break, mod, line}, stack: stack)
  end

  defp meta_cmd({:wait_after_at, mod, line, sp}, state) do
    meta_cmd({:wait_at, mod, line, sp}, state)
  end

  defp meta_cmd(:running, state) do
    win = :dbg_wx_trace_win.unmark_line(r_state(state, :win))
    gui_enable_functions(:running)
    :dbg_wx_trace_win.update_bindings(r_state(state, :win), [])

    :dbg_wx_trace_win.display(
      r_state(state, :win),
      {:running, r_state(state, :cm)}
    )

    r_state(state, win: win, status: {:running, :null, :null})
  end

  defp meta_cmd(:idle, state) do
    win = :dbg_wx_trace_win.show_no_code(r_state(state, :win))
    gui_enable_functions(:idle)
    :dbg_wx_trace_win.update_bindings(r_state(state, :win), [])
    :dbg_wx_trace_win.display(r_state(state, :win), :idle)
    r_state(state, win: win, status: {:idle, :null, :null}, cm: :undefined)
  end

  defp meta_cmd({:trace, _Bool}, state) do
    state
  end

  defp meta_cmd({:stack_trace, flag}, state) do
    :dbg_wx_trace_win.select(map(flag), true)
    gui_enable_updown(flag, r_state(state, :stack))
    {status, _, _} = r_state(state, :status)

    cond do
      status === :break or status === :wait_break ->
        gui_enable_btrace(r_state(state, :trace), flag)

      true ->
        :ignore
    end

    r_state(state, stack_trace: flag)
  end

  defp meta_cmd({:trace_output, strFun}, state) do
    p = p(state)

    :dbg_wx_trace_win.trace_output(
      r_state(state, :win),
      strFun.(p)
    )

    state
  end

  defp meta_cmd({:eval_rsp, res}, state) do
    str = pretty(res, state)
    :dbg_wx_trace_win.eval_output(r_state(state, :win), [?<, str, 10], :normal)
    state
  end

  defp pretty(term, state) do
    strings =
      case r_state(state, :strings) do
        [:str_on] ->
          true

        [] ->
          false
      end

    :io_lib_pretty.print(
      term,
      [{:encoding, :unicode}, {:strings, strings}]
    )
  end

  defp menus() do
    [
      {:File, [{:Close, :no}]},
      {:Edit, [{:"Go To Line", 0}, {:Search, 1}]},
      {:Process,
       [
         {:Step, 0},
         {:Next, 0},
         {:Continue, 0},
         {:Finish, 0},
         {:Skip, :no},
         {:"Time Out", :no},
         {:Stop, :no},
         :separator,
         {:Kill, :no},
         :separator,
         {:Messages, 0},
         {:"Back Trace", :no},
         :separator,
         {:Where, 0},
         {:Up, :no},
         {:Down, :no}
       ]},
      {:Break,
       [
         {:"Line Break...", 5},
         {:"Conditional Break...", :no},
         {:"Function Break...", :no},
         :separator,
         {:"Enable All", :no},
         {:"Disable All", :no},
         {:"Delete All", 0},
         :separator
       ]},
      {:Options,
       [
         {:"Trace Window", :no, :cascade,
          [
            {:"Search Area", :no, :check},
            {:"Button Area", :no, :check},
            {:"Evaluator Area", :no, :check},
            {:"Bindings Area", :no, :check},
            {:"Trace Area", :no, :check}
          ]},
         {:"Stack Trace", :no, :cascade,
          [
            {:"Stack On, Tail", :no, :radio},
            {:"Stack On, No Tail", :no, :radio},
            {:"Stack Off", :no, :radio}
          ]},
         {:Strings, :no, :cascade, [{:"Use range of +pc flag", :no, :check}]},
         {:"Back Trace Size...", :no}
       ]},
      {:Windows, []},
      {:Help, [{:Debugger, :no}]}
    ]
  end

  defp enable(:init) do
    []
  end

  defp enable(:idle) do
    [:Stop, :Kill]
  end

  defp enable(:break) do
    [:Step, :Next, :Continue, :Finish, :Skip, :Kill, :Messages]
  end

  defp enable(:exit) do
    []
  end

  defp enable(:wait_break) do
    [:Continue, :"Time Out", :Kill]
  end

  defp enable(:wait_running) do
    [:Stop, :Kill]
  end

  defp enable(:running) do
    [:Stop, :Kill]
  end

  defp all_buttons() do
    [
      :Step,
      :Next,
      :Continue,
      :Finish,
      :Skip,
      :"Time Out",
      :Stop,
      :Kill,
      :Messages,
      :"Back Trace",
      :Where,
      :Up,
      :Down
    ]
  end

  defp shortcut(:e) do
    {:if_enabled, :Search}
  end

  defp shortcut(:g) do
    {:if_enabled, :"Go To Line"}
  end

  defp shortcut(:s) do
    {:if_enabled, :Step}
  end

  defp shortcut(:n) do
    {:if_enabled, :Next}
  end

  defp shortcut(:c) do
    {:if_enabled, :Continue}
  end

  defp shortcut(:f) do
    {:if_enabled, :Finish}
  end

  defp shortcut(:m) do
    {:if_enabled, :Messages}
  end

  defp shortcut(:w) do
    {:if_enabled, :Where}
  end

  defp shortcut(:b) do
    {:always, :"Line Break..."}
  end

  defp shortcut(:d) do
    {:always, :"Delete All"}
  end

  defp shortcut(_) do
    false
  end

  defp map(:"Stack On, Tail") do
    :all
  end

  defp map(:"Stack On, No Tail") do
    :no_tail
  end

  defp map(:"Stack Off") do
    false
  end

  defp map(:all) do
    :"Stack On, Tail"
  end

  defp map(true) do
    :"Stack On, Tail"
  end

  defp map(:no_tail) do
    :"Stack On, No Tail"
  end

  defp map(false) do
    :"Stack Off"
  end

  defp map(:"Use range of +pc flag") do
    :str_on
  end

  defp map(:str_on) do
    :"Use range of +pc flag"
  end

  defp p(r_state(strings: [:str_on])) do
    '~tp'
  end

  defp p(r_state(strings: [])) do
    '~ltp'
  end

  defp gui_show_module(win, {mod, line}, _Reason, cm, pid, how) do
    gui_show_module(win, mod, line, cm, pid, how)
  end

  defp gui_show_module(win, mod, line, mod, _Pid, how) do
    :dbg_wx_trace_win.mark_line(win, line, how)
  end

  defp gui_show_module(win, mod, line, _Cm, pid, how) do
    win2 =
      case :dbg_wx_trace_win.is_shown(win, mod) do
        false ->
          gui_load_module(win, mod, pid)
      end

    :dbg_wx_trace_win.mark_line(win2, line, how)
  end

  defp gui_load_module(win, mod, _Pid) do
    :dbg_wx_trace_win.display(win, {:text, 'Loading module...'})

    case :dbg_iserver.call({:raw_contents, mod, :any}) do
      {:ok, contents} ->
        win2 = :dbg_wx_trace_win.show_code(win, mod, contents)
        :dbg_wx_trace_win.display(win, {:text, ''})
        win2

      :not_found ->
        :dbg_wx_trace_win.show_no_code(win)
    end
  end

  defp gui_update_bindings(win, meta) do
    bs = :int.meta(meta, :bindings, :nostack)
    :dbg_wx_trace_win.update_bindings(win, bs)
  end

  defp gui_enable_functions(status) do
    enable = enable(status)
    disable = all_buttons() -- enable
    :dbg_wx_trace_win.enable(disable, false)
    :dbg_wx_trace_win.enable(enable, true)
  end

  defp gui_enable_updown(flag, stack) do
    {enable, disable} =
      cond do
        flag === false ->
          {[], [:Up, :Down]}

        true ->
          case stack do
            {1, 1} ->
              {[], [:Up, :Down]}

            {2, 2} ->
              {[], [:Up, :Down]}

            {max, max} ->
              {[:Up], [:Down]}

            {2, _Max} ->
              {[:Down], [:Up]}

            {_Cur, _Max} ->
              {[:Up, :Down], []}
          end
      end

    :dbg_wx_trace_win.enable(enable, true)
    :dbg_wx_trace_win.enable(disable, false)

    cond do
      enable === [] ->
        :dbg_wx_trace_win.enable([:Where], false)

      true ->
        :dbg_wx_trace_win.enable([:Where], true)
    end
  end

  defp gui_enable_btrace(trace, stackTrace) do
    bool =
      cond do
        trace === false ->
          false

        stackTrace === false ->
          false

        true ->
          true
      end

    :dbg_wx_trace_win.enable([:"Back Trace"], bool)
  end
end
