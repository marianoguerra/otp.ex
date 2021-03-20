defmodule :m_gen_event do
  use Bitwise
  import Kernel, except: [send: 2]
  require Record

  Record.defrecord(:r_handler, :handler,
    module: :undefined,
    id: false,
    state: :undefined,
    supervised: false
  )

  def start() do
    :gen.start(:gen_event, :nolink, :"no callback module", [], [])
  end

  def start(name) when is_tuple(name) do
    :gen.start(:gen_event, :nolink, name, :"no callback module", [], [])
  end

  def start(options) when is_list(options) do
    :gen.start(:gen_event, :nolink, :"no callback module", [], options)
  end

  def start(name, options) do
    :gen.start(:gen_event, :nolink, name, :"no callback module", [], options)
  end

  def start_link() do
    :gen.start(:gen_event, :link, :"no callback module", [], [])
  end

  def start_link(name) when is_tuple(name) do
    :gen.start(:gen_event, :link, name, :"no callback module", [], [])
  end

  def start_link(options) when is_list(options) do
    :gen.start(:gen_event, :link, :"no callback module", [], options)
  end

  def start_link(name, options) do
    :gen.start(:gen_event, :link, name, :"no callback module", [], options)
  end

  def start_monitor() do
    :gen.start(:gen_event, :monitor, :"no callback module", [], [])
  end

  def start_monitor(name) when is_tuple(name) do
    :gen.start(:gen_event, :monitor, name, :"no callback module", [], [])
  end

  def start_monitor(options) when is_list(options) do
    :gen.start(:gen_event, :monitor, :"no callback module", [], options)
  end

  def start_monitor(name, options) do
    :gen.start(:gen_event, :monitor, name, :"no callback module", [], options)
  end

  def init_it(starter, :self, name, mod, args, options) do
    init_it(starter, self(), name, mod, args, options)
  end

  def init_it(starter, parent, name0, _, _, options) do
    :erlang.process_flag(:trap_exit, true)
    name = :gen.name(name0)
    debug = :gen.debug_options(name, options)
    hibernateAfterTimeout = :gen.hibernate_after(options)
    :proc_lib.init_ack(starter, {:ok, self()})
    loop(parent, name, [], hibernateAfterTimeout, debug, false)
  end

  def add_handler(m, handler, args) do
    rpc(m, {:add_handler, handler, args})
  end

  def add_sup_handler(m, handler, args) do
    rpc(m, {:add_sup_handler, handler, args, self()})
  end

  def notify(m, event) do
    send(m, {:notify, event})
  end

  def sync_notify(m, event) do
    rpc(m, {:sync_notify, event})
  end

  def call(m, handler, query) do
    call1(m, handler, query)
  end

  def call(m, handler, query, timeout) do
    call1(m, handler, query, timeout)
  end

  def send_request(m, handler, query) do
    :gen.send_request(m, self(), {:call, handler, query})
  end

  def wait_response(requestId, timeout) do
    case :gen.wait_response(requestId, timeout) do
      {:reply, {:error, _} = err} ->
        err

      return ->
        return
    end
  end

  def check_response(msg, requestId) do
    case :gen.check_response(msg, requestId) do
      {:reply, {:error, _} = err} ->
        err

      return ->
        return
    end
  end

  def delete_handler(m, handler, args) do
    rpc(m, {:delete_handler, handler, args})
  end

  def swap_handler(m, {h1, a1}, {h2, a2}) do
    rpc(m, {:swap_handler, h1, a1, h2, a2})
  end

  def swap_sup_handler(m, {h1, a1}, {h2, a2}) do
    rpc(m, {:swap_sup_handler, h1, a1, h2, a2, self()})
  end

  def which_handlers(m) do
    rpc(m, :which_handlers)
  end

  def stop(m) do
    :gen.stop(m)
  end

  def stop(m, reason, timeout) do
    :gen.stop(m, reason, timeout)
  end

  defp rpc(m, cmd) do
    {:ok, reply} = :gen.call(m, self(), cmd, :infinity)
    reply
  end

  defp call1(m, handler, query) do
    cmd = {:call, handler, query}

    try do
      :gen.call(m, self(), cmd)
    catch
      :exit, reason ->
        exit({reason, {:gen_event, :call, [m, handler, query]}})
    else
      {:ok, res} ->
        res
    end
  end

  defp call1(m, handler, query, timeout) do
    cmd = {:call, handler, query}

    try do
      :gen.call(m, self(), cmd, timeout)
    catch
      :exit, reason ->
        exit({reason, {:gen_event, :call, [m, handler, query, timeout]}})
    else
      {:ok, res} ->
        res
    end
  end

  defp send({:global, name}, cmd) do
    try do
      :global.send(name, cmd)
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end

    :ok
  end

  defp send({:via, mod, name}, cmd) do
    try do
      mod.send(name, cmd)
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end

    :ok
  end

  defp send(m, cmd) do
    send(m, cmd)
    :ok
  end

  defp loop(parent, serverName, mSL, hibernateAfterTimeout, debug, true) do
    :proc_lib.hibernate(:gen_event, :wake_hib, [
      parent,
      serverName,
      mSL,
      hibernateAfterTimeout,
      debug
    ])
  end

  defp loop(parent, serverName, mSL, hibernateAfterTimeout, debug, _) do
    fetch_msg(parent, serverName, mSL, hibernateAfterTimeout, debug, false)
  end

  def wake_hib(parent, serverName, mSL, hibernateAfterTimeout, debug) do
    fetch_msg(parent, serverName, mSL, hibernateAfterTimeout, debug, true)
  end

  defp fetch_msg(parent, serverName, mSL, hibernateAfterTimeout, debug, hib) do
    receive do
      {:system, from, req} ->
        :sys.handle_system_msg(
          req,
          from,
          parent,
          :gen_event,
          debug,
          [serverName, mSL, hibernateAfterTimeout, hib],
          hib
        )

      {:EXIT, ^parent, reason} ->
        terminate_server(reason, parent, mSL, serverName)

      msg when debug === [] ->
        handle_msg(msg, parent, serverName, mSL, hibernateAfterTimeout, [])

      msg ->
        debug1 = :sys.handle_debug(debug, &print_event/3, serverName, {:in, msg})
        handle_msg(msg, parent, serverName, mSL, hibernateAfterTimeout, debug1)
    after
      hibernateAfterTimeout ->
        loop(parent, serverName, mSL, hibernateAfterTimeout, debug, true)
    end
  end

  defp handle_msg(msg, parent, serverName, mSL, hibernateAfterTimeout, debug) do
    case msg do
      {:notify, event} ->
        {hib, mSL1} = server_notify(event, :handle_event, mSL, serverName)
        loop(parent, serverName, mSL1, hibernateAfterTimeout, debug, hib)

      {_From, tag, {:sync_notify, event}} ->
        {hib, mSL1} = server_notify(event, :handle_event, mSL, serverName)
        reply(tag, :ok)
        loop(parent, serverName, mSL1, hibernateAfterTimeout, debug, hib)

      {:EXIT, from, reason} ->
        mSL1 = handle_exit(from, reason, mSL, serverName)
        loop(parent, serverName, mSL1, hibernateAfterTimeout, debug, false)

      {_From, tag, {:call, handler, query}} ->
        {hib, reply, mSL1} = server_call(handler, query, mSL, serverName)
        reply(tag, reply)
        loop(parent, serverName, mSL1, hibernateAfterTimeout, debug, hib)

      {_From, tag, {:add_handler, handler, args}} ->
        {hib, reply, mSL1} = server_add_handler(handler, args, mSL)
        reply(tag, reply)
        loop(parent, serverName, mSL1, hibernateAfterTimeout, debug, hib)

      {_From, tag, {:add_sup_handler, handler, args, supP}} ->
        {hib, reply, mSL1} = server_add_sup_handler(handler, args, mSL, supP)
        reply(tag, reply)
        loop(parent, serverName, mSL1, hibernateAfterTimeout, debug, hib)

      {_From, tag, {:delete_handler, handler, args}} ->
        {reply, mSL1} = server_delete_handler(handler, args, mSL, serverName)
        reply(tag, reply)
        loop(parent, serverName, mSL1, hibernateAfterTimeout, debug, false)

      {_From, tag, {:swap_handler, handler1, args1, handler2, args2}} ->
        {hib, reply, mSL1} =
          server_swap_handler(handler1, args1, handler2, args2, mSL, serverName)

        reply(tag, reply)
        loop(parent, serverName, mSL1, hibernateAfterTimeout, debug, hib)

      {_From, tag, {:swap_sup_handler, handler1, args1, handler2, args2, sup}} ->
        {hib, reply, mSL1} =
          server_swap_handler(handler1, args1, handler2, args2, mSL, sup, serverName)

        reply(tag, reply)
        loop(parent, serverName, mSL1, hibernateAfterTimeout, debug, hib)

      {_From, tag, :stop} ->
        try do
          terminate_server(:normal, parent, mSL, serverName)
        catch
          :error, e -> {:EXIT, {e, __STACKTRACE__}}
          :exit, e -> {:EXIT, e}
          e -> e
        end

        reply(tag, :ok)

      {_From, tag, :which_handlers} ->
        reply(tag, the_handlers(mSL))
        loop(parent, serverName, mSL, hibernateAfterTimeout, debug, false)

      {_From, tag, :get_modules} ->
        reply(tag, get_modules(mSL))
        loop(parent, serverName, mSL, hibernateAfterTimeout, debug, false)

      other ->
        {hib, mSL1} = server_notify(other, :handle_info, mSL, serverName)
        loop(parent, serverName, mSL1, hibernateAfterTimeout, debug, hib)
    end
  end

  defp terminate_server(reason, parent, mSL, serverName) do
    stop_handlers(mSL, serverName)
    do_unlink(parent, mSL)
    exit(reason)
  end

  defp reply({from, ref}, msg) do
    send(from, {ref, msg})
    :ok
  end

  defp do_unlink(parent, mSL) do
    :lists.foreach(
      fn
        handler
        when r_handler(handler, :supervised) === parent ->
          true

        handler when is_pid(r_handler(handler, :supervised)) ->
          :erlang.unlink(r_handler(handler, :supervised))
          true

        _ ->
          true
      end,
      mSL
    )
  end

  defp handle_exit(from, reason, mSL, sName) do
    mSL1 = terminate_supervised(from, reason, mSL, sName)
    {_, mSL2} = server_notify({:EXIT, from, reason}, :handle_info, mSL1, sName)
    mSL2
  end

  defp terminate_supervised(pid, reason, mSL, sName) do
    f = fn
      ha when r_handler(ha, :supervised) === pid ->
        do_terminate(
          r_handler(ha, :module),
          ha,
          {:stop, reason},
          r_handler(ha, :state),
          {:parent_terminated, {pid, reason}},
          sName,
          :shutdown
        )

        false

      _ ->
        true
    end

    :lists.filter(f, mSL)
  end

  def system_continue(parent, debug, [serverName, mSL, hibernateAfterTimeout, hib]) do
    loop(parent, serverName, mSL, hibernateAfterTimeout, debug, hib)
  end

  def system_terminate(reason, parent, _Debug, [serverName, mSL, _HibernateAfterTimeout, _Hib]) do
    terminate_server(reason, parent, mSL, serverName)
  end

  def system_code_change([serverName, mSL, hibernateAfterTimeout, hib], module, oldVsn, extra) do
    mSL1 =
      :lists.zf(
        fn
          h when r_handler(h, :module) === module ->
            {:ok, newState} =
              module.code_change(
                oldVsn,
                r_handler(h, :state),
                extra
              )

            {true, r_handler(h, state: newState)}

          _ ->
            true
        end,
        mSL
      )

    {:ok, [serverName, mSL1, hibernateAfterTimeout, hib]}
  end

  def system_get_state([_ServerName, mSL, _HibernateAfterTimeout, _Hib]) do
    {:ok,
     for r_handler(module: mod, id: id, state: state) <- mSL do
       {mod, id, state}
     end}
  end

  def system_replace_state(
        stateFun,
        [serverName, mSL, hibernateAfterTimeout, hib]
      ) do
    {nMSL, nStates} =
      :lists.unzip(
        for r_handler(
              module: mod,
              id: id,
              state: state
            ) = hS <- mSL do
          cur = {mod, id, state}

          try do
            nState = {^mod, ^id, nS} = stateFun.(cur)
            {r_handler(hS, state: nS), nState}
          catch
            _, _ ->
              {hS, cur}
          end
        end
      )

    {:ok, nStates, [serverName, nMSL, hibernateAfterTimeout, hib]}
  end

  defp print_event(dev, {:in, msg}, name) do
    case msg do
      {:notify, event} ->
        :io.format(dev, '*DBG* ~tp got event ~tp~n', [name, event])

      {_, _, {:call, handler, query}} ->
        :io.format(dev, '*DBG* ~tp(~tp) got call ~tp~n', [name, handler, query])

      _ ->
        :io.format(dev, '*DBG* ~tp got ~tp~n', [name, msg])
    end
  end

  defp print_event(dev, dbg, name) do
    :io.format(dev, '*DBG* ~tp : ~tp~n', [name, dbg])
  end

  defp server_add_handler({mod, id}, args, mSL) do
    handler = r_handler(module: mod, id: id)
    server_add_handler(mod, handler, args, mSL)
  end

  defp server_add_handler(mod, args, mSL) do
    handler = r_handler(module: mod)
    server_add_handler(mod, handler, args, mSL)
  end

  defp server_add_handler(mod, handler, args, mSL) do
    case (try do
            mod.init(args)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, state} ->
        {false, :ok, [r_handler(handler, state: state) | mSL]}

      {:ok, state, :hibernate} ->
        {true, :ok, [r_handler(handler, state: state) | mSL]}

      other ->
        {false, other, mSL}
    end
  end

  defp server_add_sup_handler({mod, id}, args, mSL, parent) do
    :erlang.link(parent)
    handler = r_handler(module: mod, id: id, supervised: parent)
    server_add_handler(mod, handler, args, mSL)
  end

  defp server_add_sup_handler(mod, args, mSL, parent) do
    :erlang.link(parent)
    handler = r_handler(module: mod, supervised: parent)
    server_add_handler(mod, handler, args, mSL)
  end

  defp server_delete_handler(handlerId, args, mSL, sName) do
    case split(handlerId, mSL) do
      {mod, handler, mSL1} ->
        {do_terminate(mod, handler, args, r_handler(handler, :state), :delete, sName, :normal),
         mSL1}

      :error ->
        {{:error, :module_not_found}, mSL}
    end
  end

  defp server_swap_handler(handler1, args1, handler2, args2, mSL, sName) do
    {state2, sup, mSL1} = split_and_terminate(handler1, args1, mSL, sName, handler2, false)

    case s_s_h(sup, handler2, {args2, state2}, mSL1) do
      {hib, :ok, mSL2} ->
        {hib, :ok, mSL2}

      {hib, what, mSL2} ->
        {hib, {:error, what}, mSL2}
    end
  end

  defp server_swap_handler(handler1, args1, handler2, args2, mSL, sup, sName) do
    {state2, _, mSL1} = split_and_terminate(handler1, args1, mSL, sName, handler2, sup)

    case s_s_h(sup, handler2, {args2, state2}, mSL1) do
      {hib, :ok, mSL2} ->
        {hib, :ok, mSL2}

      {hib, what, mSL2} ->
        {hib, {:error, what}, mSL2}
    end
  end

  defp s_s_h(false, handler, args, mSL) do
    server_add_handler(handler, args, mSL)
  end

  defp s_s_h(pid, handler, args, mSL) do
    server_add_sup_handler(handler, args, mSL, pid)
  end

  defp split_and_terminate(handlerId, args, mSL, sName, handler2, sup) do
    case split(handlerId, mSL) do
      {mod, handler, mSL1} ->
        oldSup = r_handler(handler, :supervised)

        newSup =
          cond do
            not sup ->
              oldSup

            true ->
              sup
          end

        {do_terminate(
           mod,
           handler,
           args,
           r_handler(handler, :state),
           :swapped,
           sName,
           {:swapped, handler2, newSup}
         ), oldSup, mSL1}

      :error ->
        {:error, false, mSL}
    end
  end

  defp server_notify(event, func, [handler | t], sName) do
    case server_update(handler, func, event, sName) do
      {:ok, handler1} ->
        {hib, newHandlers} = server_notify(event, func, t, sName)
        {hib, [handler1 | newHandlers]}

      {:hibernate, handler1} ->
        {_Hib, newHandlers} = server_notify(event, func, t, sName)
        {true, [handler1 | newHandlers]}

      :no ->
        server_notify(event, func, t, sName)
    end
  end

  defp server_notify(_, _, [], _) do
    {false, []}
  end

  defp server_update(handler1, func, event, sName) do
    mod1 = r_handler(handler1, :module)
    state = r_handler(handler1, :state)

    case (try do
            apply(mod1, func, [event, state])
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, state1} ->
        {:ok, r_handler(handler1, state: state1)}

      {:ok, state1, :hibernate} ->
        {:hibernate, r_handler(handler1, state: state1)}

      {:swap_handler, args1, state1, handler2, args2} ->
        do_swap(mod1, handler1, args1, state1, handler2, args2, sName)

      :remove_handler ->
        do_terminate(mod1, handler1, :remove_handler, state, :remove, sName, :normal)
        :no

      {:EXIT, {:undef, [{^mod1, :handle_info, [_, _], _} | _]}} ->
        case :logger.allow(:warning, :gen_event) do
          true ->
            :erlang.apply(:logger, :macro_log, [
              %{
                mfa: {:gen_event, :server_update, 4},
                line: 632,
                file: 'otp/lib/stdlib/src/gen_event.erl'
              },
              :warning,
              %{label: {:gen_event, :no_handle_info}, module: mod1, message: event},
              %{
                domain: [:otp],
                report_cb: &:gen_event.format_log/2,
                error_logger: %{tag: :warning_msg, report_cb: &:gen_event.format_log/1}
              }
            ])

          false ->
            :ok
        end

        {:ok, handler1}

      other ->
        do_terminate(mod1, handler1, {:error, other}, state, event, sName, :crash)
        :no
    end
  end

  defp do_swap(mod1, handler1, args1, state1, handler2, args2, sName) do
    state2 =
      do_terminate(
        mod1,
        handler1,
        args1,
        state1,
        :swapped,
        sName,
        {:swapped, handler2, r_handler(handler1, :supervised)}
      )

    {mod2, handler} = new_handler(handler2, handler1)

    case (try do
            mod2.init({args2, state2})
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, state2a} ->
        {:ok, r_handler(handler, state: state2a)}

      other ->
        report_terminate(handler, :crash, {:error, other}, sName, false)
        :no
    end
  end

  defp new_handler({mod, id}, handler1) do
    {mod, r_handler(module: mod, id: id, supervised: r_handler(handler1, :supervised))}
  end

  defp new_handler(mod, handler1) do
    {mod, r_handler(module: mod, supervised: r_handler(handler1, :supervised))}
  end

  defp split(ha, mSL) do
    split(ha, mSL, [])
  end

  defp split({mod, id}, [ha | t], l)
       when r_handler(ha, :module) === mod and r_handler(ha, :id) === id do
    {mod, ha, :lists.reverse(l, t)}
  end

  defp split(mod, [ha | t], l)
       when r_handler(ha, :module) === mod and not r_handler(ha, :id) do
    {mod, ha, :lists.reverse(l, t)}
  end

  defp split(ha, [h | t], l) do
    split(ha, t, [h | l])
  end

  defp split(_, [], _) do
    :error
  end

  defp server_call(handler, query, mSL, sName) do
    case search(handler, mSL) do
      {:ok, ha} ->
        case server_call_update(ha, query, sName) do
          {:no, reply} ->
            {false, reply, delete(handler, mSL)}

          {{:ok, ha1}, reply} ->
            {false, reply, replace(handler, mSL, ha1)}

          {{:hibernate, ha1}, reply} ->
            {true, reply, replace(handler, mSL, ha1)}
        end

      false ->
        {false, {:error, :bad_module}, mSL}
    end
  end

  defp search({mod, id}, [ha | _MSL])
       when r_handler(ha, :module) === mod and r_handler(ha, :id) === id do
    {:ok, ha}
  end

  defp search(mod, [ha | _MSL])
       when r_handler(ha, :module) === mod and not r_handler(ha, :id) do
    {:ok, ha}
  end

  defp search(handler, [_ | mSL]) do
    search(handler, mSL)
  end

  defp search(_, []) do
    false
  end

  defp delete({mod, id}, [ha | mSL])
       when r_handler(ha, :module) === mod and r_handler(ha, :id) === id do
    mSL
  end

  defp delete(mod, [ha | mSL])
       when r_handler(ha, :module) === mod and not r_handler(ha, :id) do
    mSL
  end

  defp delete(handler, [ha | mSL]) do
    [ha | delete(handler, mSL)]
  end

  defp delete(_, []) do
    []
  end

  defp replace({mod, id}, [ha | mSL], newHa)
       when r_handler(ha, :module) === mod and r_handler(ha, :id) === id do
    [newHa | mSL]
  end

  defp replace(mod, [ha | mSL], newHa)
       when r_handler(ha, :module) === mod and not r_handler(ha, :id) do
    [newHa | mSL]
  end

  defp replace(handler, [ha | mSL], newHa) do
    [ha | replace(handler, mSL, newHa)]
  end

  defp replace(_, [], newHa) do
    [newHa]
  end

  defp server_call_update(handler1, query, sName) do
    mod1 = r_handler(handler1, :module)
    state = r_handler(handler1, :state)

    case (try do
            mod1.handle_call(query, state)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, reply, state1} ->
        {{:ok, r_handler(handler1, state: state1)}, reply}

      {:ok, reply, state1, :hibernate} ->
        {{:hibernate, r_handler(handler1, state: state1)}, reply}

      {:swap_handler, reply, args1, state1, handler2, args2} ->
        {do_swap(mod1, handler1, args1, state1, handler2, args2, sName), reply}

      {:remove_handler, reply} ->
        do_terminate(mod1, handler1, :remove_handler, state, :remove, sName, :normal)
        {:no, reply}

      other ->
        do_terminate(mod1, handler1, {:error, other}, state, query, sName, :crash)
        {:no, {:error, other}}
    end
  end

  defp do_terminate(mod, handler, args, state, lastIn, sName, reason) do
    case :erlang.function_exported(mod, :terminate, 2) do
      true ->
        res =
          try do
            mod.terminate(args, state)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end

        report_terminate(handler, reason, args, state, lastIn, sName, res)
        res

      false ->
        report_terminate(handler, reason, args, state, lastIn, sName, :ok)
        :ok
    end
  end

  defp report_terminate(handler, :crash, {:error, why}, state, lastIn, sName, _) do
    report_terminate(handler, why, state, lastIn, sName)
  end

  defp report_terminate(handler, how, _, state, lastIn, sName, _) do
    report_terminate(handler, how, state, lastIn, sName)
  end

  defp report_terminate(handler, reason, state, lastIn, sName) do
    report_error(handler, reason, state, lastIn, sName)

    case r_handler(handler, :supervised) do
      false ->
        :ok

      pid ->
        send(pid, {:gen_event_EXIT, handler(handler), reason})
        :ok
    end
  end

  defp report_error(_Handler, :normal, _, _, _) do
    :ok
  end

  defp report_error(_Handler, :shutdown, _, _, _) do
    :ok
  end

  defp report_error(_Handler, {:swapped, _, _}, _, _, _) do
    :ok
  end

  defp report_error(handler, reason, state, lastIn, sName) do
    case :logger.allow(:error, :gen_event) do
      true ->
        :erlang.apply(:logger, :macro_log, [
          %{
            mfa: {:gen_event, :report_error, 5},
            line: 792,
            file: 'otp/lib/stdlib/src/gen_event.erl'
          },
          :error,
          %{
            label: {:gen_event, :terminate},
            handler: handler(handler),
            name: sName,
            last_message: lastIn,
            state: format_status(:terminate, r_handler(handler, :module), :erlang.get(), state),
            reason: reason
          },
          %{
            domain: [:otp],
            report_cb: &:gen_event.format_log/2,
            error_logger: %{tag: :error, report_cb: &:gen_event.format_log/1}
          }
        ])

      false ->
        :ok
    end
  end

  def format_log(report) do
    depth = :error_logger.get_format_depth()
    formatOpts = %{chars_limit: :unlimited, depth: depth, single_line: false, encoding: :utf8}

    format_log_multi(
      limit_report(report, depth),
      formatOpts
    )
  end

  defp limit_report(report, :unlimited) do
    report
  end

  defp limit_report(
         %{label: {:gen_event, :terminate}, last_message: lastIn, state: state, reason: reason} =
           report,
         depth
       ) do
    Map.merge(report, %{
      last_message: :io_lib.limit_term(lastIn, depth),
      state: :io_lib.limit_term(state, depth),
      reason: :io_lib.limit_term(reason, depth)
    })
  end

  defp limit_report(
         %{label: {:gen_event, :no_handle_info}, message: msg} = report,
         depth
       ) do
    Map.put(report, :message, :io_lib.limit_term(msg, depth))
  end

  def format_log(report, formatOpts0) do
    default = %{chars_limit: :unlimited, depth: :unlimited, single_line: false, encoding: :utf8}
    formatOpts = :maps.merge(default, formatOpts0)

    ioOpts =
      case formatOpts do
        %{chars_limit: :unlimited} ->
          []

        %{chars_limit: limit} ->
          [{:chars_limit, limit}]
      end

    {format, args} = format_log_single(report, formatOpts)
    :io_lib.format(format, args, ioOpts)
  end

  defp format_log_single(
         %{
           label: {:gen_event, :terminate},
           handler: handler,
           name: sName,
           last_message: lastIn,
           state: state,
           reason: reason
         },
         %{single_line: true, depth: depth} = formatOpts
       ) do
    p = p(formatOpts)
    reason1 = fix_reason(reason)

    format1 =
      :lists.append([
        'Generic event handler ',
        p,
        ' crashed. Installed: ',
        p,
        '. Last event: ',
        p,
        '. State: ',
        p,
        '. Reason: ',
        p,
        '.'
      ])

    args1 =
      case depth do
        :unlimited ->
          [handler, sName, reason1, lastIn, state]

        _ ->
          [handler, depth, sName, depth, reason1, depth, lastIn, depth, state, depth]
      end

    {format1, args1}
  end

  defp format_log_single(
         %{label: {:gen_event, :no_handle_info}, module: mod, message: msg},
         %{single_line: true, depth: depth} = formatOpts
       ) do
    p = p(formatOpts)
    format = :lists.append(['Undefined handle_info in ', p, '. Unhandled message: ', p, '.'])

    args =
      case depth do
        :unlimited ->
          [mod, msg]

        _ ->
          [mod, depth, msg, depth]
      end

    {format, args}
  end

  defp format_log_single(report, formatOpts) do
    format_log_multi(report, formatOpts)
  end

  defp format_log_multi(
         %{
           label: {:gen_event, :terminate},
           handler: handler,
           name: sName,
           last_message: lastIn,
           state: state,
           reason: reason
         },
         %{depth: depth} = formatOpts
       ) do
    reason1 = fix_reason(reason)
    p = p(formatOpts)

    format =
      :lists.append([
        '** gen_event handler ',
        p,
        ' crashed.\n',
        '** Was installed in ',
        p,
        '\n',
        '** Last event was: ',
        p,
        '\n',
        '** When handler state == ',
        p,
        '\n',
        '** Reason == ',
        p,
        '\n'
      ])

    args =
      case depth do
        :unlimited ->
          [handler, sName, lastIn, state, reason1]

        _ ->
          [handler, depth, sName, depth, lastIn, depth, state, depth, reason1, depth]
      end

    {format, args}
  end

  defp format_log_multi(
         %{label: {:gen_event, :no_handle_info}, module: mod, message: msg},
         %{depth: depth} = formatOpts
       ) do
    p = p(formatOpts)
    format = '** Undefined handle_info in ~p\n** Unhandled message: ' ++ p ++ '\n'

    args =
      case depth do
        :unlimited ->
          [mod, msg]

        _ ->
          [mod, msg, depth]
      end

    {format, args}
  end

  defp fix_reason({:EXIT, {:undef, [{m, f, a, _L} | _] = mFAs} = reason}) do
    case :code.is_loaded(m) do
      false ->
        {:"module could not be loaded", mFAs}

      _ ->
        case :erlang.function_exported(m, f, length(a)) do
          true ->
            reason

          false ->
            {:"function not exported", mFAs}
        end
    end
  end

  defp fix_reason({:EXIT, reason}) do
    reason
  end

  defp fix_reason(reason) do
    reason
  end

  defp p(%{single_line: single, depth: depth, encoding: enc}) do
    '~' ++ single(single) ++ mod(enc) ++ p(depth)
  end

  defp p(:unlimited) do
    'p'
  end

  defp p(_Depth) do
    'P'
  end

  defp single(true) do
    '0'
  end

  defp single(false) do
    ''
  end

  defp mod(:latin1) do
    ''
  end

  defp mod(_) do
    't'
  end

  defp handler(handler) when not r_handler(handler, :id) do
    r_handler(handler, :module)
  end

  defp handler(handler) do
    {r_handler(handler, :module), r_handler(handler, :id)}
  end

  defp the_handlers(mSL) do
    for handler <- mSL do
      handler(handler)
    end
  end

  defp stop_handlers([handler | t], sName) do
    mod = r_handler(handler, :module)
    do_terminate(mod, handler, :stop, r_handler(handler, :state), :stop, sName, :shutdown)
    stop_handlers(t, sName)
  end

  defp stop_handlers([], _) do
    []
  end

  defp get_modules(mSL) do
    mods =
      for handler <- mSL do
        r_handler(handler, :module)
      end

    :ordsets.to_list(:ordsets.from_list(mods))
  end

  def format_status(opt, statusData) do
    [pDict, sysState, parent, _Debug, [serverName, mSL, _HibernateAfterTimeout, _Hib]] =
      statusData

    header = :gen.format_status_header('Status for event handler', serverName)

    fmtMSL =
      for r_handler(
            module: mod,
            state: state
          ) = mS <- mSL do
        r_handler(mS, state: format_status(opt, mod, pDict, state))
      end

    [
      {:header, header},
      {:data, [{'Status', sysState}, {'Parent', parent}]},
      {:items, {'Installed handlers', fmtMSL}}
    ]
  end

  defp format_status(opt, mod, pDict, state) do
    case :erlang.function_exported(mod, :format_status, 2) do
      true ->
        args = [pDict, state]

        case (try do
                mod.format_status(opt, args)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          {:EXIT, _} ->
            state

          else__ ->
            else__
        end

      false ->
        state
    end
  end
end
