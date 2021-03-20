defmodule :m_gen_server do
  use Bitwise

  def start(mod, args, options) do
    :gen.start(:gen_server, :nolink, mod, args, options)
  end

  def start(name, mod, args, options) do
    :gen.start(:gen_server, :nolink, name, mod, args, options)
  end

  def start_link(mod, args, options) do
    :gen.start(:gen_server, :link, mod, args, options)
  end

  def start_link(name, mod, args, options) do
    :gen.start(:gen_server, :link, name, mod, args, options)
  end

  def start_monitor(mod, args, options) do
    :gen.start(:gen_server, :monitor, mod, args, options)
  end

  def start_monitor(name, mod, args, options) do
    :gen.start(:gen_server, :monitor, name, mod, args, options)
  end

  def stop(name) do
    :gen.stop(name)
  end

  def stop(name, reason, timeout) do
    :gen.stop(name, reason, timeout)
  end

  def call(name, request) do
    case (try do
            :gen.call(name, :"$gen_call", request)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, res} ->
        res

      {:EXIT, reason} ->
        exit({reason, {:gen_server, :call, [name, request]}})
    end
  end

  def call(name, request, timeout) do
    case (try do
            :gen.call(name, :"$gen_call", request, timeout)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, res} ->
        res

      {:EXIT, reason} ->
        exit({reason, {:gen_server, :call, [name, request, timeout]}})
    end
  end

  def send_request(name, request) do
    :gen.send_request(name, :"$gen_call", request)
  end

  def wait_response(requestId, timeout) do
    :gen.wait_response(requestId, timeout)
  end

  def check_response(msg, requestId) do
    :gen.check_response(msg, requestId)
  end

  def cast({:global, name}, request) do
    try do
      :global.send(name, cast_msg(request))
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end

    :ok
  end

  def cast({:via, mod, name}, request) do
    try do
      mod.send(name, cast_msg(request))
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end

    :ok
  end

  def cast({name, node} = dest, request)
      when is_atom(name) and is_atom(node) do
    do_cast(dest, request)
  end

  def cast(dest, request) when is_atom(dest) do
    do_cast(dest, request)
  end

  def cast(dest, request) when is_pid(dest) do
    do_cast(dest, request)
  end

  defp do_cast(dest, request) do
    do_send(dest, cast_msg(request))
    :ok
  end

  defp cast_msg(request) do
    {:"$gen_cast", request}
  end

  def reply({to, tag}, reply) do
    try do
      send(to, {tag, reply})
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end

    :ok
  end

  def abcast(name, request) when is_atom(name) do
    do_abcast([node() | :erlang.nodes()], name, cast_msg(request))
  end

  def abcast(nodes, name, request)
      when is_list(nodes) and
             is_atom(name) do
    do_abcast(nodes, name, cast_msg(request))
  end

  defp do_abcast([node | nodes], name, msg) when is_atom(node) do
    do_send({name, node}, msg)
    do_abcast(nodes, name, msg)
  end

  defp do_abcast([], _, _) do
    :abcast
  end

  def multi_call(name, req) when is_atom(name) do
    do_multi_call([node() | :erlang.nodes()], name, req, :infinity)
  end

  def multi_call(nodes, name, req)
      when is_list(nodes) and
             is_atom(name) do
    do_multi_call(nodes, name, req, :infinity)
  end

  def multi_call(nodes, name, req, :infinity) do
    do_multi_call(nodes, name, req, :infinity)
  end

  def multi_call(nodes, name, req, timeout)
      when is_list(nodes) and is_atom(name) and
             is_integer(timeout) and timeout >= 0 do
    do_multi_call(nodes, name, req, timeout)
  end

  def enter_loop(mod, options, state) do
    enter_loop(mod, options, state, self(), :infinity)
  end

  def enter_loop(mod, options, state, serverName = {scope, _})
      when scope == :local or scope == :global do
    enter_loop(mod, options, state, serverName, :infinity)
  end

  def enter_loop(mod, options, state, serverName = {:via, _, _}) do
    enter_loop(mod, options, state, serverName, :infinity)
  end

  def enter_loop(mod, options, state, timeout) do
    enter_loop(mod, options, state, self(), timeout)
  end

  def enter_loop(mod, options, state, serverName, timeout) do
    name = :gen.get_proc_name(serverName)
    parent = :gen.get_parent()
    debug = :gen.debug_options(name, options)
    hibernateAfterTimeout = :gen.hibernate_after(options)
    loop(parent, name, state, mod, timeout, hibernateAfterTimeout, debug)
  end

  def init_it(starter, :self, name, mod, args, options) do
    init_it(starter, self(), name, mod, args, options)
  end

  def init_it(starter, parent, name0, mod, args, options) do
    name = :gen.name(name0)
    debug = :gen.debug_options(name, options)
    hibernateAfterTimeout = :gen.hibernate_after(options)

    case init_it(mod, args) do
      {:ok, {:ok, state}} ->
        :proc_lib.init_ack(starter, {:ok, self()})
        loop(parent, name, state, mod, :infinity, hibernateAfterTimeout, debug)

      {:ok, {:ok, state, timeout}} ->
        :proc_lib.init_ack(starter, {:ok, self()})
        loop(parent, name, state, mod, timeout, hibernateAfterTimeout, debug)

      {:ok, {:stop, reason}} ->
        :gen.unregister_name(name0)
        :proc_lib.init_ack(starter, {:error, reason})
        exit(reason)

      {:ok, :ignore} ->
        :gen.unregister_name(name0)
        :proc_lib.init_ack(starter, :ignore)
        exit(:normal)

      {:ok, else__} ->
        error = {:bad_return_value, else__}
        :proc_lib.init_ack(starter, {:error, error})
        exit(error)

      {:EXIT, class, reason, stacktrace} ->
        :gen.unregister_name(name0)

        :proc_lib.init_ack(
          starter,
          {:error, terminate_reason(class, reason, stacktrace)}
        )

        :erlang.raise(class, reason, stacktrace)
    end
  end

  defp init_it(mod, args) do
    try do
      {:ok, mod.init(args)}
    catch
      r ->
        {:ok, r}

      class, r ->
        {:EXIT, class, r, __STACKTRACE__}
    end
  end

  defp loop(parent, name, state, mod, {:continue, continue} = msg, hibernateAfterTimeout, debug) do
    reply = try_dispatch(mod, :handle_continue, continue, state)

    case debug do
      [] ->
        handle_common_reply(
          reply,
          parent,
          name,
          :undefined,
          msg,
          mod,
          hibernateAfterTimeout,
          state
        )

      _ ->
        debug1 = :sys.handle_debug(debug, &print_event/3, name, msg)

        handle_common_reply(
          reply,
          parent,
          name,
          :undefined,
          msg,
          mod,
          hibernateAfterTimeout,
          state,
          debug1
        )
    end
  end

  defp loop(parent, name, state, mod, :hibernate, hibernateAfterTimeout, debug) do
    :proc_lib.hibernate(:gen_server, :wake_hib, [
      parent,
      name,
      state,
      mod,
      hibernateAfterTimeout,
      debug
    ])
  end

  defp loop(parent, name, state, mod, :infinity, hibernateAfterTimeout, debug) do
    receive do
      msg ->
        decode_msg(msg, parent, name, state, mod, :infinity, hibernateAfterTimeout, debug, false)
    after
      hibernateAfterTimeout ->
        loop(parent, name, state, mod, :hibernate, hibernateAfterTimeout, debug)
    end
  end

  defp loop(parent, name, state, mod, time, hibernateAfterTimeout, debug) do
    msg =
      receive do
        input ->
          input
      after
        time ->
          :timeout
      end

    decode_msg(msg, parent, name, state, mod, time, hibernateAfterTimeout, debug, false)
  end

  def wake_hib(parent, name, state, mod, hibernateAfterTimeout, debug) do
    msg =
      receive do
        input ->
          input
      end

    decode_msg(msg, parent, name, state, mod, :hibernate, hibernateAfterTimeout, debug, true)
  end

  defp decode_msg(msg, parent, name, state, mod, time, hibernateAfterTimeout, debug, hib) do
    case msg do
      {:system, from, req} ->
        :sys.handle_system_msg(
          req,
          from,
          parent,
          :gen_server,
          debug,
          [name, state, mod, time, hibernateAfterTimeout],
          hib
        )

      {:EXIT, ^parent, reason} ->
        terminate(
          reason,
          :erlang.element(
            2,
            :erlang.process_info(
              self(),
              :current_stacktrace
            )
          ),
          name,
          :undefined,
          msg,
          mod,
          state,
          debug
        )

      _Msg when debug === [] ->
        handle_msg(msg, parent, name, state, mod, hibernateAfterTimeout)

      _Msg ->
        debug1 = :sys.handle_debug(debug, &print_event/3, name, {:in, msg})
        handle_msg(msg, parent, name, state, mod, hibernateAfterTimeout, debug1)
    end
  end

  defp do_send(dest, msg) do
    try do
      :erlang.send(dest, msg)
    catch
      :error, _ ->
        :ok
    end

    :ok
  end

  defp do_multi_call(nodes, name, req, :infinity) do
    tag = make_ref()
    monitors = send_nodes(nodes, name, tag, req)
    rec_nodes(tag, monitors, name, :undefined)
  end

  defp do_multi_call(nodes, name, req, timeout) do
    tag = make_ref()
    caller = self()

    receiver =
      spawn(fn ->
        :erlang.process_flag(:trap_exit, true)
        mref = :erlang.monitor(:process, caller)

        receive do
          {^caller, ^tag} ->
            monitors = send_nodes(nodes, name, tag, req)
            timerId = :erlang.start_timer(timeout, self(), :ok)
            result = rec_nodes(tag, monitors, name, timerId)
            exit({self(), tag, result})

          {:DOWN, ^mref, _, _, _} ->
            exit(:normal)
        end
      end)

    mref = :erlang.monitor(:process, receiver)
    send(receiver, {self(), tag})

    receive do
      {:DOWN, ^mref, _, _, {^receiver, ^tag, result}} ->
        result

      {:DOWN, ^mref, _, _, reason} ->
        exit(reason)
    end
  end

  defp send_nodes(nodes, name, tag, req) do
    send_nodes(nodes, name, tag, req, [])
  end

  defp send_nodes([node | tail], name, tag, req, monitors)
       when is_atom(node) do
    monitor = start_monitor(node, name)

    try do
      send({name, node}, {:"$gen_call", {self(), {tag, node}}, req})
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end

    send_nodes(tail, name, tag, req, [monitor | monitors])
  end

  defp send_nodes([_Node | tail], name, tag, req, monitors) do
    send_nodes(tail, name, tag, req, monitors)
  end

  defp send_nodes([], _Name, _Tag, _Req, monitors) do
    monitors
  end

  defp rec_nodes(tag, nodes, name, timerId) do
    rec_nodes(tag, nodes, name, [], [], 2000, timerId)
  end

  defp rec_nodes(tag, [{n, r} | tail], name, badnodes, replies, time, timerId) do
    receive do
      {:DOWN, ^r, _, _, _} ->
        rec_nodes(tag, tail, name, [n | badnodes], replies, time, timerId)

      {{^tag, ^n}, reply} ->
        :erlang.demonitor(r, [:flush])
        rec_nodes(tag, tail, name, badnodes, [{n, reply} | replies], time, timerId)

      {:timeout, ^timerId, _} ->
        :erlang.demonitor(r, [:flush])
        rec_nodes_rest(tag, tail, name, [n | badnodes], replies)
    end
  end

  defp rec_nodes(tag, [n | tail], name, badnodes, replies, time, timerId) do
    receive do
      {:nodedown, ^n} ->
        :erlang.monitor_node(n, false)
        rec_nodes(tag, tail, name, [n | badnodes], replies, 2000, timerId)

      {{^tag, ^n}, reply} ->
        receive do
          {:nodedown, ^n} ->
            :ok
        after
          0 ->
            :ok
        end

        :erlang.monitor_node(n, false)
        rec_nodes(tag, tail, name, badnodes, [{n, reply} | replies], 2000, timerId)

      {:timeout, ^timerId, _} ->
        receive do
          {:nodedown, ^n} ->
            :ok
        after
          0 ->
            :ok
        end

        :erlang.monitor_node(n, false)
        rec_nodes_rest(tag, tail, name, [n | badnodes], replies)
    after
      time ->
        case :rpc.call(n, :erlang, :whereis, [name]) do
          pid when is_pid(pid) ->
            rec_nodes(tag, [n | tail], name, badnodes, replies, :infinity, timerId)

          _ ->
            receive do
              {:nodedown, ^n} ->
                :ok
            after
              0 ->
                :ok
            end

            :erlang.monitor_node(n, false)
            rec_nodes(tag, tail, name, [n | badnodes], replies, 2000, timerId)
        end
    end
  end

  defp rec_nodes(_, [], _, badnodes, replies, _, timerId) do
    case (try do
            :erlang.cancel_timer(timerId)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      false ->
        receive do
          {:timeout, ^timerId, _} ->
            :ok
        after
          0 ->
            :ok
        end

      _ ->
        :ok
    end

    {replies, badnodes}
  end

  defp rec_nodes_rest(tag, [{n, r} | tail], name, badnodes, replies) do
    receive do
      {:DOWN, ^r, _, _, _} ->
        rec_nodes_rest(tag, tail, name, [n | badnodes], replies)

      {{^tag, ^n}, reply} ->
        :erlang.demonitor(r, [:flush])
        rec_nodes_rest(tag, tail, name, badnodes, [{n, reply} | replies])
    after
      0 ->
        :erlang.demonitor(r, [:flush])
        rec_nodes_rest(tag, tail, name, [n | badnodes], replies)
    end
  end

  defp rec_nodes_rest(tag, [n | tail], name, badnodes, replies) do
    receive do
      {:nodedown, ^n} ->
        :erlang.monitor_node(n, false)
        rec_nodes_rest(tag, tail, name, [n | badnodes], replies)

      {{^tag, ^n}, reply} ->
        receive do
          {:nodedown, ^n} ->
            :ok
        after
          0 ->
            :ok
        end

        :erlang.monitor_node(n, false)
        rec_nodes_rest(tag, tail, name, badnodes, [{n, reply} | replies])
    after
      0 ->
        receive do
          {:nodedown, ^n} ->
            :ok
        after
          0 ->
            :ok
        end

        :erlang.monitor_node(n, false)
        rec_nodes_rest(tag, tail, name, [n | badnodes], replies)
    end
  end

  defp rec_nodes_rest(_Tag, [], _Name, badnodes, replies) do
    {replies, badnodes}
  end

  defp start_monitor(node, name)
       when is_atom(node) and
              is_atom(name) do
    cond do
      node() === :nonode@nohost and
          node !== :nonode@nohost ->
        ref = make_ref()
        send(self(), {:DOWN, ref, :process, {name, node}, :noconnection})
        {node, ref}

      true ->
        case (try do
                :erlang.monitor(:process, {name, node})
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          {:EXIT, _} ->
            :erlang.monitor_node(node, true)
            node

          ref when is_reference(ref) ->
            {node, ref}
        end
    end
  end

  defp try_dispatch({:"$gen_cast", msg}, mod, state) do
    try_dispatch(mod, :handle_cast, msg, state)
  end

  defp try_dispatch(info, mod, state) do
    try_dispatch(mod, :handle_info, info, state)
  end

  defp try_dispatch(mod, func, msg, state) do
    try do
      {:ok, apply(mod, func, [msg, state])}
    catch
      r ->
        {:ok, r}

      :error, :undef = r when func == :handle_info ->
        case :erlang.function_exported(mod, :handle_info, 2) do
          false ->
            case :logger.allow(:warning, :gen_server) do
              true ->
                :erlang.apply(:logger, :macro_log, [
                  %{
                    mfa: {:gen_server, :try_dispatch, 4},
                    line: 687,
                    file: 'otp/lib/stdlib/src/gen_server.erl'
                  },
                  :warning,
                  %{label: {:gen_server, :no_handle_info}, module: mod, message: msg},
                  %{
                    domain: [:otp],
                    report_cb: &:gen_server.format_log/2,
                    error_logger: %{tag: :warning_msg, report_cb: &:gen_server.format_log/1}
                  }
                ])

              false ->
                :ok
            end

            {:ok, {:noreply, state}}

          true ->
            {:EXIT, :error, r, __STACKTRACE__}
        end

      class, r ->
        {:EXIT, class, r, __STACKTRACE__}
    end
  end

  defp try_handle_call(mod, msg, from, state) do
    try do
      {:ok, mod.handle_call(msg, from, state)}
    catch
      r ->
        {:ok, r}

      class, r ->
        {:EXIT, class, r, __STACKTRACE__}
    end
  end

  defp try_terminate(mod, reason, state) do
    case :erlang.function_exported(mod, :terminate, 2) do
      true ->
        try do
          {:ok, mod.terminate(reason, state)}
        catch
          r ->
            {:ok, r}

          class, r ->
            {:EXIT, class, r, __STACKTRACE__}
        end

      false ->
        {:ok, :ok}
    end
  end

  defp handle_msg({:"$gen_call", from, msg}, parent, name, state, mod, hibernateAfterTimeout) do
    result = try_handle_call(mod, msg, from, state)

    case result do
      {:ok, {:reply, reply, nState}} ->
        reply(from, reply)
        loop(parent, name, nState, mod, :infinity, hibernateAfterTimeout, [])

      {:ok, {:reply, reply, nState, time1}} ->
        reply(from, reply)
        loop(parent, name, nState, mod, time1, hibernateAfterTimeout, [])

      {:ok, {:noreply, nState}} ->
        loop(parent, name, nState, mod, :infinity, hibernateAfterTimeout, [])

      {:ok, {:noreply, nState, time1}} ->
        loop(parent, name, nState, mod, time1, hibernateAfterTimeout, [])

      {:ok, {:stop, reason, reply, nState}} ->
        try do
          terminate(
            reason,
            :erlang.element(
              2,
              :erlang.process_info(
                self(),
                :current_stacktrace
              )
            ),
            name,
            from,
            msg,
            mod,
            nState,
            []
          )
        after
          reply(from, reply)
        end

      other ->
        handle_common_reply(other, parent, name, from, msg, mod, hibernateAfterTimeout, state)
    end
  end

  defp handle_msg(msg, parent, name, state, mod, hibernateAfterTimeout) do
    reply = try_dispatch(msg, mod, state)
    handle_common_reply(reply, parent, name, :undefined, msg, mod, hibernateAfterTimeout, state)
  end

  defp handle_msg(
         {:"$gen_call", from, msg},
         parent,
         name,
         state,
         mod,
         hibernateAfterTimeout,
         debug
       ) do
    result = try_handle_call(mod, msg, from, state)

    case result do
      {:ok, {:reply, reply, nState}} ->
        debug1 = reply(name, from, reply, nState, debug)
        loop(parent, name, nState, mod, :infinity, hibernateAfterTimeout, debug1)

      {:ok, {:reply, reply, nState, time1}} ->
        debug1 = reply(name, from, reply, nState, debug)
        loop(parent, name, nState, mod, time1, hibernateAfterTimeout, debug1)

      {:ok, {:noreply, nState}} ->
        debug1 = :sys.handle_debug(debug, &print_event/3, name, {:noreply, nState})
        loop(parent, name, nState, mod, :infinity, hibernateAfterTimeout, debug1)

      {:ok, {:noreply, nState, time1}} ->
        debug1 = :sys.handle_debug(debug, &print_event/3, name, {:noreply, nState})
        loop(parent, name, nState, mod, time1, hibernateAfterTimeout, debug1)

      {:ok, {:stop, reason, reply, nState}} ->
        try do
          terminate(
            reason,
            :erlang.element(
              2,
              :erlang.process_info(
                self(),
                :current_stacktrace
              )
            ),
            name,
            from,
            msg,
            mod,
            nState,
            debug
          )
        after
          _ = reply(name, from, reply, nState, debug)
        end

      other ->
        handle_common_reply(
          other,
          parent,
          name,
          from,
          msg,
          mod,
          hibernateAfterTimeout,
          state,
          debug
        )
    end
  end

  defp handle_msg(msg, parent, name, state, mod, hibernateAfterTimeout, debug) do
    reply = try_dispatch(msg, mod, state)

    handle_common_reply(
      reply,
      parent,
      name,
      :undefined,
      msg,
      mod,
      hibernateAfterTimeout,
      state,
      debug
    )
  end

  defp handle_common_reply(reply, parent, name, from, msg, mod, hibernateAfterTimeout, state) do
    case reply do
      {:ok, {:noreply, nState}} ->
        loop(parent, name, nState, mod, :infinity, hibernateAfterTimeout, [])

      {:ok, {:noreply, nState, time1}} ->
        loop(parent, name, nState, mod, time1, hibernateAfterTimeout, [])

      {:ok, {:stop, reason, nState}} ->
        terminate(
          reason,
          :erlang.element(
            2,
            :erlang.process_info(
              self(),
              :current_stacktrace
            )
          ),
          name,
          from,
          msg,
          mod,
          nState,
          []
        )

      {:EXIT, class, reason, stacktrace} ->
        terminate(class, reason, stacktrace, name, from, msg, mod, state, [])

      {:ok, badReply} ->
        terminate(
          {:bad_return_value, badReply},
          :erlang.element(
            2,
            :erlang.process_info(
              self(),
              :current_stacktrace
            )
          ),
          name,
          from,
          msg,
          mod,
          state,
          []
        )
    end
  end

  defp handle_common_reply(
         reply,
         parent,
         name,
         from,
         msg,
         mod,
         hibernateAfterTimeout,
         state,
         debug
       ) do
    case reply do
      {:ok, {:noreply, nState}} ->
        debug1 = :sys.handle_debug(debug, &print_event/3, name, {:noreply, nState})
        loop(parent, name, nState, mod, :infinity, hibernateAfterTimeout, debug1)

      {:ok, {:noreply, nState, time1}} ->
        debug1 = :sys.handle_debug(debug, &print_event/3, name, {:noreply, nState})
        loop(parent, name, nState, mod, time1, hibernateAfterTimeout, debug1)

      {:ok, {:stop, reason, nState}} ->
        terminate(
          reason,
          :erlang.element(
            2,
            :erlang.process_info(
              self(),
              :current_stacktrace
            )
          ),
          name,
          from,
          msg,
          mod,
          nState,
          debug
        )

      {:EXIT, class, reason, stacktrace} ->
        terminate(class, reason, stacktrace, name, from, msg, mod, state, debug)

      {:ok, badReply} ->
        terminate(
          {:bad_return_value, badReply},
          :erlang.element(
            2,
            :erlang.process_info(
              self(),
              :current_stacktrace
            )
          ),
          name,
          from,
          msg,
          mod,
          state,
          debug
        )
    end
  end

  defp reply(name, from, reply, state, debug) do
    reply(from, reply)
    :sys.handle_debug(debug, &print_event/3, name, {:out, reply, from, state})
  end

  def system_continue(parent, debug, [name, state, mod, time, hibernateAfterTimeout]) do
    loop(parent, name, state, mod, time, hibernateAfterTimeout, debug)
  end

  def system_terminate(reason, _Parent, debug, [name, state, mod, _Time, _HibernateAfterTimeout]) do
    terminate(
      reason,
      :erlang.element(
        2,
        :erlang.process_info(
          self(),
          :current_stacktrace
        )
      ),
      name,
      :undefined,
      [],
      mod,
      state,
      debug
    )
  end

  def system_code_change([name, state, mod, time, hibernateAfterTimeout], _Module, oldVsn, extra) do
    case (try do
            mod.code_change(oldVsn, state, extra)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, newState} ->
        {:ok, [name, newState, mod, time, hibernateAfterTimeout]}

      else__ ->
        else__
    end
  end

  def system_get_state([_Name, state, _Mod, _Time, _HibernateAfterTimeout]) do
    {:ok, state}
  end

  def system_replace_state(
        stateFun,
        [name, state, mod, time, hibernateAfterTimeout]
      ) do
    nState = stateFun.(state)
    {:ok, nState, [name, nState, mod, time, hibernateAfterTimeout]}
  end

  defp print_event(dev, {:in, msg}, name) do
    case msg do
      {:"$gen_call", {from, _Tag}, call} ->
        :io.format(dev, '*DBG* ~tp got call ~tp from ~tw~n', [name, call, from])

      {:"$gen_cast", cast} ->
        :io.format(dev, '*DBG* ~tp got cast ~tp~n', [name, cast])

      _ ->
        :io.format(dev, '*DBG* ~tp got ~tp~n', [name, msg])
    end
  end

  defp print_event(dev, {:out, msg, {to, _Tag}, state}, name) do
    :io.format(dev, '*DBG* ~tp sent ~tp to ~tw, new state ~tp~n', [name, msg, to, state])
  end

  defp print_event(dev, {:noreply, state}, name) do
    :io.format(dev, '*DBG* ~tp new state ~tp~n', [name, state])
  end

  defp print_event(dev, event, name) do
    :io.format(dev, '*DBG* ~tp dbg  ~tp~n', [name, event])
  end

  defp terminate(reason, stacktrace, name, from, msg, mod, state, debug) do
    terminate(:exit, reason, stacktrace, reason, name, from, msg, mod, state, debug)
  end

  defp terminate(class, reason, stacktrace, name, from, msg, mod, state, debug) do
    reportReason = {reason, stacktrace}
    terminate(class, reason, stacktrace, reportReason, name, from, msg, mod, state, debug)
  end

  defp terminate(class, reason, stacktrace, reportReason, name, from, msg, mod, state, debug) do
    reply = try_terminate(mod, terminate_reason(class, reason, stacktrace), state)

    case reply do
      {:EXIT, c, r, s} ->
        error_info({r, s}, name, from, msg, mod, state, debug)
        :erlang.raise(c, r, s)

      _ ->
        case {class, reason} do
          {:exit, :normal} ->
            :ok

          {:exit, :shutdown} ->
            :ok

          {:exit, {:shutdown, _}} ->
            :ok

          _ ->
            error_info(reportReason, name, from, msg, mod, state, debug)
        end
    end

    case stacktrace do
      [] ->
        apply(:erlang, class, [reason])

      _ ->
        :erlang.raise(class, reason, stacktrace)
    end
  end

  defp terminate_reason(:error, reason, stacktrace) do
    {reason, stacktrace}
  end

  defp terminate_reason(:exit, reason, _Stacktrace) do
    reason
  end

  defp error_info(_Reason, :application_controller, _From, _Msg, _Mod, _State, _Debug) do
    :ok
  end

  defp error_info(reason, name, from, msg, mod, state, debug) do
    log = :sys.get_log(debug)

    case :logger.allow(:error, :gen_server) do
      true ->
        :erlang.apply(:logger, :macro_log, [
          %{
            mfa: {:gen_server, :error_info, 7},
            line: 934,
            file: 'otp/lib/stdlib/src/gen_server.erl'
          },
          :error,
          %{
            label: {:gen_server, :terminate},
            name: name,
            last_message: msg,
            state: format_status(:terminate, mod, :erlang.get(), state),
            log: format_log_state(mod, log),
            reason: reason,
            client_info: client_stacktrace(from)
          },
          %{
            domain: [:otp],
            report_cb: &:gen_server.format_log/2,
            error_logger: %{tag: :error, report_cb: &:gen_server.format_log/1}
          }
        ])

      false ->
        :ok
    end

    :ok
  end

  defp client_stacktrace(:undefined) do
    :undefined
  end

  defp client_stacktrace({from, _Tag}) do
    client_stacktrace(from)
  end

  defp client_stacktrace(from)
       when is_pid(from) and
              node(from) === node() do
    case :erlang.process_info(
           from,
           [:current_stacktrace, :registered_name]
         ) do
      :undefined ->
        {from, :dead}

      [{:current_stacktrace, stacktrace}, {:registered_name, []}] ->
        {from, {from, stacktrace}}

      [{:current_stacktrace, stacktrace}, {:registered_name, name}] ->
        {from, {name, stacktrace}}
    end
  end

  defp client_stacktrace(from) when is_pid(from) do
    {from, :remote}
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
         %{
           label: {:gen_server, :terminate},
           last_message: msg,
           state: state,
           log: log,
           reason: reason,
           client_info: client
         } = report,
         depth
       ) do
    Map.merge(report, %{
      last_message: :io_lib.limit_term(msg, depth),
      state: :io_lib.limit_term(state, depth),
      log:
        for l <- log do
          :io_lib.limit_term(l, depth)
        end,
      reason: :io_lib.limit_term(reason, depth),
      client_info: limit_client_report(client, depth)
    })
  end

  defp limit_report(
         %{label: {:gen_server, :no_handle_info}, message: msg} = report,
         depth
       ) do
    Map.put(report, :message, :io_lib.limit_term(msg, depth))
  end

  defp limit_client_report({from, {name, stacktrace}}, depth) do
    {from, {name, :io_lib.limit_term(stacktrace, depth)}}
  end

  defp limit_client_report(client, _) do
    client
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
           label: {:gen_server, :terminate},
           name: name,
           last_message: msg,
           state: state,
           log: log,
           reason: reason,
           client_info: client
         },
         %{single_line: true, depth: depth} = formatOpts
       ) do
    p = p(formatOpts)

    format1 =
      :lists.append([
        'Generic server ',
        p,
        ' terminating. Reason: ',
        p,
        '. Last message: ',
        p,
        '. State: ',
        p,
        '.'
      ])

    {serverLogFormat, serverLogArgs} =
      format_server_log_single(
        log,
        formatOpts
      )

    {clientLogFormat, clientLogArgs} =
      format_client_log_single(
        client,
        formatOpts
      )

    args1 =
      case depth do
        :unlimited ->
          [name, fix_reason(reason), msg, state]

        _ ->
          [name, depth, fix_reason(reason), depth, msg, depth, state, depth]
      end

    {format1 ++ serverLogFormat ++ clientLogFormat, args1 ++ serverLogArgs ++ clientLogArgs}
  end

  defp format_log_single(
         %{label: {:gen_server, :no_handle_info}, module: mod, message: msg},
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
           label: {:gen_server, :terminate},
           name: name,
           last_message: msg,
           state: state,
           log: log,
           reason: reason,
           client_info: client
         },
         %{depth: depth} = formatOpts
       ) do
    reason1 = fix_reason(reason)

    {clientFmt, clientArgs} =
      format_client_log(
        client,
        formatOpts
      )

    p = p(formatOpts)

    format =
      :lists.append(
        [
          '** Generic server ',
          p,
          ' terminating \n** Last message in was ',
          p,
          '~n** When Server state == ',
          p,
          '~n** Reason for termination ==~n** ',
          p,
          '~n'
        ] ++
          case log do
            [] ->
              []

            _ ->
              [
                '** Log ==~n** ['
                | :lists.join(
                    ',~n    ',
                    :lists.duplicate(
                      length(log),
                      p
                    )
                  )
              ] ++ [']~n']
          end
      ) ++ clientFmt

    args =
      case depth do
        :unlimited ->
          [name, msg, state, reason1] ++
            case log do
              [] ->
                []

              _ ->
                log
            end ++ clientArgs

        _ ->
          [name, depth, msg, depth, state, depth, reason1, depth] ++
            case log do
              [] ->
                []

              _ ->
                :lists.flatmap(
                  fn l ->
                    [l, depth]
                  end,
                  log
                )
            end ++ clientArgs
      end

    {format, args}
  end

  defp format_log_multi(
         %{label: {:gen_server, :no_handle_info}, module: mod, message: msg},
         %{depth: depth} = formatOpts
       ) do
    p = p(formatOpts)
    format = '** Undefined handle_info in ~p~n** Unhandled message: ' ++ p ++ '~n'

    args =
      case depth do
        :unlimited ->
          [mod, msg]

        _ ->
          [mod, msg, depth]
      end

    {format, args}
  end

  defp fix_reason({:undef, [{m, f, a, l} | mFAs]} = reason) do
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
  end

  defp fix_reason(reason) do
    reason
  end

  defp format_server_log_single([], _) do
    {'', []}
  end

  defp format_server_log_single(log, formatOpts) do
    args =
      case :maps.get(:depth, formatOpts) do
        :unlimited ->
          [log]

        depth ->
          [log, depth]
      end

    {' Log: ' ++ p(formatOpts), args}
  end

  defp format_client_log_single(:undefined, _) do
    {'', []}
  end

  defp format_client_log_single({from, :dead}, _) do
    {' Client ~0p is dead.', [from]}
  end

  defp format_client_log_single({from, :remote}, _) do
    {' Client ~0p is remote on node ~0p.', [from, node(from)]}
  end

  defp format_client_log_single({_From, {name, stacktrace0}}, formatOpts) do
    p = p(formatOpts)
    stacktrace = :lists.sublist(stacktrace0, 4)

    args =
      case :maps.get(:depth, formatOpts) do
        :unlimited ->
          [name, stacktrace]

        depth ->
          [name, depth, stacktrace, depth]
      end

    {' Client ' ++ p ++ ' stacktrace: ' ++ p ++ '.', args}
  end

  defp format_client_log(:undefined, _) do
    {'', []}
  end

  defp format_client_log({from, :dead}, _) do
    {'** Client ~p is dead~n', [from]}
  end

  defp format_client_log({from, :remote}, _) do
    {'** Client ~p is remote on node ~p~n', [from, node(from)]}
  end

  defp format_client_log({_From, {name, stacktrace}}, formatOpts) do
    p = p(formatOpts)
    format = :lists.append(['** Client ', p, ' stacktrace~n', '** ', p, '~n'])

    args =
      case :maps.get(:depth, formatOpts) do
        :unlimited ->
          [name, stacktrace]

        depth ->
          [name, depth, stacktrace, depth]
      end

    {format, args}
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

  def format_status(opt, statusData) do
    [pDict, sysState, parent, debug, [name, state, mod, _Time, _HibernateAfterTimeout]] =
      statusData

    header = :gen.format_status_header('Status for generic server', name)
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
