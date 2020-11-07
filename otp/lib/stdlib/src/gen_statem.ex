defmodule :m_gen_statem do
  use Bitwise
  import Kernel, except: [send: 2]

  defp callback_mode(callbackMode) do
    case callbackMode do
      :state_functions ->
        true

      :handle_event_function ->
        true

      _ ->
        false
    end
  end

  defp state_enter(stateEnter) do
    case stateEnter do
      :state_enter ->
        true

      _ ->
        false
    end
  end

  defp event_type(type) do
    case type do
      {:call, from} ->
        from(from)

      :cast ->
        true

      :info ->
        true

      :internal ->
        true

      _ ->
        timeout_event_type(type)
    end
  end

  defp from({pid, _}) when is_pid(pid) do
    true
  end

  defp from(_) do
    false
  end

  defp timeout_event_type(type) do
    case type do
      :timeout ->
        true

      :state_timeout ->
        true

      {:timeout, _Name} ->
        true

      _ ->
        false
    end
  end

  require Record

  Record.defrecord(:r_params, :params,
    callback_mode: :undefined,
    state_enter: false,
    parent: :undefined,
    modules: :undefined,
    name: :undefined,
    hibernate_after: :infinity
  )

  Record.defrecord(:r_state, :state,
    state_data: {:undefined, :undefined},
    postponed: [],
    timers: %{:t0q => []},
    hibernate: false
  )

  def start(module, args, opts) do
    :gen.start(:gen_statem, :nolink, module, args, opts)
  end

  def start(serverName, module, args, opts) do
    :gen.start(:gen_statem, :nolink, serverName, module, args, opts)
  end

  def start_link(module, args, opts) do
    :gen.start(:gen_statem, :link, module, args, opts)
  end

  def start_link(serverName, module, args, opts) do
    :gen.start(:gen_statem, :link, serverName, module, args, opts)
  end

  def start_monitor(module, args, opts) do
    :gen.start(:gen_statem, :monitor, module, args, opts)
  end

  def start_monitor(serverName, module, args, opts) do
    :gen.start(:gen_statem, :monitor, serverName, module, args, opts)
  end

  def stop(serverRef) do
    :gen.stop(serverRef)
  end

  def stop(serverRef, reason, timeout) do
    :gen.stop(serverRef, reason, timeout)
  end

  def cast(serverRef, msg) when is_pid(serverRef) do
    send(serverRef, wrap_cast(msg))
  end

  def cast(serverRef, msg) when is_atom(serverRef) do
    send(serverRef, wrap_cast(msg))
  end

  def cast({:global, name}, msg) do
    try do
      :global.send(name, wrap_cast(msg))
    catch
      _, _ ->
        :ok
    else
      _ ->
        :ok
    end
  end

  def cast({:via, regMod, name}, msg) do
    try do
      regMod.send(name, wrap_cast(msg))
    catch
      _, _ ->
        :ok
    else
      _ ->
        :ok
    end
  end

  def cast({name, node} = serverRef, msg)
      when is_atom(name) and is_atom(node) do
    send(serverRef, wrap_cast(msg))
  end

  def call(serverRef, request) do
    call(serverRef, request, :infinity)
  end

  def call(serverRef, request, :infinity = t = timeout) do
    call_dirty(serverRef, request, timeout, t)
  end

  def call(serverRef, request, {:dirty_timeout, t} = timeout) do
    call_dirty(serverRef, request, timeout, t)
  end

  def call(serverRef, request, {:clean_timeout, t} = timeout) do
    call_clean(serverRef, request, timeout, t)
  end

  def call(serverRef, request, {_, _} = timeout) do
    :erlang.error(:badarg, [serverRef, request, timeout])
  end

  def call(serverRef, request, timeout) do
    call_clean(serverRef, request, timeout, timeout)
  end

  def send_request(name, request) do
    :gen.send_request(name, :"$gen_call", request)
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

  def reply({:reply, from, reply}) do
    reply(from, reply)
  end

  def reply(replies) when is_list(replies) do
    replies(replies)
  end

  def reply({to, tag}, reply) when is_pid(to) do
    msg = {tag, reply}

    try do
      send(to, msg)
    catch
      _, _ ->
        :ok
    else
      _ ->
        :ok
    end
  end

  def enter_loop(module, opts, state, data) do
    enter_loop(module, opts, state, data, self())
  end

  def enter_loop(module, opts, state, data, server_or_Actions) do
    cond do
      is_list(server_or_Actions) ->
        enter_loop(module, opts, state, data, self(), server_or_Actions)

      true ->
        enter_loop(module, opts, state, data, server_or_Actions, [])
    end
  end

  def enter_loop(module, opts, state, data, server, actions) do
    is_atom(module) or :erlang.error({:atom, module})
    parent = :gen.get_parent()
    name = :gen.get_proc_name(server)
    debug = :gen.debug_options(name, opts)
    hibernateAfterTimeout = :gen.hibernate_after(opts)
    enter(parent, debug, module, name, hibernateAfterTimeout, state, data, actions)
  end

  defp wrap_cast(event) do
    {:"$gen_cast", event}
  end

  defp call_dirty(serverRef, request, timeout, t) do
    try do
      :gen.call(serverRef, :"$gen_call", request, t)
    catch
      class, reason ->
        :erlang.raise(
          class,
          {reason, {:gen_statem, :call, [serverRef, request, timeout]}},
          __STACKTRACE__
        )
    else
      {:ok, reply} ->
        reply
    end
  end

  defp call_clean(serverRef, request, timeout, t) do
    ref = make_ref()
    self = self()

    pid =
      spawn(fn ->
        send(
          self,
          try do
            :gen.call(serverRef, :"$gen_call", request, t)
          catch
            class, reason ->
              {ref, class, reason, __STACKTRACE__}
          else
            result ->
              {ref, result}
          end
        )
      end)

    mref = :erlang.monitor(:process, pid)

    receive do
      {^ref, result} ->
        :erlang.demonitor(mref, [:flush])

        case result do
          {:ok, reply} ->
            reply
        end

      {^ref, class, reason, stacktrace} ->
        :erlang.demonitor(mref, [:flush])

        :erlang.raise(
          class,
          {reason, {:gen_statem, :call, [serverRef, request, timeout]}},
          stacktrace
        )

      {:DOWN, ^mref, _, _, reason} ->
        exit(reason)
    end
  end

  defp replies([{:reply, from, reply} | replies]) do
    reply(from, reply)
    replies(replies)
  end

  defp replies([]) do
    :ok
  end

  defp send(proc, msg) do
    try do
      :erlang.send(proc, msg)
    catch
      :error, _ ->
        :ok
    end

    :ok
  end

  defp enter(parent, debug, module, name, hibernateAfterTimeout, state, data, actions) do
    q = [{:internal, :init_state}]
    actions_1 = listify(actions) ++ [{:postpone, false}]

    p =
      r_params(
        parent: parent,
        modules: [module],
        name: name,
        hibernate_after: hibernateAfterTimeout
      )

    s = r_state(state_data: {state, data})

    debug_1 =
      case debug do
        [] ->
          debug

        _ ->
          sys_debug(
            debug,
            name,
            {:enter, state}
          )
      end

    loop_state_callback(p, debug_1, s, q, {state, data}, actions_1)
  end

  def init_it(starter, :self, serverRef, module, args, opts) do
    init_it(starter, self(), serverRef, module, args, opts)
  end

  def init_it(starter, parent, serverRef, module, args, opts) do
    name = :gen.get_proc_name(serverRef)
    debug = :gen.debug_options(name, opts)
    hibernateAfterTimeout = :gen.hibernate_after(opts)

    try do
      module.init(args)
    catch
      result ->
        init_result(
          starter,
          parent,
          serverRef,
          module,
          result,
          name,
          debug,
          hibernateAfterTimeout
        )

      class, reason ->
        :gen.unregister_name(serverRef)
        :proc_lib.init_ack(starter, {:error, reason})

        error_info(
          class,
          reason,
          __STACKTRACE__,
          debug,
          r_params(parent: parent, name: name, modules: [module]),
          r_state(),
          []
        )

        :erlang.raise(class, reason, __STACKTRACE__)
    else
      result ->
        init_result(
          starter,
          parent,
          serverRef,
          module,
          result,
          name,
          debug,
          hibernateAfterTimeout
        )
    end
  end

  defp init_result(starter, parent, serverRef, module, result, name, debug, hibernateAfterTimeout) do
    case result do
      {:ok, state, data} ->
        :proc_lib.init_ack(starter, {:ok, self()})
        enter(parent, debug, module, name, hibernateAfterTimeout, state, data, [])

      {:ok, state, data, actions} ->
        :proc_lib.init_ack(starter, {:ok, self()})
        enter(parent, debug, module, name, hibernateAfterTimeout, state, data, actions)

      {:stop, reason} ->
        :gen.unregister_name(serverRef)
        :proc_lib.init_ack(starter, {:error, reason})
        exit(reason)

      :ignore ->
        :gen.unregister_name(serverRef)
        :proc_lib.init_ack(starter, :ignore)
        exit(:normal)

      _ ->
        :gen.unregister_name(serverRef)
        error = {:bad_return_from_init, result}
        :proc_lib.init_ack(starter, {:error, error})

        error_info(
          :error,
          error,
          :erlang.element(
            2,
            :erlang.process_info(
              self(),
              :current_stacktrace
            )
          ),
          debug,
          r_params(parent: parent, name: name, modules: [module]),
          r_state(),
          []
        )

        exit(error)
    end
  end

  def system_continue(parent, debug, {p, s}) do
    loop(update_parent(p, parent), debug, s)
  end

  def system_terminate(reason, parent, debug, {p, s}) do
    terminate(
      :exit,
      reason,
      :erlang.element(
        2,
        :erlang.process_info(
          self(),
          :current_stacktrace
        )
      ),
      update_parent(p, parent),
      debug,
      s,
      []
    )
  end

  def system_code_change(
        {r_params(modules: [module | _]) = p, r_state(state_data: {state, data}) = s},
        _Mod,
        oldVsn,
        extra
      ) do
    case (try do
            module.code_change(oldVsn, state, data, extra)
          catch
            result ->
              result
          end) do
      {:ok, newState, newData} ->
        {:ok,
         {r_params(p, callback_mode: :undefined), r_state(s, state_data: {newState, newData})}}

      {:ok, _} = error ->
        :erlang.error({:case_clause, error})

      error ->
        error
    end
  end

  def system_get_state({_P, r_state(state_data: state_Data)}) do
    {:ok, state_Data}
  end

  def system_replace_state(stateFun, {p, r_state(state_data: state_Data) = s}) do
    newState_NewData = stateFun.(state_Data)
    {:ok, newState_NewData, {p, r_state(s, state_data: newState_NewData)}}
  end

  def format_status(
        opt,
        [
          pDict,
          sysState,
          parent,
          debug,
          {r_params(name: name, modules: modules) = p,
           r_state(postponed: postponed, timers: timers) = s}
        ]
      ) do
    header = :gen.format_status_header('Status for state machine', name)
    log = :sys.get_log(debug)

    [
      [
        {:header, header},
        {:data,
         [
           {'Status', sysState},
           {'Parent', parent},
           {'Modules', modules},
           {'Time-outs', list_timeouts(timers)},
           {'Logged Events', log},
           {'Postponed', postponed}
         ]}
      ]
      | case format_status(opt, pDict, update_parent(p, parent), s) do
          l when is_list(l) ->
            l

          t ->
            [t]
        end
    ]
  end

  defp update_parent(p, parent) do
    case p do
      r_params(parent: ^parent) ->
        p

      r_params() ->
        r_params(p, parent: parent)
    end
  end

  defp sys_debug(debug, nameState, entry) do
    :sys.handle_debug(debug, &print_event/3, nameState, entry)
  end

  defp print_event(dev, systemEvent, name) do
    case systemEvent do
      {:in, event, state} ->
        :io.format(dev, '*DBG* ~tp receive ~ts in state ~tp~n', [name, event_string(event), state])

      {:code_change, event, state} ->
        :io.format(dev, '*DBG* ~tp receive ~ts after code change in state ~tp~n', [
          name,
          event_string(event),
          state
        ])

      {:out, reply, {to, _Tag}} ->
        :io.format(dev, '*DBG* ~tp send ~tp to ~tw~n', [name, reply, to])

      {:enter, state} ->
        :io.format(dev, '*DBG* ~tp enter in state ~tp~n', [name, state])

      {:start_timer, action, state} ->
        :io.format(dev, '*DBG* ~tp start_timer ~tp in state ~tp~n', [name, action, state])

      {:insert_timeout, event, state} ->
        :io.format(dev, '*DBG* ~tp insert_timeout ~tp in state ~tp~n', [name, event, state])

      {:terminate, reason, state} ->
        :io.format(dev, '*DBG* ~tp terminate ~tp in state ~tp~n', [name, reason, state])

      {tag, event, state, nextState}
      when tag === :postpone or
             tag === :consume ->
        stateString =
          case nextState do
            ^state ->
              :io_lib.format('~tp', [state])

            _ ->
              :io_lib.format('~tp => ~tp', [state, nextState])
          end

        :io.format(dev, '*DBG* ~tp ~tw ~ts in state ~ts~n', [
          name,
          tag,
          event_string(event),
          stateString
        ])
    end
  end

  defp event_string(event) do
    case event do
      {{:call, {pid, _Tag}}, request} ->
        :io_lib.format('call ~tp from ~tw', [request, pid])

      {eventType, eventContent} ->
        :io_lib.format('~tw ~tp', [eventType, eventContent])
    end
  end

  def wakeup_from_hibernate(p, debug, s) do
    loop_receive(p, debug, s)
  end

  defp loop(p, debug, r_state(hibernate: true) = s) do
    loop_hibernate(p, debug, s)
  end

  defp loop(p, debug, s) do
    loop_receive(p, debug, s)
  end

  defp loop_hibernate(p, debug, s) do
    :proc_lib.hibernate(:gen_statem, :wakeup_from_hibernate, [p, debug, s])

    :erlang.error(
      {:should_not_have_arrived_here_but_instead_in, {:gen_statem, :wakeup_from_hibernate, 3}}
    )
  end

  defp loop_receive(r_params(hibernate_after: hibernateAfterTimeout) = p, debug, s) do
    receive do
      msg ->
        case msg do
          {:"$gen_call", from, request} ->
            loop_receive_result(p, debug, s, {{:call, from}, request})

          {:"$gen_cast", cast} ->
            loop_receive_result(p, debug, s, {:cast, cast})

          {:timeout, timerRef, timeoutType} ->
            case r_state(s, :timers) do
              %{^timeoutType => {^timerRef, timeoutMsg}} = timers
              when timeoutType !== :t0q ->
                timers_1 = :maps.remove(timeoutType, timers)
                s_1 = r_state(s, timers: timers_1)
                loop_receive_result(p, debug, s_1, {timeoutType, timeoutMsg})

              %{} ->
                loop_receive_result(p, debug, s, {:info, msg})
            end

          {:system, pid, req} ->
            :sys.handle_system_msg(
              req,
              pid,
              r_params(p, :parent),
              :gen_statem,
              debug,
              {p, s},
              r_state(s, :hibernate)
            )

          {:EXIT, pid, reason} ->
            case r_params(p, :parent) do
              ^pid ->
                terminate(
                  :exit,
                  reason,
                  :erlang.element(
                    2,
                    :erlang.process_info(
                      self(),
                      :current_stacktrace
                    )
                  ),
                  p,
                  debug,
                  s,
                  []
                )

              _ ->
                loop_receive_result(p, debug, s, {:info, msg})
            end

          _ ->
            loop_receive_result(p, debug, s, {:info, msg})
        end
    after
      hibernateAfterTimeout ->
        loop_hibernate(p, debug, s)
    end
  end

  defp loop_receive_result(p, [] = debug, s, event) do
    events = []
    loop_event(p, debug, s, event, events)
  end

  defp loop_receive_result(
         r_params(name: name, callback_mode: callbackMode) = p,
         debug,
         r_state(state_data: {state, _Data}) = s,
         event
       ) do
    debug_1 =
      case callbackMode do
        :undefined ->
          sys_debug(debug, name, {:code_change, event, state})

        _ ->
          sys_debug(debug, name, {:in, event, state})
      end

    events = []
    loop_event(p, debug_1, s, event, events)
  end

  defp loop_event(p, debug, r_state(hibernate: true) = s, event, events) do
    _ = :erlang.garbage_collect()
    loop_event_handler(p, debug, s, event, events)
  end

  defp loop_event(p, debug, s, event, events) do
    loop_event_handler(p, debug, s, event, events)
  end

  defp loop_event_handler(p, debug, r_state(state_data: state_Data) = s, event, events) do
    q = [event | events]
    loop_state_callback(p, debug, s, q, state_Data, event)
  end

  defp loop_state_enter(
         p,
         debug,
         r_state(state_data: {prevState, _PrevData}) = s,
         q,
         nextState_NewData,
         nextEventsR,
         hibernate,
         timeoutsR,
         postpone
       ) do
    stateCall = false
    callbackEvent = {:enter, prevState}

    loop_state_callback(
      p,
      debug,
      s,
      q,
      nextState_NewData,
      nextEventsR,
      hibernate,
      timeoutsR,
      postpone,
      stateCall,
      callbackEvent
    )
  end

  defp loop_state_callback(p, debug, s, q, state_Data, callbackEvent) do
    nextEventsR = []
    hibernate = false
    timeoutsR = []
    postpone = false
    stateCall = true

    loop_state_callback(
      p,
      debug,
      s,
      q,
      state_Data,
      nextEventsR,
      hibernate,
      timeoutsR,
      postpone,
      stateCall,
      callbackEvent
    )
  end

  defp loop_state_callback(
         r_params(
           callback_mode: :undefined,
           modules: [module | _]
         ) = p,
         debug,
         s,
         q,
         state_Data,
         nextEventsR,
         hibernate,
         timeoutsR,
         postpone,
         stateCall,
         callbackEvent
       ) do
    try do
      module.callback_mode()
    catch
      callbackMode ->
        loop_callback_mode_result(
          p,
          debug,
          s,
          q,
          state_Data,
          nextEventsR,
          hibernate,
          timeoutsR,
          postpone,
          stateCall,
          callbackEvent,
          callbackMode,
          listify(callbackMode),
          :undefined,
          false
        )

      class, reason ->
        terminate(class, reason, __STACKTRACE__, p, debug, s, q)
    else
      callbackMode ->
        loop_callback_mode_result(
          p,
          debug,
          s,
          q,
          state_Data,
          nextEventsR,
          hibernate,
          timeoutsR,
          postpone,
          stateCall,
          callbackEvent,
          callbackMode,
          listify(callbackMode),
          :undefined,
          false
        )
    end
  end

  defp loop_state_callback(
         r_params(
           callback_mode: callbackMode,
           modules: [module | _]
         ) = p,
         debug,
         s,
         q,
         {state, data} = state_Data,
         nextEventsR,
         hibernate,
         timeoutsR,
         postpone,
         stateCall,
         {type, content}
       ) do
    try do
      case callbackMode do
        :state_functions ->
          apply(module, state, [type, content, data])

        :handle_event_function ->
          module.handle_event(type, content, state, data)
      end
    catch
      result ->
        loop_state_callback_result(
          p,
          debug,
          s,
          q,
          state_Data,
          nextEventsR,
          hibernate,
          timeoutsR,
          postpone,
          stateCall,
          result
        )

      class, reason ->
        terminate(class, reason, __STACKTRACE__, p, debug, s, q)
    else
      result ->
        loop_state_callback_result(
          p,
          debug,
          s,
          q,
          state_Data,
          nextEventsR,
          hibernate,
          timeoutsR,
          postpone,
          stateCall,
          result
        )
    end
  end

  defp loop_state_callback(
         p,
         debug,
         s,
         q,
         state_Data,
         nextEventsR,
         hibernate,
         timeoutsR,
         postpone,
         stateCall,
         actions
       )
       when is_list(actions) do
    callEnter = true

    loop_actions_list(
      p,
      debug,
      s,
      q,
      state_Data,
      nextEventsR,
      hibernate,
      timeoutsR,
      postpone,
      callEnter,
      stateCall,
      actions
    )
  end

  defp loop_callback_mode_result(
         p,
         debug,
         s,
         q,
         state_Data,
         nextEventsR,
         hibernate,
         timeoutsR,
         postpone,
         stateCall,
         callbackEvent,
         callbackMode,
         [h | t],
         newCallbackMode,
         newStateEnter
       ) do
    case callback_mode(h) do
      true ->
        loop_callback_mode_result(
          p,
          debug,
          s,
          q,
          state_Data,
          nextEventsR,
          hibernate,
          timeoutsR,
          postpone,
          stateCall,
          callbackEvent,
          callbackMode,
          t,
          h,
          newStateEnter
        )

      false ->
        case state_enter(h) do
          true ->
            loop_callback_mode_result(
              p,
              debug,
              s,
              q,
              state_Data,
              nextEventsR,
              hibernate,
              timeoutsR,
              postpone,
              stateCall,
              callbackEvent,
              callbackMode,
              t,
              newCallbackMode,
              true
            )

          false ->
            terminate(
              :error,
              {:bad_return_from_callback_mode, callbackMode},
              :erlang.element(
                2,
                :erlang.process_info(
                  self(),
                  :current_stacktrace
                )
              ),
              p,
              debug,
              s,
              q
            )
        end
    end
  end

  defp loop_callback_mode_result(
         p,
         debug,
         s,
         q,
         state_Data,
         nextEventsR,
         hibernate,
         timeoutsR,
         postpone,
         stateCall,
         callbackEvent,
         callbackMode,
         [],
         newCallbackMode,
         newStateEnter
       ) do
    case newCallbackMode do
      :undefined ->
        terminate(
          :error,
          {:bad_return_from_callback_mode, callbackMode},
          :erlang.element(
            2,
            :erlang.process_info(
              self(),
              :current_stacktrace
            )
          ),
          p,
          debug,
          s,
          q
        )

      _ ->
        p_1 =
          r_params(p,
            callback_mode: newCallbackMode,
            state_enter: newStateEnter
          )

        loop_state_callback(
          p_1,
          debug,
          s,
          q,
          state_Data,
          nextEventsR,
          hibernate,
          timeoutsR,
          postpone,
          stateCall,
          callbackEvent
        )
    end
  end

  defp loop_state_callback_result(
         p,
         debug,
         s,
         q,
         {state, _Data} = state_Data,
         nextEventsR,
         hibernate,
         timeoutsR,
         postpone,
         stateCall,
         result
       ) do
    case result do
      {:next_state, ^state, newData} ->
        loop_actions(
          p,
          debug,
          s,
          q,
          {state, newData},
          nextEventsR,
          hibernate,
          timeoutsR,
          postpone,
          false
        )

      {:next_state, nextState, newData} when stateCall ->
        loop_actions(
          p,
          debug,
          s,
          q,
          {nextState, newData},
          nextEventsR,
          hibernate,
          timeoutsR,
          postpone,
          true
        )

      {:next_state, _NextState, _NewData} ->
        terminate(
          :error,
          {:bad_state_enter_return_from_state_function, result},
          :erlang.element(
            2,
            :erlang.process_info(
              self(),
              :current_stacktrace
            )
          ),
          p,
          debug,
          r_state(s, state_data: state_Data, hibernate: hibernate),
          q
        )

      {:next_state, ^state, newData, actions} ->
        loop_actions(
          p,
          debug,
          s,
          q,
          {state, newData},
          nextEventsR,
          hibernate,
          timeoutsR,
          postpone,
          false,
          stateCall,
          actions
        )

      {:next_state, nextState, newData, actions}
      when stateCall ->
        loop_actions(
          p,
          debug,
          s,
          q,
          {nextState, newData},
          nextEventsR,
          hibernate,
          timeoutsR,
          postpone,
          true,
          stateCall,
          actions
        )

      {:next_state, _NextState, _NewData, _Actions} ->
        terminate(
          :error,
          {:bad_state_enter_return_from_state_function, result},
          :erlang.element(
            2,
            :erlang.process_info(
              self(),
              :current_stacktrace
            )
          ),
          p,
          debug,
          r_state(s, state_data: state_Data, hibernate: hibernate),
          q
        )

      {:keep_state, newData} ->
        loop_actions(
          p,
          debug,
          s,
          q,
          {state, newData},
          nextEventsR,
          hibernate,
          timeoutsR,
          postpone,
          false
        )

      {:keep_state, newData, actions} ->
        loop_actions(
          p,
          debug,
          s,
          q,
          {state, newData},
          nextEventsR,
          hibernate,
          timeoutsR,
          postpone,
          false,
          stateCall,
          actions
        )

      :keep_state_and_data ->
        loop_actions(
          p,
          debug,
          s,
          q,
          state_Data,
          nextEventsR,
          hibernate,
          timeoutsR,
          postpone,
          false
        )

      {:keep_state_and_data, actions} ->
        loop_actions(
          p,
          debug,
          s,
          q,
          state_Data,
          nextEventsR,
          hibernate,
          timeoutsR,
          postpone,
          false,
          stateCall,
          actions
        )

      {:repeat_state, newData} ->
        loop_actions(
          p,
          debug,
          s,
          q,
          {state, newData},
          nextEventsR,
          hibernate,
          timeoutsR,
          postpone,
          true
        )

      {:repeat_state, newData, actions} ->
        loop_actions(
          p,
          debug,
          s,
          q,
          {state, newData},
          nextEventsR,
          hibernate,
          timeoutsR,
          postpone,
          true,
          stateCall,
          actions
        )

      :repeat_state_and_data ->
        loop_actions(
          p,
          debug,
          s,
          q,
          state_Data,
          nextEventsR,
          hibernate,
          timeoutsR,
          postpone,
          true
        )

      {:repeat_state_and_data, actions} ->
        loop_actions(
          p,
          debug,
          s,
          q,
          state_Data,
          nextEventsR,
          hibernate,
          timeoutsR,
          postpone,
          true,
          stateCall,
          actions
        )

      :stop ->
        terminate(
          :exit,
          :normal,
          :erlang.element(
            2,
            :erlang.process_info(
              self(),
              :current_stacktrace
            )
          ),
          p,
          debug,
          r_state(s, state_data: state_Data, hibernate: hibernate),
          q
        )

      {:stop, reason} ->
        terminate(
          :exit,
          reason,
          :erlang.element(
            2,
            :erlang.process_info(
              self(),
              :current_stacktrace
            )
          ),
          p,
          debug,
          r_state(s, state_data: state_Data, hibernate: hibernate),
          q
        )

      {:stop, reason, newData} ->
        terminate(
          :exit,
          reason,
          :erlang.element(
            2,
            :erlang.process_info(
              self(),
              :current_stacktrace
            )
          ),
          p,
          debug,
          r_state(s,
            state_data: {state, newData},
            hibernate: hibernate
          ),
          q
        )

      {:stop_and_reply, reason, replies} ->
        reply_then_terminate(
          :exit,
          reason,
          :erlang.element(
            2,
            :erlang.process_info(
              self(),
              :current_stacktrace
            )
          ),
          p,
          debug,
          r_state(s,
            state_data: state_Data,
            hibernate: hibernate
          ),
          q,
          replies
        )

      {:stop_and_reply, reason, replies, newData} ->
        reply_then_terminate(
          :exit,
          reason,
          :erlang.element(
            2,
            :erlang.process_info(
              self(),
              :current_stacktrace
            )
          ),
          p,
          debug,
          r_state(s,
            state_data: {state, newData},
            hibernate: hibernate
          ),
          q,
          replies
        )

      _ ->
        terminate(
          :error,
          {:bad_return_from_state_function, result},
          :erlang.element(
            2,
            :erlang.process_info(
              self(),
              :current_stacktrace
            )
          ),
          p,
          debug,
          r_state(s, state_data: state_Data, hibernate: hibernate),
          q
        )
    end
  end

  defp loop_actions(
         p,
         debug,
         s,
         q,
         nextState_NewData,
         nextEventsR,
         hibernate,
         timeoutsR,
         postpone,
         callEnter,
         _StateCall,
         []
       ) do
    loop_actions(
      p,
      debug,
      s,
      q,
      nextState_NewData,
      nextEventsR,
      hibernate,
      timeoutsR,
      postpone,
      callEnter
    )
  end

  defp loop_actions(
         p,
         debug,
         s,
         q,
         nextState_NewData,
         nextEventsR,
         hibernate,
         timeoutsR,
         postpone,
         callEnter,
         stateCall,
         actions
       ) do
    loop_actions_list(
      p,
      debug,
      s,
      q,
      nextState_NewData,
      nextEventsR,
      hibernate,
      timeoutsR,
      postpone,
      callEnter,
      stateCall,
      listify(actions)
    )
  end

  defp loop_actions(
         p,
         debug,
         s,
         q,
         nextState_NewData,
         nextEventsR,
         hibernate,
         timeoutsR,
         postpone,
         callEnter
       ) do
    case callEnter and r_params(p, :state_enter) do
      true ->
        loop_state_enter(
          p,
          debug,
          s,
          q,
          nextState_NewData,
          nextEventsR,
          hibernate,
          timeoutsR,
          postpone
        )

      false ->
        loop_state_transition(
          p,
          debug,
          s,
          q,
          nextState_NewData,
          nextEventsR,
          hibernate,
          timeoutsR,
          postpone
        )
    end
  end

  defp loop_actions_list(
         p,
         debug,
         s,
         q,
         nextState_NewData,
         nextEventsR,
         hibernate,
         timeoutsR,
         postpone,
         callEnter,
         _StateCall,
         []
       ) do
    case r_params(p, :state_enter) do
      true when callEnter ->
        loop_state_enter(
          p,
          debug,
          s,
          q,
          nextState_NewData,
          nextEventsR,
          hibernate,
          timeoutsR,
          postpone
        )

      _ ->
        loop_state_transition(
          p,
          debug,
          s,
          q,
          nextState_NewData,
          nextEventsR,
          hibernate,
          timeoutsR,
          postpone
        )
    end
  end

  defp loop_actions_list(
         p,
         debug,
         s,
         q,
         nextState_NewData,
         nextEventsR,
         hibernate,
         timeoutsR,
         postpone,
         callEnter,
         stateCall,
         [action | actions]
       ) do
    case action do
      {:reply, from, reply} ->
        loop_actions_reply(
          p,
          debug,
          s,
          q,
          nextState_NewData,
          nextEventsR,
          hibernate,
          timeoutsR,
          postpone,
          callEnter,
          stateCall,
          actions,
          from,
          reply
        )

      {:hibernate, hibernate_1} when is_boolean(hibernate_1) ->
        loop_actions_list(
          p,
          debug,
          s,
          q,
          nextState_NewData,
          nextEventsR,
          hibernate_1,
          timeoutsR,
          postpone,
          callEnter,
          stateCall,
          actions
        )

      :hibernate ->
        loop_actions_list(
          p,
          debug,
          s,
          q,
          nextState_NewData,
          nextEventsR,
          true,
          timeoutsR,
          postpone,
          callEnter,
          stateCall,
          actions
        )

      {:postpone, postpone_1} when not postpone_1 or stateCall ->
        loop_actions_list(
          p,
          debug,
          s,
          q,
          nextState_NewData,
          nextEventsR,
          hibernate,
          timeoutsR,
          postpone_1,
          callEnter,
          stateCall,
          actions
        )

      :postpone when stateCall ->
        loop_actions_list(
          p,
          debug,
          s,
          q,
          nextState_NewData,
          nextEventsR,
          hibernate,
          timeoutsR,
          true,
          callEnter,
          stateCall,
          actions
        )

      :postpone ->
        terminate(
          :error,
          {:bad_state_enter_action_from_state_function, action},
          :erlang.element(
            2,
            :erlang.process_info(
              self(),
              :current_stacktrace
            )
          ),
          p,
          debug,
          r_state(s,
            state_data: nextState_NewData,
            hibernate: hibernate
          ),
          q
        )

      {:next_event, type, content} ->
        loop_actions_next_event(
          p,
          debug,
          s,
          q,
          nextState_NewData,
          nextEventsR,
          hibernate,
          timeoutsR,
          postpone,
          callEnter,
          stateCall,
          actions,
          type,
          content
        )

      {tag, newModule}
      when (tag === :change_callback_module and
              is_atom(newModule)) or
             (tag === :push_callback_module and is_atom(newModule)) ->
        cond do
          stateCall ->
            newModules =
              case tag do
                :change_callback_module ->
                  [newModule | tl(r_params(p, :modules))]

                :push_callback_module ->
                  [newModule | r_params(p, :modules)]
              end

            p_1 =
              r_params(p,
                callback_mode: :undefined,
                modules: newModules
              )

            loop_actions_list(
              p_1,
              debug,
              s,
              q,
              nextState_NewData,
              nextEventsR,
              hibernate,
              timeoutsR,
              postpone,
              callEnter,
              stateCall,
              actions
            )

          true ->
            terminate(
              :error,
              {:bad_state_enter_action_from_state_function, action},
              :erlang.element(
                2,
                :erlang.process_info(
                  self(),
                  :current_stacktrace
                )
              ),
              p,
              debug,
              r_state(s,
                state_data: nextState_NewData,
                hibernate: hibernate
              ),
              q
            )
        end

      :pop_callback_module when tl(r_params(p, :modules)) !== [] ->
        cond do
          stateCall ->
            newModules = tl(r_params(p, :modules))

            p_1 =
              r_params(p,
                callback_mode: :undefined,
                modules: newModules
              )

            loop_actions_list(
              p_1,
              debug,
              s,
              q,
              nextState_NewData,
              nextEventsR,
              hibernate,
              timeoutsR,
              postpone,
              callEnter,
              stateCall,
              actions
            )

          true ->
            terminate(
              :error,
              {:bad_state_enter_action_from_state_function, action},
              :erlang.element(
                2,
                :erlang.process_info(
                  self(),
                  :current_stacktrace
                )
              ),
              p,
              debug,
              r_state(s,
                state_data: nextState_NewData,
                hibernate: hibernate
              ),
              q
            )
        end

      _ ->
        loop_actions_list(
          p,
          debug,
          s,
          q,
          nextState_NewData,
          nextEventsR,
          hibernate,
          timeoutsR,
          postpone,
          callEnter,
          stateCall,
          actions,
          action
        )
    end
  end

  defp loop_actions_list(
         p,
         debug,
         s,
         q,
         nextState_NewData,
         nextEventsR,
         hibernate,
         timeoutsR,
         postpone,
         callEnter,
         stateCall,
         actions,
         {timeoutType, time, timeoutMsg, timeoutOpts} = timeout
       ) do
    case timeout_event_type(timeoutType) do
      true ->
        case listify(timeoutOpts) do
          [{:abs, true}]
          when is_integer(time) or time === :infinity ->
            loop_actions_list(
              p,
              debug,
              s,
              q,
              nextState_NewData,
              nextEventsR,
              hibernate,
              [timeout | timeoutsR],
              postpone,
              callEnter,
              stateCall,
              actions
            )

          [{:abs, false}]
          when (is_integer(time) and 0 <= time) or time === :infinity ->
            relativeTimeout = {timeoutType, time, timeoutMsg}

            loop_actions_list(
              p,
              debug,
              s,
              q,
              nextState_NewData,
              nextEventsR,
              hibernate,
              [relativeTimeout | timeoutsR],
              postpone,
              callEnter,
              stateCall,
              actions
            )

          []
          when (is_integer(time) and 0 <= time) or time === :infinity ->
            relativeTimeout = {timeoutType, time, timeoutMsg}

            loop_actions_list(
              p,
              debug,
              s,
              q,
              nextState_NewData,
              nextEventsR,
              hibernate,
              [relativeTimeout | timeoutsR],
              postpone,
              callEnter,
              stateCall,
              actions
            )

          timeoutOptsList ->
            case parse_timeout_opts_abs(timeoutOptsList) do
              true when is_integer(time) or time === :infinity ->
                loop_actions_list(
                  p,
                  debug,
                  s,
                  q,
                  nextState_NewData,
                  nextEventsR,
                  hibernate,
                  [timeout | timeoutsR],
                  postpone,
                  callEnter,
                  stateCall,
                  actions
                )

              false
              when (is_integer(time) and 0 <= time) or time === :infinity ->
                relativeTimeout = {timeoutType, time, timeoutMsg}

                loop_actions_list(
                  p,
                  debug,
                  s,
                  q,
                  nextState_NewData,
                  nextEventsR,
                  hibernate,
                  [relativeTimeout | timeoutsR],
                  postpone,
                  callEnter,
                  stateCall,
                  actions
                )

              _ ->
                terminate(
                  :error,
                  {:bad_action_from_state_function, timeout},
                  :erlang.element(
                    2,
                    :erlang.process_info(
                      self(),
                      :current_stacktrace
                    )
                  ),
                  p,
                  debug,
                  r_state(s,
                    state_data: nextState_NewData,
                    hibernate: hibernate
                  ),
                  q
                )
            end
        end

      false ->
        terminate(
          :error,
          {:bad_action_from_state_function, timeout},
          :erlang.element(
            2,
            :erlang.process_info(
              self(),
              :current_stacktrace
            )
          ),
          p,
          debug,
          r_state(s,
            state_data: nextState_NewData,
            hibernate: hibernate
          ),
          q
        )
    end
  end

  defp loop_actions_list(
         p,
         debug,
         s,
         q,
         nextState_NewData,
         nextEventsR,
         hibernate,
         timeoutsR,
         postpone,
         callEnter,
         stateCall,
         actions,
         {timeoutType, time, _TimeoutMsg} = timeout
       ) do
    case timeout_event_type(timeoutType) do
      true
      when (is_integer(time) and 0 <= time) or time === :infinity or
             time === :update ->
        loop_actions_list(
          p,
          debug,
          s,
          q,
          nextState_NewData,
          nextEventsR,
          hibernate,
          [timeout | timeoutsR],
          postpone,
          callEnter,
          stateCall,
          actions
        )

      _ ->
        terminate(
          :error,
          {:bad_action_from_state_function, timeout},
          :erlang.element(
            2,
            :erlang.process_info(
              self(),
              :current_stacktrace
            )
          ),
          p,
          debug,
          r_state(s,
            state_data: nextState_NewData,
            hibernate: hibernate
          ),
          q
        )
    end
  end

  defp loop_actions_list(
         p,
         debug,
         s,
         q,
         nextState_NewData,
         nextEventsR,
         hibernate,
         timeoutsR,
         postpone,
         callEnter,
         stateCall,
         actions,
         {timeoutType, :cancel} = action
       ) do
    case timeout_event_type(timeoutType) do
      true ->
        timeout = {timeoutType, :infinity, :undefined}

        loop_actions_list(
          p,
          debug,
          s,
          q,
          nextState_NewData,
          nextEventsR,
          hibernate,
          [timeout | timeoutsR],
          postpone,
          callEnter,
          stateCall,
          actions
        )

      false ->
        terminate(
          :error,
          {:bad_action_from_state_function, action},
          :erlang.element(
            2,
            :erlang.process_info(
              self(),
              :current_stacktrace
            )
          ),
          p,
          debug,
          r_state(s,
            state_data: nextState_NewData,
            hibernate: hibernate
          ),
          q
        )
    end
  end

  defp loop_actions_list(
         p,
         debug,
         s,
         q,
         nextState_NewData,
         nextEventsR,
         hibernate,
         timeoutsR,
         postpone,
         callEnter,
         stateCall,
         actions,
         time
       ) do
    cond do
      (is_integer(time) and 0 <= time) or time === :infinity ->
        timeout = {:timeout, time, time}

        loop_actions_list(
          p,
          debug,
          s,
          q,
          nextState_NewData,
          nextEventsR,
          hibernate,
          [timeout | timeoutsR],
          postpone,
          callEnter,
          stateCall,
          actions
        )

      true ->
        terminate(
          :error,
          {:bad_action_from_state_function, time},
          :erlang.element(
            2,
            :erlang.process_info(
              self(),
              :current_stacktrace
            )
          ),
          p,
          debug,
          r_state(s,
            state_data: nextState_NewData,
            hibernate: hibernate
          ),
          q
        )
    end
  end

  defp loop_actions_reply(
         p,
         debug,
         s,
         q,
         nextState_NewData,
         nextEventsR,
         hibernate,
         timeoutsR,
         postpone,
         callEnter,
         stateCall,
         actions,
         from,
         reply
       ) do
    case from(from) do
      true ->
        reply(from, reply)

        debug_1 =
          case debug do
            [] ->
              debug

            _ ->
              sys_debug(
                debug,
                r_params(p, :name),
                {:out, reply, from}
              )
          end

        loop_actions_list(
          p,
          debug_1,
          s,
          q,
          nextState_NewData,
          nextEventsR,
          hibernate,
          timeoutsR,
          postpone,
          callEnter,
          stateCall,
          actions
        )

      false ->
        terminate(
          :error,
          {:bad_action_from_state_function, {:reply, from, reply}},
          :erlang.element(
            2,
            :erlang.process_info(
              self(),
              :current_stacktrace
            )
          ),
          p,
          debug,
          r_state(s,
            state_data: nextState_NewData,
            hibernate: hibernate
          ),
          q
        )
    end
  end

  defp loop_actions_next_event(
         p,
         debug,
         s,
         q,
         nextState_NewData,
         nextEventsR,
         hibernate,
         timeoutsR,
         postpone,
         callEnter,
         stateCall,
         actions,
         type,
         content
       ) do
    case event_type(type) do
      true when stateCall ->
        nextEvent = {type, content}

        case debug do
          [] ->
            loop_actions_list(
              p,
              debug,
              s,
              q,
              nextState_NewData,
              [nextEvent | nextEventsR],
              hibernate,
              timeoutsR,
              postpone,
              callEnter,
              stateCall,
              actions
            )

          _ ->
            name = r_params(p, :name)
            {state, _Data} = r_state(s, :state_data)
            debug_1 = sys_debug(debug, name, {:in, {type, content}, state})

            loop_actions_list(
              p,
              debug_1,
              s,
              q,
              nextState_NewData,
              [nextEvent | nextEventsR],
              hibernate,
              timeoutsR,
              postpone,
              callEnter,
              stateCall,
              actions
            )
        end

      _ ->
        terminate(
          :error,
          {cond do
             stateCall ->
               :bad_action_from_state_function

             true ->
               :bad_state_enter_action_from_state_function
           end, {:next_event, type, content}},
          :erlang.element(
            2,
            :erlang.process_info(
              self(),
              :current_stacktrace
            )
          ),
          p,
          debug,
          r_state(s,
            state_data: nextState_NewData,
            hibernate: hibernate
          ),
          q
        )
    end
  end

  defp loop_state_transition(
         p,
         debug,
         r_state(state_data: {state, _Data}, postponed: postponed) = s,
         [event | events],
         {nextState, _NewData} = nextState_NewData,
         nextEventsR,
         hibernate,
         timeoutsR,
         postpone
       ) do
    postponed_1 =
      case postpone do
        true ->
          [event | postponed]

        false ->
          postponed
      end

    case debug do
      [] ->
        cond do
          nextState === state ->
            loop_keep_state(
              p,
              debug,
              s,
              events,
              nextState_NewData,
              nextEventsR,
              hibernate,
              timeoutsR,
              postponed_1
            )

          true ->
            loop_state_change(
              p,
              debug,
              s,
              events,
              nextState_NewData,
              nextEventsR,
              hibernate,
              timeoutsR,
              postponed_1
            )
        end

      _ ->
        name = r_params(p, :name)

        debug_1 =
          case postpone do
            true ->
              sys_debug(debug, name, {:postpone, event, state, nextState})

            false ->
              sys_debug(debug, name, {:consume, event, state, nextState})
          end

        cond do
          nextState === state ->
            loop_keep_state(
              p,
              debug_1,
              s,
              events,
              nextState_NewData,
              nextEventsR,
              hibernate,
              timeoutsR,
              postponed_1
            )

          true ->
            loop_state_change(
              p,
              debug_1,
              s,
              events,
              nextState_NewData,
              nextEventsR,
              hibernate,
              timeoutsR,
              postponed_1
            )
        end
    end
  end

  defp loop_keep_state(
         p,
         debug,
         r_state(timers: timers) = s,
         events,
         nextState_NewData,
         nextEventsR,
         hibernate,
         timeoutsR,
         postponed
       ) do
    case timers do
      %{:timeout => {timerRef, _TimeoutMsg}} ->
        loop_next_events(
          p,
          debug,
          s,
          events,
          nextState_NewData,
          nextEventsR,
          hibernate,
          timeoutsR,
          postponed,
          cancel_timer(:timeout, timerRef, timers)
        )

      _ ->
        loop_next_events(
          p,
          debug,
          s,
          events,
          nextState_NewData,
          nextEventsR,
          hibernate,
          timeoutsR,
          postponed,
          timers
        )
    end
  end

  defp loop_state_change(
         p,
         debug,
         s,
         events,
         nextState_NewData,
         nextEventsR,
         hibernate,
         timeoutsR,
         postponed
       ) do
    case postponed do
      [] ->
        loop_state_change(
          p,
          debug,
          s,
          events,
          nextState_NewData,
          nextEventsR,
          hibernate,
          timeoutsR
        )

      [e1] ->
        loop_state_change(
          p,
          debug,
          s,
          [e1 | events],
          nextState_NewData,
          nextEventsR,
          hibernate,
          timeoutsR
        )

      [e2, e1] ->
        loop_state_change(
          p,
          debug,
          s,
          [[e1, e2] | events],
          nextState_NewData,
          nextEventsR,
          hibernate,
          timeoutsR
        )

      [[_, _] | _] ->
        loop_state_change(
          p,
          debug,
          s,
          :lists.reverse(postponed, events),
          nextState_NewData,
          nextEventsR,
          hibernate,
          timeoutsR
        )
    end
  end

  defp loop_state_change(
         p,
         debug,
         r_state(timers: timers) = s,
         events,
         nextState_NewData,
         nextEventsR,
         hibernate,
         timeoutsR
       ) do
    case timers do
      %{:state_timeout => {timerRef, _TimeoutMsg}} ->
        loop_next_events(
          p,
          debug,
          s,
          events,
          nextState_NewData,
          nextEventsR,
          hibernate,
          timeoutsR,
          [],
          cancel_timer(
            :timeout,
            cancel_timer(:state_timeout, timerRef, timers)
          )
        )

      %{:timeout => {timerRef, _TimeoutMsg}} ->
        loop_next_events(
          p,
          debug,
          s,
          events,
          nextState_NewData,
          nextEventsR,
          hibernate,
          timeoutsR,
          [],
          cancel_timer(:timeout, timerRef, timers)
        )

      _ ->
        loop_next_events(
          p,
          debug,
          s,
          events,
          nextState_NewData,
          nextEventsR,
          hibernate,
          timeoutsR,
          [],
          timers
        )
    end
  end

  defp loop_next_events(
         p,
         debug,
         s,
         events,
         nextState_NewData,
         nextEventsR,
         hibernate,
         [],
         postponed,
         timers
       ) do
    loop_done(
      p,
      debug,
      r_state(s,
        state_data: nextState_NewData,
        postponed: postponed,
        timers: timers,
        hibernate: hibernate
      ),
      events,
      nextEventsR
    )
  end

  defp loop_next_events(
         p,
         debug,
         s,
         events,
         nextState_NewData,
         nextEventsR,
         hibernate,
         timeoutsR,
         postponed,
         timers
       ) do
    seen = %{}
    timeoutEvents = []

    loop_timeouts(
      p,
      debug,
      s,
      events,
      nextState_NewData,
      nextEventsR,
      hibernate,
      timeoutsR,
      postponed,
      timers,
      seen,
      timeoutEvents
    )
  end

  defp loop_timeouts(
         p,
         debug,
         s,
         events,
         nextState_NewData,
         nextEventsR,
         hibernate,
         [],
         postponed,
         timers,
         _Seen,
         timeoutEvents
       ) do
    case timeoutEvents do
      [] ->
        s_1 =
          r_state(s,
            state_data: nextState_NewData,
            postponed: postponed,
            timers: timers,
            hibernate: hibernate
          )

        loop_done(p, debug, s_1, events, nextEventsR)

      [_ | _] ->
        %{:t0q => t0Q} = timers

        s_1 =
          r_state(s,
            state_data: nextState_NewData,
            postponed: postponed,
            timers: %{timers | :t0q => t0Q ++ timeoutEvents},
            hibernate: hibernate
          )

        loop_done(p, debug, s_1, events, nextEventsR)
    end
  end

  defp loop_timeouts(
         p,
         debug,
         s,
         events,
         nextState_NewData,
         nextEventsR,
         hibernate,
         [timeout | timeoutsR],
         postponed,
         timers,
         seen,
         timeoutEvents
       ) do
    timeoutType = :erlang.element(1, timeout)

    case seen do
      %{^timeoutType => _} ->
        loop_timeouts(
          p,
          debug,
          s,
          events,
          nextState_NewData,
          nextEventsR,
          hibernate,
          timeoutsR,
          postponed,
          timers,
          seen,
          timeoutEvents
        )

      %{} ->
        case timeout do
          {_, time, timeoutMsg} ->
            loop_timeouts_start(
              p,
              debug,
              s,
              events,
              nextState_NewData,
              nextEventsR,
              hibernate,
              timeoutsR,
              postponed,
              timers,
              seen,
              timeoutEvents,
              timeoutType,
              time,
              timeoutMsg,
              []
            )

          {_, time, timeoutMsg, timeoutOpts} ->
            loop_timeouts_start(
              p,
              debug,
              s,
              events,
              nextState_NewData,
              nextEventsR,
              hibernate,
              timeoutsR,
              postponed,
              timers,
              seen,
              timeoutEvents,
              timeoutType,
              time,
              timeoutMsg,
              listify(timeoutOpts)
            )
        end
    end
  end

  defp loop_timeouts_start(
         p,
         debug,
         s,
         events,
         nextState_NewData,
         nextEventsR,
         hibernate,
         timeoutsR,
         postponed,
         timers,
         seen,
         timeoutEvents,
         timeoutType,
         time,
         timeoutMsg,
         timeoutOpts
       ) do
    case time do
      :infinity ->
        loop_timeouts_cancel(
          p,
          debug,
          s,
          events,
          nextState_NewData,
          nextEventsR,
          hibernate,
          timeoutsR,
          postponed,
          timers,
          seen,
          timeoutEvents,
          timeoutType
        )

      :update ->
        loop_timeouts_update(
          p,
          debug,
          s,
          events,
          nextState_NewData,
          nextEventsR,
          hibernate,
          timeoutsR,
          postponed,
          timers,
          seen,
          timeoutEvents,
          timeoutType,
          timeoutMsg
        )

      0 ->
        timerRef = 0
        timeoutEvents_1 = [timeoutType | timeoutEvents]

        loop_timeouts_register(
          p,
          debug,
          s,
          events,
          nextState_NewData,
          nextEventsR,
          hibernate,
          timeoutsR,
          postponed,
          timers,
          seen,
          timeoutEvents_1,
          timeoutType,
          time,
          timeoutMsg,
          timeoutOpts,
          timerRef
        )

      _ ->
        timerRef = :erlang.start_timer(time, self(), timeoutType, timeoutOpts)

        loop_timeouts_register(
          p,
          debug,
          s,
          events,
          nextState_NewData,
          nextEventsR,
          hibernate,
          timeoutsR,
          postponed,
          timers,
          seen,
          timeoutEvents,
          timeoutType,
          time,
          timeoutMsg,
          timeoutOpts,
          timerRef
        )
    end
  end

  defp loop_timeouts_register(
         p,
         debug,
         s,
         events,
         nextState_NewData,
         nextEventsR,
         hibernate,
         timeoutsR,
         postponed,
         timers,
         seen,
         timeoutEvents,
         timeoutType,
         time,
         timeoutMsg,
         timeoutOpts,
         timerRef
       ) do
    case debug do
      [] ->
        loop_timeouts_register(
          p,
          debug,
          s,
          events,
          nextState_NewData,
          nextEventsR,
          hibernate,
          timeoutsR,
          postponed,
          timers,
          seen,
          timeoutEvents,
          timeoutType,
          timerRef,
          timeoutMsg
        )

      _ ->
        {state, _Data} = nextState_NewData

        debug_1 =
          sys_debug(
            debug,
            r_params(p, :name),
            {:start_timer, {timeoutType, time, timeoutMsg, timeoutOpts}, state}
          )

        loop_timeouts_register(
          p,
          debug_1,
          s,
          events,
          nextState_NewData,
          nextEventsR,
          hibernate,
          timeoutsR,
          postponed,
          timers,
          seen,
          timeoutEvents,
          timeoutType,
          timerRef,
          timeoutMsg
        )
    end
  end

  defp loop_timeouts_register(
         p,
         debug,
         s,
         events,
         nextState_NewData,
         nextEventsR,
         hibernate,
         timeoutsR,
         postponed,
         timers,
         seen,
         timeoutEvents,
         timeoutType,
         timerRef,
         timeoutMsg
       ) do
    case timers do
      %{^timeoutType => {0, _OldTimeoutMsg}, :t0q => t0Q} ->
        timers_1 = %{
          timers
          | timeoutType => {0, timeoutMsg},
            :t0q => :lists.delete(timeoutType, t0Q)
        }

        loop_timeouts(
          p,
          debug,
          s,
          events,
          nextState_NewData,
          nextEventsR,
          hibernate,
          timeoutsR,
          postponed,
          timers_1,
          %{seen | timeoutType => true},
          timeoutEvents
        )

      %{^timeoutType => {oldTimerRef, _OldTimeoutMsg}} ->
        cancel_timer(oldTimerRef)
        timers_1 = %{timers | timeoutType => {timerRef, timeoutMsg}}

        loop_timeouts(
          p,
          debug,
          s,
          events,
          nextState_NewData,
          nextEventsR,
          hibernate,
          timeoutsR,
          postponed,
          timers_1,
          %{seen | timeoutType => true},
          timeoutEvents
        )

      %{} ->
        timers_1 = %{timers | timeoutType => {timerRef, timeoutMsg}}

        loop_timeouts(
          p,
          debug,
          s,
          events,
          nextState_NewData,
          nextEventsR,
          hibernate,
          timeoutsR,
          postponed,
          timers_1,
          %{seen | timeoutType => true},
          timeoutEvents
        )
    end
  end

  defp loop_timeouts_cancel(
         p,
         debug,
         s,
         events,
         nextState_NewData,
         nextEventsR,
         hibernate,
         timeoutsR,
         postponed,
         timers,
         seen,
         timeoutEvents,
         timeoutType
       ) do
    case timers do
      %{^timeoutType => {timerRef, _TimeoutMsg}} ->
        timers_1 = cancel_timer(timeoutType, timerRef, timers)

        loop_timeouts(
          p,
          debug,
          s,
          events,
          nextState_NewData,
          nextEventsR,
          hibernate,
          timeoutsR,
          postponed,
          timers_1,
          %{seen | timeoutType => true},
          timeoutEvents
        )

      %{} ->
        loop_timeouts(
          p,
          debug,
          s,
          events,
          nextState_NewData,
          nextEventsR,
          hibernate,
          timeoutsR,
          postponed,
          timers,
          %{seen | timeoutType => true},
          timeoutEvents
        )
    end
  end

  defp loop_timeouts_update(
         p,
         debug,
         s,
         events,
         nextState_NewData,
         nextEventsR,
         hibernate,
         timeoutsR,
         postponed,
         timers,
         seen,
         timeoutEvents,
         timeoutType,
         timeoutMsg
       ) do
    case timers do
      %{^timeoutType => {timerRef, _OldTimeoutMsg}} ->
        timers_1 = %{timers | timeoutType => {timerRef, timeoutMsg}}

        loop_timeouts(
          p,
          debug,
          s,
          events,
          nextState_NewData,
          nextEventsR,
          hibernate,
          timeoutsR,
          postponed,
          timers_1,
          %{seen | timeoutType => true},
          timeoutEvents
        )

      %{} ->
        timers_1 = %{timers | timeoutType => {0, timeoutMsg}}
        timeoutEvents_1 = [timeoutType | timeoutEvents]

        loop_timeouts(
          p,
          debug,
          s,
          events,
          nextState_NewData,
          nextEventsR,
          hibernate,
          timeoutsR,
          postponed,
          timers_1,
          %{seen | timeoutType => true},
          timeoutEvents_1
        )
    end
  end

  defp loop_done(p, debug, s, events, nextEventsR) do
    case nextEventsR do
      [] ->
        loop_done(p, debug, s, events)

      [e1] ->
        loop_done(p, debug, s, [e1 | events])

      [e2, e1] ->
        loop_done(p, debug, s, [[e1, e2] | events])

      [[_, _] | _] ->
        loop_done(p, debug, s, :lists.reverse(nextEventsR, events))
    end
  end

  defp loop_done(p, debug, s, q) do
    case q do
      [] ->
        case r_state(s, :timers) do
          %{:t0q => [timeoutType | _]} = timers ->
            %{^timeoutType => {0 = timerRef, timeoutMsg}} = timers
            timers_1 = cancel_timer(timeoutType, timerRef, timers)
            s_1 = r_state(s, timers: timers_1)
            event = {timeoutType, timeoutMsg}
            loop_receive_result(p, debug, s_1, event)

          %{} ->
            loop(p, debug, s)
        end

      [event | events] ->
        loop_event(p, debug, s, event, events)
    end
  end

  defp parse_timeout_opts_abs(opts) do
    parse_timeout_opts_abs(opts, false)
  end

  defp parse_timeout_opts_abs(opts, abs) do
    case opts do
      [] ->
        abs

      [{:abs, abs_1} | ^opts] when is_boolean(abs_1) ->
        parse_timeout_opts_abs(opts, abs_1)

      _ ->
        :badarg
    end
  end

  defp reply_then_terminate(class, reason, stacktrace, p, debug, s, q, replies) do
    do_reply_then_terminate(class, reason, stacktrace, p, debug, s, q, listify(replies))
  end

  defp do_reply_then_terminate(class, reason, stacktrace, p, debug, s, q, []) do
    terminate(class, reason, stacktrace, p, debug, s, q)
  end

  defp do_reply_then_terminate(class, reason, stacktrace, p, debug, s, q, [r | rs]) do
    case r do
      {:reply, from, reply} ->
        case from(from) do
          true ->
            reply(from, reply)

            debug_1 =
              case debug do
                [] ->
                  debug

                _ ->
                  sys_debug(
                    debug,
                    r_params(p, :name),
                    {:out, reply, from}
                  )
              end

            do_reply_then_terminate(class, reason, stacktrace, p, debug_1, s, q, rs)

          false ->
            terminate(
              :error,
              {:bad_reply_action_from_state_function, r},
              :erlang.element(
                2,
                :erlang.process_info(
                  self(),
                  :current_stacktrace
                )
              ),
              p,
              debug,
              s,
              q
            )
        end

      _ ->
        terminate(
          :error,
          {:bad_reply_action_from_state_function, r},
          :erlang.element(
            2,
            :erlang.process_info(
              self(),
              :current_stacktrace
            )
          ),
          p,
          debug,
          s,
          q
        )
    end
  end

  defp terminate(
         class,
         reason,
         stacktrace,
         r_params(modules: [module | _]) = p,
         debug,
         r_state(state_data: {state, data}) = s,
         q
       ) do
    case :erlang.function_exported(module, :terminate, 3) do
      true ->
        try do
          module.terminate(reason, state, data)
        catch
          _ ->
            :ok

          c, r ->
            error_info(c, r, __STACKTRACE__, debug, p, s, q)
            :erlang.raise(c, r, __STACKTRACE__)
        else
          _ ->
            :ok
        end

      false ->
        :ok
    end

    _ =
      case reason do
        :normal ->
          terminate_sys_debug(debug, p, state, reason)

        :shutdown ->
          terminate_sys_debug(debug, p, state, reason)

        {:shutdown, _} ->
          terminate_sys_debug(debug, p, state, reason)

        _ ->
          error_info(class, reason, stacktrace, debug, p, s, q)
      end

    case stacktrace do
      [] ->
        apply(:erlang, class, [reason])

      [_ | _] ->
        :erlang.raise(class, reason, stacktrace)
    end
  end

  defp terminate_sys_debug(debug, p, state, reason) do
    case debug do
      [] ->
        debug

      _ ->
        sys_debug(
          debug,
          r_params(p, :name),
          {:terminate, reason, state}
        )
    end
  end

  defp error_info(
         class,
         reason,
         stacktrace,
         debug,
         r_params(
           name: name,
           modules: modules,
           callback_mode: callbackMode,
           state_enter: stateEnter
         ) = p,
         r_state(postponed: postponed, timers: timers) = s,
         q
       ) do
    log = :sys.get_log(debug)

    case :logger.allow(:error, :gen_statem) do
      true ->
        :erlang.apply(:logger, :macro_log, [
          %{
            :mfa => {:gen_statem, :error_info, 7},
            :line => 2382,
            :file => 'otp/lib/stdlib/src/gen_statem.erl'
          },
          :error,
          %{
            :label => {:gen_statem, :terminate},
            :name => name,
            :queue => q,
            :postponed => postponed,
            :modules => modules,
            :callback_mode => callbackMode,
            :state_enter => stateEnter,
            :state => format_status(:terminate, :erlang.get(), p, s),
            :timeouts => list_timeouts(timers),
            :log => log,
            :reason => {class, reason, stacktrace},
            :client_info => client_stacktrace(q)
          },
          %{
            :domain => [:otp],
            :report_cb => &:gen_statem.format_log/2,
            :error_logger => %{:tag => :error, :report_cb => &:gen_statem.format_log/1}
          }
        ])

      false ->
        :ok
    end
  end

  defp client_stacktrace([]) do
    :undefined
  end

  defp client_stacktrace([{{:call, {pid, _Tag}}, _Req} | _])
       when is_pid(pid) do
    cond do
      node(pid) === node() ->
        case :erlang.process_info(
               pid,
               [:current_stacktrace, :registered_name]
             ) do
          :undefined ->
            {pid, :dead}

          [{:current_stacktrace, stacktrace}, {:registered_name, []}] ->
            {pid, {pid, stacktrace}}

          [{:current_stacktrace, stacktrace}, {:registered_name, name}] ->
            {pid, {name, stacktrace}}
        end

      true ->
        {pid, :remote}
    end
  end

  defp client_stacktrace([_ | _]) do
    :undefined
  end

  def format_log(report) do
    depth = :error_logger.get_format_depth()

    formatOpts = %{
      :chars_limit => :unlimited,
      :depth => depth,
      :single_line => false,
      :encoding => :utf8
    }

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
           :label => {:gen_statem, :terminate},
           :queue => q,
           :postponed => postponed,
           :modules => modules,
           :state => fmtData,
           :timeouts => timeouts,
           :log => log,
           :reason => {class, reason, stacktrace},
           :client_info => clientInfo
         } = report,
         depth
       ) do
    %{
      report
      | :queue =>
          case q do
            [event | events] ->
              [
                :io_lib.limit_term(event, depth)
                | :io_lib.limit_term(events, depth)
              ]

            _ ->
              []
          end,
        :postponed =>
          case postponed do
            [] ->
              []

            _ ->
              :io_lib.limit_term(postponed, depth)
          end,
        :modules => :io_lib.limit_term(modules, depth),
        :state => :io_lib.limit_term(fmtData, depth),
        :timeouts =>
          case timeouts do
            {0, _} ->
              timeouts

            _ ->
              :io_lib.limit_term(timeouts, depth)
          end,
        :log =>
          case log do
            [] ->
              []

            _ ->
              for t <- log do
                :io_lib.limit_term(t, depth)
              end
          end,
        :reason =>
          {class, :io_lib.limit_term(reason, depth), :io_lib.limit_term(stacktrace, depth)},
        :client_info => limit_client_info(clientInfo, depth)
    }
  end

  defp limit_client_info({pid, {name, stacktrace}}, depth) do
    {pid, {name, :io_lib.limit_term(stacktrace, depth)}}
  end

  defp limit_client_info(client, _Depth) do
    client
  end

  def format_log(report, formatOpts0) do
    default = %{
      :chars_limit => :unlimited,
      :depth => :unlimited,
      :single_line => false,
      :encoding => :utf8
    }

    formatOpts = :maps.merge(default, formatOpts0)

    ioOpts =
      case formatOpts do
        %{:chars_limit => :unlimited} ->
          []

        %{:chars_limit => limit} ->
          [{:chars_limit, limit}]
      end

    {format, args} = format_log_single(report, formatOpts)
    :io_lib.format(format, args, ioOpts)
  end

  defp format_log_single(
         %{
           :label => {:gen_statem, :terminate},
           :name => name,
           :queue => q,
           :state => fmtData,
           :log => log,
           :reason => {class, reason, stacktrace},
           :client_info => clientInfo
         },
         %{:single_line => true, :depth => depth} = formatOpts
       ) do
    p = p(formatOpts)
    {fixedReason, fixedStacktrace} = fix_reason(class, reason, stacktrace)
    {clientFmt, clientArgs} = format_client_log_single(clientInfo, p, depth)

    format =
      :lists.append([
        'State machine ',
        p,
        ' terminating. Reason: ',
        p,
        case fixedStacktrace do
          [] ->
            ''

          _ ->
            '. Stack: ' ++ p
        end,
        case q do
          [] ->
            ''

          _ ->
            '. Last event: ' ++ p
        end,
        '. State: ',
        p,
        case log do
          [] ->
            ''

          _ ->
            '. Log: ' ++ p
        end,
        '.'
      ])

    args0 =
      [name, fixedReason] ++
        case fixedStacktrace do
          [] ->
            []

          _ ->
            [fixedStacktrace]
        end ++
        case q do
          [] ->
            []

          [event | _] ->
            [event]
        end ++
        [fmtData] ++
        case log do
          [] ->
            []

          _ ->
            [log]
        end

    args =
      case depth do
        :unlimited ->
          args0

        _ ->
          :lists.flatmap(
            fn a ->
              [a, depth]
            end,
            args0
          )
      end

    {format ++ clientFmt, args ++ clientArgs}
  end

  defp format_log_single(report, formatOpts) do
    format_log_multi(report, formatOpts)
  end

  defp format_log_multi(
         %{
           :label => {:gen_statem, :terminate},
           :name => name,
           :queue => q,
           :postponed => postponed,
           :modules => modules,
           :callback_mode => callbackMode,
           :state_enter => stateEnter,
           :state => fmtData,
           :timeouts => timeouts,
           :log => log,
           :reason => {class, reason, stacktrace},
           :client_info => clientInfo
         },
         %{:depth => depth} = formatOpts
       ) do
    p = p(formatOpts)
    {fixedReason, fixedStacktrace} = fix_reason(class, reason, stacktrace)
    {clientFmt, clientArgs} = format_client_log(clientInfo, p, depth)

    cBMode =
      case stateEnter do
        true ->
          [callbackMode, :state_enter]

        false ->
          callbackMode
      end

    format =
      :lists.append([
        '** State machine ',
        p,
        ' terminating~n',
        case q do
          [] ->
            ''

          _ ->
            '** Last event = ' ++ p ++ '~n'
        end,
        '** When server state  = ',
        p,
        '~n',
        '** Reason for termination = ',
        p,
        ':',
        p,
        '~n',
        '** Callback modules = ',
        p,
        '~n',
        '** Callback mode = ',
        p,
        '~n',
        case q do
          [[_, _] | _] ->
            '** Queued = ' ++ p ++ '~n'

          _ ->
            ''
        end,
        case postponed do
          [] ->
            ''

          _ ->
            '** Postponed = ' ++ p ++ '~n'
        end,
        case fixedStacktrace do
          [] ->
            ''

          _ ->
            '** Stacktrace =~n**  ' ++ p ++ '~n'
        end,
        case timeouts do
          {0, _} ->
            ''

          _ ->
            '** Time-outs: ' ++ p ++ '~n'
        end,
        case log do
          [] ->
            ''

          _ ->
            '** Log =~n**  ' ++ p ++ '~n'
        end
      ])

    args0 =
      [
        name
        | case q do
            [] ->
              []

            [event | _] ->
              [event]
          end
      ] ++
        [fmtData, class, fixedReason, modules, cBMode] ++
        case q do
          [_ | [_ | _] = events] ->
            [events]

          _ ->
            []
        end ++
        case postponed do
          [] ->
            []

          _ ->
            [postponed]
        end ++
        case fixedStacktrace do
          [] ->
            []

          _ ->
            [fixedStacktrace]
        end ++
        case timeouts do
          {0, _} ->
            []

          _ ->
            [timeouts]
        end ++
        case log do
          [] ->
            []

          _ ->
            [log]
        end

    args =
      case depth do
        :unlimited ->
          args0

        _ ->
          :lists.flatmap(
            fn a ->
              [a, depth]
            end,
            args0
          )
      end

    {format ++ clientFmt, args ++ clientArgs}
  end

  defp fix_reason(class, reason, stacktrace) do
    case stacktrace do
      [{m, f, args, _} | sT]
      when class === :error and
             reason === :undef ->
        case :code.is_loaded(m) do
          false ->
            {{:"module could not be loaded", m}, sT}

          _ ->
            arity =
              cond do
                is_list(args) ->
                  length(args)

                is_integer(args) ->
                  args
              end

            case :erlang.function_exported(m, f, arity) do
              true ->
                {reason, stacktrace}

              false ->
                {{:"function not exported", {m, f, arity}}, sT}
            end
        end

      _ ->
        {reason, stacktrace}
    end
  end

  defp format_client_log_single(:undefined, _, _) do
    {'', []}
  end

  defp format_client_log_single({pid, :dead}, _, _) do
    {' Client ~0p is dead.', [pid]}
  end

  defp format_client_log_single({pid, :remote}, _, _) do
    {' Client ~0p is remote on node ~0p.', [pid, node(pid)]}
  end

  defp format_client_log_single({_Pid, {name, stacktrace0}}, p, depth) do
    stacktrace = :lists.sublist(stacktrace0, 4)
    format = :lists.append([' Client ', p, ' stacktrace: ', p, '.'])

    args =
      case depth do
        :unlimited ->
          [name, stacktrace]

        _ ->
          [name, depth, stacktrace, depth]
      end

    {format, args}
  end

  defp format_client_log(:undefined, _, _) do
    {'', []}
  end

  defp format_client_log({pid, :dead}, _, _) do
    {'** Client ~p is dead~n', [pid]}
  end

  defp format_client_log({pid, :remote}, _, _) do
    {'** Client ~p is remote on node ~p~n', [pid, node(pid)]}
  end

  defp format_client_log({_Pid, {name, stacktrace}}, p, depth) do
    format = :lists.append(['** Client ', p, ' stacktrace~n** ', p, '~n'])

    args =
      case depth do
        :unlimited ->
          [name, stacktrace]

        _ ->
          [name, depth, stacktrace, depth]
      end

    {format, args}
  end

  defp p(%{:single_line => single, :depth => depth, :encoding => enc}) do
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

  defp format_status(
         opt,
         pDict,
         r_params(modules: [module | _]),
         r_state(state_data: {state, data} = state_Data)
       ) do
    case :erlang.function_exported(module, :format_status, 2) do
      true ->
        try do
          module.format_status(opt, [pDict, state, data])
        catch
          result ->
            result

          _, _ ->
            format_status_default(
              opt,
              {state, :erlang.atom_to_list(module) ++ ':format_status/2 crashed'}
            )
        end

      false ->
        format_status_default(opt, state_Data)
    end
  end

  defp format_status_default(opt, state_Data) do
    case opt do
      :terminate ->
        state_Data

      _ ->
        [{:data, [{'State', state_Data}]}]
    end
  end

  defp listify(item) when is_list(item) do
    item
  end

  defp listify(item) do
    [item]
  end

  defp cancel_timer(timerRef) do
    case :erlang.cancel_timer(timerRef) do
      false ->
        receive do
          {:timeout, ^timerRef, _} ->
            :ok
        end

      _ ->
        :ok
    end
  end

  defp cancel_timer(timeoutType, timerRef, timers) do
    case timerRef do
      0 ->
        :maps.remove(
          timeoutType,
          :maps.update(
            :t0q,
            :lists.delete(
              timeoutType,
              :maps.get(
                :t0q,
                timers
              )
            ),
            timers
          )
        )

      _ ->
        case :erlang.cancel_timer(timerRef) do
          false ->
            receive do
              {:timeout, ^timerRef, _} ->
                :ok
            end

          _ ->
            :ok
        end

        :maps.remove(
          timeoutType,
          timers
        )
    end
  end

  defp cancel_timer(timeoutType, timers) do
    case timers do
      %{^timeoutType => {timerRef, _TimeoutMsg}} ->
        case timerRef do
          0 ->
            :maps.remove(
              timeoutType,
              :maps.update(
                :t0q,
                :lists.delete(
                  timeoutType,
                  :maps.get(
                    :t0q,
                    timers
                  )
                ),
                timers
              )
            )

          _ ->
            case :erlang.cancel_timer(timerRef) do
              false ->
                receive do
                  {:timeout, ^timerRef, _} ->
                    :ok
                end

              _ ->
                :ok
            end

            :maps.remove(
              timeoutType,
              timers
            )
        end

      %{} ->
        timers
    end
  end

  defp list_timeouts(timers) do
    {:maps.size(timers) - 1,
     :maps.fold(
       fn
         :t0q, _, acc ->
           acc

         timeoutType, {_TimerRef, timeoutMsg}, acc ->
           [{timeoutType, timeoutMsg} | acc]
       end,
       [],
       timers
     )}
  end
end
