defmodule :m_logger_server do
  use Bitwise
  @behaviour :gen_server
  require Record

  Record.defrecord(:r_state, :state,
    tid: :undefined,
    async_req: :undefined,
    async_req_queue: :undefined,
    remote_logger: :undefined
  )

  def start_link() do
    :gen_server.start_link({:local, :logger}, :logger_server, [], [])
  end

  def add_handler(id, module, config0) do
    try do
      {check_id(id), check_mod(module)}
    catch
      error ->
        {:error, error}
    else
      {:ok, :ok} ->
        case sanity_check(id, config0) do
          :ok ->
            default = default_config(id, module)
            config = :maps.merge(default, config0)
            call({:add_handler, id, module, config})

          error ->
            error
        end
    end
  end

  def remove_handler(handlerId) do
    call({:remove_handler, handlerId})
  end

  def add_filter(owner, filter) do
    case sanity_check(owner, :filters, [filter]) do
      :ok ->
        call({:add_filter, owner, filter})

      error ->
        error
    end
  end

  def remove_filter(owner, filterId) do
    call({:remove_filter, owner, filterId})
  end

  def set_module_level(modules, level) when is_list(modules) do
    case :lists.all(
           fn m ->
             is_atom(m)
           end,
           modules
         ) do
      true ->
        case sanity_check(:primary, :level, level) do
          :ok ->
            call({:set_module_level, modules, level})

          error ->
            error
        end

      false ->
        {:error, {:not_a_list_of_modules, modules}}
    end
  end

  def set_module_level(modules, _) do
    {:error, {:not_a_list_of_modules, modules}}
  end

  def unset_module_level() do
    call({:unset_module_level, :all})
  end

  def unset_module_level(modules) when is_list(modules) do
    case :lists.all(
           fn m ->
             is_atom(m)
           end,
           modules
         ) do
      true ->
        call({:unset_module_level, modules})

      false ->
        {:error, {:not_a_list_of_modules, modules}}
    end
  end

  def unset_module_level(modules) do
    {:error, {:not_a_list_of_modules, modules}}
  end

  def set_config(owner, key, value) do
    case sanity_check(owner, key, value) do
      :ok ->
        call({:change_config, :set, owner, key, value})

      error ->
        error
    end
  end

  def set_config(owner, config) do
    case sanity_check(owner, config) do
      :ok ->
        call({:change_config, :set, owner, config})

      error ->
        error
    end
  end

  def update_config(owner, key, value) do
    case sanity_check(owner, key, value) do
      :ok ->
        call({:change_config, :update, owner, key, value})

      error ->
        error
    end
  end

  def update_config(owner, config) do
    case sanity_check(owner, config) do
      :ok ->
        call({:change_config, :update, owner, config})

      error ->
        error
    end
  end

  def update_formatter_config(handlerId, formatterConfig)
      when is_map(formatterConfig) do
    call({:update_formatter_config, handlerId, formatterConfig})
  end

  def update_formatter_config(_HandlerId, formatterConfig) do
    {:error, {:invalid_formatter_config, formatterConfig}}
  end

  def init([]) do
    :erlang.process_flag(:trap_exit, true)
    :erlang.put(:"$logger_cb_process", true)
    tid = :logger_config.new(:logger)
    :logger_config.create(tid, :proxy, :logger_proxy.get_default_config())

    primaryConfig =
      :maps.merge(
        default_config(:primary),
        %{handlers: [:simple]}
      )

    :logger_config.create(tid, :primary, primaryConfig)

    simpleConfig0 =
      :maps.merge(
        default_config(
          :simple,
          :logger_simple_h
        ),
        %{
          filter_default: :stop,
          filters: [
            {:remote_gl, {&:logger_filters.remote_gl/2, :stop}},
            {:domain, {&:logger_filters.domain/2, {:log, :super, [:otp]}}},
            {:no_domain, {&:logger_filters.domain/2, {:log, :undefined, []}}}
          ]
        }
      )

    {:ok, simpleConfig} = :logger_simple_h.adding_handler(simpleConfig0)
    :logger_config.create(tid, :simple, simpleConfig)
    {:ok, r_state(tid: tid, async_req_queue: :queue.new())}
  end

  def handle_call({:add_handler, id, module, hConfig}, from, r_state(tid: tid) = state) do
    case :logger_config.exist(tid, id) do
      true ->
        {:reply, {:error, {:already_exist, id}}, state}

      false ->
        call_h_async(
          fn ->
            call_h(module, :adding_handler, [hConfig], {:ok, hConfig})
          end,
          fn
            {:ok, hConfig1} ->
              case :erlang.function_exported(module, :log, 2) do
                true ->
                  :logger_config.create(tid, id, hConfig1)

                  {:ok, config} =
                    :logger_config.get(
                      tid,
                      :primary
                    )

                  handlers = :maps.get(:handlers, config, [])

                  :logger_config.set(
                    tid,
                    :primary,
                    Map.put(config, :handlers, [
                      id
                      | handlers
                    ])
                  )

                false ->
                  {:error, {:invalid_handler, {:function_not_exported, {module, :log, 2}}}}
              end

            {:error, hReason} ->
              {:error, {:handler_not_added, hReason}}
          end,
          from,
          state
        )
    end
  end

  def handle_call({:remove_handler, handlerId}, from, r_state(tid: tid) = state) do
    case :logger_config.get(tid, handlerId) do
      {:ok, %{module: module} = hConfig} ->
        {:ok, config} = :logger_config.get(tid, :primary)
        handlers0 = :maps.get(:handlers, config, [])
        handlers = :lists.delete(handlerId, handlers0)

        call_h_async(
          fn ->
            call_h(module, :removing_handler, [hConfig], :ok)
          end,
          fn _Res ->
            :logger_config.set(tid, :primary, Map.put(config, :handlers, handlers))
            :logger_config.delete(tid, handlerId)
            :ok
          end,
          from,
          state
        )

      _ ->
        {:reply, {:error, {:not_found, handlerId}}, state}
    end
  end

  def handle_call({:add_filter, id, filter}, _From, r_state(tid: tid) = state) do
    reply = do_add_filter(tid, id, filter)
    {:reply, reply, state}
  end

  def handle_call({:remove_filter, id, filterId}, _From, r_state(tid: tid) = state) do
    reply = do_remove_filter(tid, id, filterId)
    {:reply, reply, state}
  end

  def handle_call({:change_config, setOrUpd, :proxy, config0}, _From, r_state(tid: tid) = state) do
    default =
      case setOrUpd do
        :set ->
          :logger_proxy.get_default_config()

        :update ->
          {:ok, oldConfig} = :logger_config.get(tid, :proxy)
          oldConfig
      end

    config = :maps.merge(default, config0)

    reply =
      case :logger_olp.set_opts(
             :logger_proxy,
             config
           ) do
        :ok ->
          :logger_config.set(tid, :proxy, config)

        error ->
          error
      end

    {:reply, reply, state}
  end

  def handle_call({:change_config, setOrUpd, :primary, config0}, _From, r_state(tid: tid) = state) do
    {:ok, %{handlers: handlers} = oldConfig} =
      :logger_config.get(
        tid,
        :primary
      )

    default =
      case setOrUpd do
        :set ->
          default_config(:primary)

        :update ->
          oldConfig
      end

    config = :maps.merge(default, config0)
    reply = :logger_config.set(tid, :primary, Map.put(config, :handlers, handlers))
    {:reply, reply, state}
  end

  def handle_call(
        {:change_config, _SetOrUpd, :primary, key, value},
        _From,
        r_state(tid: tid) = state
      ) do
    {:ok, oldConfig} = :logger_config.get(tid, :primary)
    reply = :logger_config.set(tid, :primary, Map.put(oldConfig, key, value))
    {:reply, reply, state}
  end

  def handle_call({:change_config, setOrUpd, handlerId, config0}, from, r_state(tid: tid) = state) do
    case :logger_config.get(tid, handlerId) do
      {:ok, %{module: module} = oldConfig} ->
        default =
          case setOrUpd do
            :set ->
              default_config(handlerId, module)

            :update ->
              oldConfig
          end

        config = :maps.merge(default, config0)

        case check_config_change(oldConfig, config) do
          :ok ->
            call_h_async(
              fn ->
                call_h(module, :changing_config, [setOrUpd, oldConfig, config], {:ok, config})
              end,
              fn
                {:ok, config1} ->
                  :logger_config.set(tid, handlerId, config1)

                error ->
                  error
              end,
              from,
              state
            )

          error ->
            {:reply, error, state}
        end

      _ ->
        {:reply, {:error, {:not_found, handlerId}}, state}
    end
  end

  def handle_call(
        {:change_config, setOrUpd, handlerId, key, value},
        from,
        r_state(tid: tid) = state
      ) do
    case :logger_config.get(tid, handlerId) do
      {:ok, %{module: module} = oldConfig} ->
        config = Map.put(oldConfig, key, value)

        case check_config_change(oldConfig, config) do
          :ok ->
            call_h_async(
              fn ->
                call_h(module, :changing_config, [setOrUpd, oldConfig, config], {:ok, config})
              end,
              fn
                {:ok, config1} ->
                  :logger_config.set(tid, handlerId, config1)

                error ->
                  error
              end,
              from,
              state
            )

          error ->
            {:reply, error, state}
        end

      _ ->
        {:reply, {:error, {:not_found, handlerId}}, state}
    end
  end

  def handle_call(
        {:update_formatter_config, handlerId, newFConfig},
        _From,
        r_state(tid: tid) = state
      ) do
    reply =
      case :logger_config.get(tid, handlerId) do
        {:ok, %{formatter: {fMod, oldFConfig}} = config} ->
          try do
            fConfig = :maps.merge(oldFConfig, newFConfig)
            check_formatter({fMod, fConfig})
            :logger_config.set(tid, handlerId, Map.put(config, :formatter, {fMod, fConfig}))
          catch
            reason ->
              {:error, reason}
          end

        _ ->
          {:error, {:not_found, handlerId}}
      end

    {:reply, reply, state}
  end

  def handle_call({:set_module_level, modules, level}, _From, state) do
    reply = :logger_config.set_module_level(modules, level)
    {:reply, reply, state}
  end

  def handle_call({:unset_module_level, modules}, _From, state) do
    reply = :logger_config.unset_module_level(modules)
    {:reply, reply, state}
  end

  def handle_cast(
        {:async_req_reply, _Ref, _Reply} = reply,
        state
      ) do
    call_h_reply(reply, state)
  end

  def handle_info({:log, level, format, args, meta}, state) do
    :logger.log(level, format, args, meta)
    {:noreply, state}
  end

  def handle_info({:log, level, report, meta}, state) do
    :logger.log(level, report, meta)
    {:noreply, state}
  end

  def handle_info({ref, _Reply}, state) when is_reference(ref) do
    {:noreply, state}
  end

  def handle_info(
        {:DOWN, _Ref, _Proc, _Pid, _Reason} = down,
        state
      ) do
    call_h_reply(down, state)
  end

  def handle_info(unexpected, state)
      when :erlang.element(
             1,
             unexpected
           ) == :EXIT do
    case :logger.allow(:debug, :logger_server) do
      true ->
        _ =
          :logger_server.do_internal_log(
            :debug,
            %{
              mfa: {:logger_server, :handle_info, 2},
              line: 355,
              file: 'otp/lib/kernel/src/logger_server.erl'
            },
            %{},
            [[{:logger, :got_unexpected_message}, {:process, :logger}, {:message, unexpected}]]
          )

        :ok

      false ->
        :ok
    end

    {:noreply, state}
  end

  def handle_info(unexpected, state) do
    case :logger.allow(:info, :logger_server) do
      true ->
        _ =
          :logger_server.do_internal_log(
            :info,
            %{
              mfa: {:logger_server, :handle_info, 2},
              line: 362,
              file: 'otp/lib/kernel/src/logger_server.erl'
            },
            %{},
            [[{:logger, :got_unexpected_message}, {:process, :logger}, {:message, unexpected}]]
          )

        :ok

      false ->
        :ok
    end

    {:noreply, state}
  end

  def terminate(_Reason, _State) do
    :ok
  end

  defp call(request) when is_tuple(request) do
    action = :erlang.element(1, request)

    case :erlang.get(:"$logger_cb_process") do
      true
      when action == :add_handler or
             action == :remove_handler or action == :add_filter or
             action == :remove_filter or action == :change_config ->
        {:error, {:attempting_syncronous_call_to_self, request}}

      _ ->
        :gen_server.call(:logger, request, :infinity)
    end
  end

  defp do_add_filter(tid, id, {fId, _} = filter) do
    case :logger_config.get(tid, id) do
      {:ok, config} ->
        filters = :maps.get(:filters, config, [])

        case :lists.keymember(fId, 1, filters) do
          true ->
            {:error, {:already_exist, fId}}

          false ->
            :logger_config.set(tid, id, Map.put(config, :filters, [filter | filters]))
        end

      error ->
        error
    end
  end

  defp do_remove_filter(tid, id, filterId) do
    case :logger_config.get(tid, id) do
      {:ok, config} ->
        filters0 = :maps.get(:filters, config, [])

        case :lists.keytake(filterId, 1, filters0) do
          {:value, _, filters} ->
            :logger_config.set(tid, id, Map.put(config, :filters, filters))

          false ->
            {:error, {:not_found, filterId}}
        end

      error ->
        error
    end
  end

  defp default_config(:primary) do
    %{level: :notice, filters: [], filter_default: :log}
  end

  defp default_config(id) do
    %{id: id, level: :all, filters: [], filter_default: :log, formatter: {:logger_formatter, %{}}}
  end

  defp default_config(id, module) do
    Map.put(default_config(id), :module, module)
  end

  defp sanity_check(owner, key, value) do
    sanity_check_1(owner, [{key, value}])
  end

  defp sanity_check(owner, config) when is_map(config) do
    sanity_check_1(owner, :maps.to_list(config))
  end

  defp sanity_check(_, config) do
    {:error, {:invalid_config, config}}
  end

  defp sanity_check_1(:proxy, _Config) do
    :ok
  end

  defp sanity_check_1(owner, config) when is_list(config) do
    try do
      type = get_type(owner)
      check_config(type, config)
    catch
      error ->
        {:error, error}
    end
  end

  defp get_type(:primary) do
    :primary
  end

  defp get_type(id) do
    check_id(id)
    :handler
  end

  defp check_config(owner, [{:level, level} | config]) do
    check_level(level)
    check_config(owner, config)
  end

  defp check_config(owner, [{:filters, filters} | config]) do
    check_filters(filters)
    check_config(owner, config)
  end

  defp check_config(owner, [{:filter_default, fD} | config]) do
    check_filter_default(fD)
    check_config(owner, config)
  end

  defp check_config(:handler, [{:formatter, formatter} | config]) do
    check_formatter(formatter)
    check_config(:handler, config)
  end

  defp check_config(:primary, [c | _]) do
    throw({:invalid_primary_config, c})
  end

  defp check_config(:handler, [{_, _} | config]) do
    check_config(:handler, config)
  end

  defp check_config(_, []) do
    :ok
  end

  defp check_id(id) when is_atom(id) do
    :ok
  end

  defp check_id(id) do
    throw({:invalid_id, id})
  end

  defp check_mod(mod) when is_atom(mod) do
    :ok
  end

  defp check_mod(mod) do
    throw({:invalid_module, mod})
  end

  defp check_level(level) do
    case :lists.member(
           level,
           [:none, :emergency, :alert, :critical, :error, :warning, :notice, :info, :debug, :all]
         ) do
      true ->
        :ok

      false ->
        throw({:invalid_level, level})
    end
  end

  defp check_filters([{id, {fun, _Args}} | filters])
       when is_atom(id) and is_function(fun, 2) do
    check_filters(filters)
  end

  defp check_filters([filter | _]) do
    throw({:invalid_filter, filter})
  end

  defp check_filters([]) do
    :ok
  end

  defp check_filters(filters) do
    throw({:invalid_filters, filters})
  end

  defp check_filter_default(fD) when fD == :stop or fD == :log do
    :ok
  end

  defp check_filter_default(fD) do
    throw({:invalid_filter_default, fD})
  end

  defp check_formatter({mod, config}) do
    check_mod(mod)

    try do
      mod.check_config(config)
    catch
      c, r ->
        case {c, r, __STACKTRACE__} do
          {:error, :undef, [{^mod, :check_config, [^config], _} | _]} ->
            :ok

          _ ->
            throw(
              {:callback_crashed,
               {c, r,
                :logger.filter_stacktrace(
                  :logger_server,
                  __STACKTRACE__
                )}}
            )
        end
    else
      :ok ->
        :ok

      {:error, error} ->
        throw(error)
    end
  end

  defp check_formatter(formatter) do
    throw({:invalid_formatter, formatter})
  end

  defp check_config_change(
         %{id: id, module: module},
         %{id: id, module: module}
       ) do
    :ok
  end

  defp check_config_change(oldConfig, newConfig) do
    {old, new} =
      :logger_server.diff_maps(
        :maps.with(
          [:id, :module],
          oldConfig
        ),
        :maps.with(
          [:id, :module],
          newConfig
        )
      )

    {:error, {:illegal_config_change, old, new}}
  end

  defp call_h(module, function, args, defRet) do
    try do
      apply(module, function, args)
    catch
      c, r ->
        case {c, r, __STACKTRACE__} do
          {:error, :undef, [{^module, ^function = :changing_config, args, _} | _]}
          when length(args) === 3 ->
            call_h(module, function, tl(args), defRet)

          {:error, :undef, [{^module, ^function, ^args, _} | _]} ->
            defRet

          _ ->
            sT =
              :logger.filter_stacktrace(
                :logger_server,
                __STACKTRACE__
              )

            case :logger.allow(:error, :logger_server) do
              true ->
                _ =
                  :logger_server.do_internal_log(
                    :error,
                    %{
                      mfa: {:logger_server, :call_h, 4},
                      line: 548,
                      file: 'otp/lib/kernel/src/logger_server.erl'
                    },
                    %{},
                    [[{:logger, :callback_crashed}, {:process, :logger}, {:reason, {c, r, sT}}]]
                  )

                :ok

              false ->
                :ok
            end

            {:error, {:callback_crashed, {c, r, sT}}}
        end
    end
  end

  defp call_h_async(asyncFun, postFun, from, r_state(async_req: :undefined) = state) do
    parent = self()

    {pid, ref} =
      spawn_monitor(fn ->
        :erlang.put(:"$logger_cb_process", true)

        receive do
          ref ->
            ref
        end

        :gen_server.cast(
          parent,
          {:async_req_reply, ref, asyncFun.()}
        )
      end)

    send(pid, ref)
    {:noreply, r_state(state, async_req: {ref, postFun, from})}
  end

  defp call_h_async(asyncFun, postFun, from, r_state(async_req_queue: q) = state) do
    {:noreply,
     r_state(state,
       async_req_queue:
         :queue.in(
           {asyncFun, postFun, from},
           q
         )
     )}
  end

  defp call_h_reply(
         {:async_req_reply, ref, reply},
         r_state(
           async_req: {ref, postFun, from},
           async_req_queue: q
         ) = state
       ) do
    :erlang.demonitor(ref, [:flush])
    _ = :gen_server.reply(from, postFun.(reply))
    {value, newQ} = :queue.out(q)

    newState =
      r_state(state,
        async_req: :undefined,
        async_req_queue: newQ
      )

    case value do
      {:value, {asyncFun, nPostFun, nFrom}} ->
        call_h_async(asyncFun, nPostFun, nFrom, newState)

      :empty ->
        {:noreply, newState}
    end
  end

  defp call_h_reply(
         {:DOWN, ref, _Proc, pid, reason},
         r_state(async_req: {ref, _PostFun, _From}) = state
       ) do
    case :logger.allow(:error, :logger_server) do
      true ->
        _ =
          :logger_server.do_internal_log(
            :error,
            %{
              mfa: {:logger_server, :call_h_reply, 2},
              line: 591,
              file: 'otp/lib/kernel/src/logger_server.erl'
            },
            %{},
            [[{:logger, :process_exited}, {:process, pid}, {:reason, reason}]]
          )

        :ok

      false ->
        :ok
    end

    call_h_reply(
      {:async_req_reply, ref, {:error, {:logger_process_exited, pid, reason}}},
      state
    )
  end

  defp call_h_reply(unexpected, state) do
    case :logger.allow(:info, :logger_server) do
      true ->
        _ =
          :logger_server.do_internal_log(
            :info,
            %{
              mfa: {:logger_server, :call_h_reply, 2},
              line: 600,
              file: 'otp/lib/kernel/src/logger_server.erl'
            },
            %{},
            [[{:logger, :got_unexpected_message}, {:process, :logger}, {:message, unexpected}]]
          )

        :ok

      false ->
        :ok
    end

    {:noreply, state}
  end

  def diff_maps(m1, m2) do
    diffs(:lists.sort(:maps.to_list(m1)), :lists.sort(:maps.to_list(m2)), %{}, %{})
  end

  defp diffs([h | t1], [h | t2], d1, d2) do
    diffs(t1, t2, d1, d2)
  end

  defp diffs([{k, v1} | t1], [{k, v2} | t2], d1, d2) do
    diffs(t1, t2, Map.put(d1, k, v1), Map.put(d2, k, v2))
  end

  defp diffs([], [], d1, d2) do
    {d1, d2}
  end

  def do_internal_log(level, location, log, [report] = data) do
    do_internal_log(level, location, log, data, {:report, report})
  end

  def do_internal_log(level, location, log, [fmt, args] = data) do
    do_internal_log(level, location, log, data, {fmt, args})
  end

  defp do_internal_log(level, location, log, data, msg) do
    meta =
      :logger.add_default_metadata(
        :maps.merge(
          location,
          :maps.get(:meta, log, %{})
        )
      )

    case log do
      %{meta: %{internal_log_event: true}} ->
        _ = spawn(:logger_simple_h, :log, [%{level: level, msg: msg, meta: meta}, %{}])

      _ ->
        _ =
          spawn(
            :logger,
            :macro_log,
            [
              location,
              level
              | data
            ] ++ [Map.put(meta, :internal_log_event, true)]
          )
    end
  end
end
