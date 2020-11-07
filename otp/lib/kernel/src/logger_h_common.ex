defmodule :m_logger_h_common do
  use Bitwise
  @behaviour :gen_server
  def filesync(module, name) do
    call(module, name, :filesync)
  end

  def adding_handler(%{:id => name, :module => module} = config) do
    hConfig0 = :maps.get(:config, config, %{})

    handlerConfig0 =
      :maps.without(
        [
          :sync_mode_qlen,
          :drop_mode_qlen,
          :flush_qlen,
          :burst_limit_enable,
          :burst_limit_max_count,
          :burst_limit_window_time,
          :overload_kill_enable,
          :overload_kill_qlen,
          :overload_kill_mem_size,
          :overload_kill_restart_after
        ] ++ [:filesync_repeat_interval],
        hConfig0
      )

    case module.check_config(name, :set, :undefined, handlerConfig0) do
      {:ok, handlerConfig} ->
        modifiedCommon =
          :maps.with(
            [:filesync_repeat_interval],
            handlerConfig
          )

        commonConfig0 =
          :maps.with(
            [:filesync_repeat_interval],
            hConfig0
          )

        commonConfig =
          :maps.merge(
            :maps.merge(
              get_default_config(),
              commonConfig0
            ),
            modifiedCommon
          )

        case check_config(commonConfig) do
          :ok ->
            hConfig = :maps.merge(commonConfig, handlerConfig)

            olpOpts =
              :maps.with(
                [
                  :sync_mode_qlen,
                  :drop_mode_qlen,
                  :flush_qlen,
                  :burst_limit_enable,
                  :burst_limit_max_count,
                  :burst_limit_window_time,
                  :overload_kill_enable,
                  :overload_kill_qlen,
                  :overload_kill_mem_size,
                  :overload_kill_restart_after
                ],
                hConfig0
              )

            start(olpOpts, %{config | :config => hConfig})

          {:error, faulty} ->
            {:error, {:invalid_config, module, faulty}}
        end

      error ->
        error
    end
  end

  def removing_handler(%{:id => name, :module => module, :config => %{:olp => olp}}) do
    case :erlang.whereis(:erlang.list_to_atom(:lists.concat([module, '_', name]))) do
      :undefined ->
        :ok

      _Pid ->
        _ = :logger_olp.stop(olp)
        :ok
    end
  end

  def changing_config(
        setOrUpdate,
        %{:id => name, :config => oldHConfig, :module => module},
        newConfig0
      ) do
    newHConfig0 = :maps.get(:config, newConfig0, %{})

    noHandlerKeys =
      [
        :sync_mode_qlen,
        :drop_mode_qlen,
        :flush_qlen,
        :burst_limit_enable,
        :burst_limit_max_count,
        :burst_limit_window_time,
        :overload_kill_enable,
        :overload_kill_qlen,
        :overload_kill_mem_size,
        :overload_kill_restart_after
      ] ++ [:filesync_repeat_interval] ++ [:olp]

    oldHandlerConfig =
      :maps.without(
        noHandlerKeys,
        oldHConfig
      )

    newHandlerConfig0 =
      :maps.without(
        noHandlerKeys,
        newHConfig0
      )

    case module.check_config(name, setOrUpdate, oldHandlerConfig, newHandlerConfig0) do
      {:ok, newHandlerConfig} ->
        modifiedCommon =
          :maps.with(
            [:filesync_repeat_interval],
            newHandlerConfig
          )

        newCommonConfig0 =
          :maps.with(
            [:filesync_repeat_interval],
            newHConfig0
          )

        oldCommonConfig =
          :maps.with(
            [:filesync_repeat_interval],
            oldHConfig
          )

        commonDefault =
          case setOrUpdate do
            :set ->
              get_default_config()

            :update ->
              oldCommonConfig
          end

        newCommonConfig =
          :maps.merge(
            :maps.merge(
              commonDefault,
              newCommonConfig0
            ),
            modifiedCommon
          )

        case check_config(newCommonConfig) do
          :ok ->
            olpDefault =
              case setOrUpdate do
                :set ->
                  :logger_olp.get_default_opts()

                :update ->
                  :maps.with(
                    [
                      :sync_mode_qlen,
                      :drop_mode_qlen,
                      :flush_qlen,
                      :burst_limit_enable,
                      :burst_limit_max_count,
                      :burst_limit_window_time,
                      :overload_kill_enable,
                      :overload_kill_qlen,
                      :overload_kill_mem_size,
                      :overload_kill_restart_after
                    ],
                    oldHConfig
                  )
              end

            olp = :maps.get(:olp, oldHConfig)

            newOlpOpts =
              :maps.merge(
                olpDefault,
                :maps.with(
                  [
                    :sync_mode_qlen,
                    :drop_mode_qlen,
                    :flush_qlen,
                    :burst_limit_enable,
                    :burst_limit_max_count,
                    :burst_limit_window_time,
                    :overload_kill_enable,
                    :overload_kill_qlen,
                    :overload_kill_mem_size,
                    :overload_kill_restart_after
                  ],
                  newHConfig0
                )
              )

            case :logger_olp.set_opts(olp, newOlpOpts) do
              :ok ->
                :logger_olp.cast(
                  olp,
                  {:config_changed, newCommonConfig, newHandlerConfig}
                )

                readOnly = :maps.with([:olp], oldHConfig)

                newHConfig =
                  :maps.merge(
                    :maps.merge(
                      :maps.merge(
                        newCommonConfig,
                        newHandlerConfig
                      ),
                      readOnly
                    ),
                    newOlpOpts
                  )

                newConfig = %{newConfig0 | :config => newHConfig}
                {:ok, newConfig}

              error ->
                error
            end

          {:error, faulty} ->
            {:error, {:invalid_config, module, faulty}}
        end

      error ->
        error
    end
  end

  def log(
        logEvent,
        config = %{:config => %{:olp => olp}}
      ) do
    true = :erlang.is_process_alive(:logger_olp.get_pid(olp))
    bin = log_to_binary(logEvent, config)
    :logger_olp.load(olp, bin)
  end

  def filter_config(%{:config => hConfig} = config) do
    %{config | :config => :maps.without([:olp], hConfig)}
  end

  defp start(
         olpOpts0,
         %{:id => name, :module => module, :config => hConfig} = config0
       ) do
    regName = :erlang.list_to_atom(:lists.concat([module, '_', name]))

    childSpec = %{
      :id => name,
      :start => {:logger_olp, :start_link, [regName, :logger_h_common, config0, olpOpts0]},
      :restart => :temporary,
      :shutdown => 2000,
      :type => :worker,
      :modules => [:logger_h_common]
    }

    case :supervisor.start_child(
           :logger_sup,
           childSpec
         ) do
      {:ok, pid, olp} ->
        :ok =
          :logger_handler_watcher.register_handler(
            name,
            pid
          )

        olpOpts = :logger_olp.get_opts(olp)
        {:ok, %{config0 | :config => %{:maps.merge(hConfig, olpOpts) | :olp => olp}}}

      {:error, {reason, ch}}
      when is_tuple(ch) and
             :erlang.element(1, ch) == :child ->
        {:error, reason}

      error ->
        error
    end
  end

  def init(%{:id => name, :module => module, :config => hConfig}) do
    :erlang.process_flag(:trap_exit, true)
    :ok

    case module.init(name, hConfig) do
      {:ok, hState} ->
        commonConfig =
          :maps.with(
            [:filesync_repeat_interval],
            hConfig
          )

        state = %{
          commonConfig
          | :id => name,
            :module => module,
            :ctrl_sync_count => 20,
            :last_op => :sync,
            :handler_state => hState
        }

        state1 = set_repeated_filesync(state)
        {:ok, state1}

      error ->
        error
    end
  end

  def handle_load(
        bin,
        %{
          :id => name,
          :module => module,
          :handler_state => handlerState,
          :ctrl_sync_count => ctrlSync
        } = state
      ) do
    cond do
      ctrlSync == 0 ->
        {_, hS1} = module.write(name, :sync, bin, handlerState)
        %{state | :handler_state => hS1, :ctrl_sync_count => 20, :last_op => :write}

      true ->
        {_, hS1} = module.write(name, :async, bin, handlerState)
        %{state | :handler_state => hS1, :ctrl_sync_count => ctrlSync - 1, :last_op => :write}
    end
  end

  def handle_call(
        :filesync,
        _From,
        state = %{:id => name, :module => module, :handler_state => handlerState}
      ) do
    {result, handlerState1} = module.filesync(name, :sync, handlerState)
    {:reply, result, %{state | :handler_state => handlerState1, :last_op => :sync}}
  end

  def handle_cast(
        :repeated_filesync,
        state = %{:filesync_repeat_interval => :no_repeat}
      ) do
    {:noreply, state}
  end

  def handle_cast(
        :repeated_filesync,
        state = %{
          :id => name,
          :module => module,
          :handler_state => handlerState,
          :last_op => lastOp
        }
      ) do
    state1 =
      cond do
        lastOp == :sync ->
          state

        true ->
          {_, hS} = module.filesync(name, :async, handlerState)
          %{state | :handler_state => hS, :last_op => :sync}
      end

    {:noreply, set_repeated_filesync(state1)}
  end

  def handle_cast(
        {:config_changed, commonConfig, hConfig},
        state = %{
          :id => name,
          :module => module,
          :handler_state => handlerState,
          :filesync_repeat_interval => oldFSyncInt
        }
      ) do
    state1 =
      case :maps.get(
             :filesync_repeat_interval,
             commonConfig
           ) do
        ^oldFSyncInt ->
          state

        fSyncInt ->
          set_repeated_filesync(
            cancel_repeated_filesync(%{state | :filesync_repeat_interval => fSyncInt})
          )
      end

    hS =
      try do
        module.config_changed(name, hConfig, handlerState)
      catch
        :error, :undef ->
          handlerState
      end

    {:noreply, %{state1 | :handler_state => hS}}
  end

  def handle_info(
        info,
        %{:id => name, :module => module, :handler_state => handlerState} = state
      ) do
    {:noreply, %{state | :handler_state => module.handle_info(name, info, handlerState)}}
  end

  def terminate(:overloaded = reason, %{:id => name} = state) do
    _ = log_handler_info(name, 'Handler ~p overloaded and stopping', [name], state)
    do_terminate(reason, state)
    configResult = :logger.get_handler_config(name)

    case configResult do
      {:ok, %{:module => module} = hConfig0} ->
        spawn(fn ->
          :logger.remove_handler(name)
        end)

        hConfig =
          try do
            module.filter_config(hConfig0)
          catch
            _, _ ->
              hConfig0
          end

        {:ok,
         fn ->
           :logger.add_handler(name, module, hConfig)
         end}

      error ->
        error_notify({name, :restart_impossible, error})
        error
    end
  end

  def terminate(reason, state) do
    do_terminate(reason, state)
  end

  defp do_terminate(
         reason,
         state = %{:id => name, :module => module, :handler_state => handlerState}
       ) do
    _ = cancel_repeated_filesync(state)
    _ = module.terminate(name, reason, handlerState)
    :ok
  end

  def code_change(_OldVsn, state, _Extra) do
    {:ok, state}
  end

  def reset_state(%{:id => name, :module => module, :handler_state => handlerState} = state) do
    %{state | :handler_state => module.reset_state(name, handlerState)}
  end

  defp call(module, name, op) when is_atom(name) do
    case :logger_olp.call(
           :erlang.list_to_atom(:lists.concat([module, '_', name])),
           op
         ) do
      {:error, :busy} ->
        {:error, :handler_busy}

      other ->
        other
    end
  end

  defp call(_, name, op) do
    {:error, {:badarg, {op, [name]}}}
  end

  def notify(
        {:mode_change, mode0, mode1},
        %{:id => name} = state
      ) do
    log_handler_info(name, 'Handler ~p switched from  ~p to ~p mode', [name, mode0, mode1], state)
  end

  def notify({:flushed, flushed}, %{:id => name} = state) do
    log_handler_info(name, 'Handler ~p flushed ~w log events', [name, flushed], state)
  end

  def notify(:restart, %{:id => name} = state) do
    log_handler_info(name, 'Handler ~p restarted', [name], state)
  end

  def notify(
        :idle,
        %{:id => name, :module => module, :handler_state => handlerState} = state
      ) do
    {_, hS} = module.filesync(name, :async, handlerState)
    %{state | :handler_state => hS, :last_op => :sync}
  end

  defp log_handler_info(
         name,
         format,
         args,
         %{:module => module, :handler_state => handlerState} = state
       ) do
    config =
      case :logger.get_handler_config(name) do
        {:ok, conf} ->
          conf

        _ ->
          %{:formatter => {:logger_formatter, %{:legacy_header => true, :single_line => false}}}
      end

    meta = %{:time => :logger.timestamp()}

    bin =
      log_to_binary(
        %{:level => :notice, :msg => {format, args}, :meta => meta},
        config
      )

    {_, hS} = module.write(name, :async, bin, handlerState)
    %{state | :handler_state => hS, :last_op => :write}
  end

  defp log_to_binary(
         %{:msg => {:report, _}, :meta => %{:report_cb => _}} = log,
         config
       ) do
    do_log_to_binary(log, config)
  end

  defp log_to_binary(
         %{:msg => {:report, _}, :meta => meta} = log,
         config
       ) do
    defaultReportCb = &:logger.format_otp_report/1

    do_log_to_binary(
      %{log | :meta => %{meta | :report_cb => defaultReportCb}},
      config
    )
  end

  defp log_to_binary(log, config) do
    do_log_to_binary(log, config)
  end

  defp do_log_to_binary(log, config) do
    {formatter, formatterConfig} =
      :maps.get(
        :formatter,
        config,
        {:logger_formatter, %{:legacy_header => true, :single_line => false}}
      )

    string = try_format(log, formatter, formatterConfig)

    try do
      string_to_binary(string)
    catch
      c2, r2 ->
        case :logger.allow(:debug, :logger_h_common) do
          true ->
            _ =
              :logger_server.do_internal_log(
                :debug,
                %{
                  :mfa => {:logger_h_common, :do_log_to_binary, 2},
                  :line => 395,
                  :file => 'otp/lib/kernel/src/logger_h_common.erl'
                },
                log,
                [
                  [
                    {:formatter_error, formatter},
                    {:config, formatterConfig},
                    {:log_event, log},
                    {:bad_return_value, string},
                    {:catched, {c2, r2, __STACKTRACE__}}
                  ]
                ]
              )

            :ok

          false ->
            :ok
        end

        "FORMATTER ERROR: bad return value\n"
    end
  end

  defp try_format(log, formatter, formatterConfig) do
    try do
      formatter.format(log, formatterConfig)
    catch
      c, r ->
        case :logger.allow(:debug, :logger_h_common) do
          true ->
            _ =
              :logger_server.do_internal_log(
                :debug,
                %{
                  :mfa => {:logger_h_common, :try_format, 3},
                  :line => 407,
                  :file => 'otp/lib/kernel/src/logger_h_common.erl'
                },
                log,
                [
                  [
                    {:formatter_crashed, formatter},
                    {:config, formatterConfig},
                    {:log_event, log},
                    {:reason,
                     {c, r,
                      :logger.filter_stacktrace(
                        :logger_h_common,
                        __STACKTRACE__
                      )}}
                  ]
                ]
              )

            :ok

          false ->
            :ok
        end

        case {:logger_formatter, %{}} do
          {^formatter, ^formatterConfig} ->
            'DEFAULT FORMATTER CRASHED\n'

          {defaultFormatter, defaultConfig} ->
            try_format(
              %{log | :msg => {'FORMATTER CRASH: ~tp', [:maps.get(:msg, log)]}},
              defaultFormatter,
              defaultConfig
            )
        end
    end
  end

  defp string_to_binary(string) do
    case :unicode.characters_to_binary(string) do
      binary when is_binary(binary) ->
        binary

      error ->
        throw(error)
    end
  end

  defp check_config(config) when is_map(config) do
    check_common_config(:maps.to_list(config))
  end

  defp check_common_config([{:filesync_repeat_interval, norA} | config])
       when is_integer(norA) or norA == :no_repeat do
    check_common_config(config)
  end

  defp check_common_config([{key, value} | _]) do
    {:error, %{key => value}}
  end

  defp check_common_config([]) do
    :ok
  end

  defp get_default_config() do
    %{:filesync_repeat_interval => 5000}
  end

  defp set_repeated_filesync(%{:filesync_repeat_interval => fSyncInt} = state)
       when is_integer(fSyncInt) do
    {:ok, tRef} = :timer.apply_after(fSyncInt, :gen_server, :cast, [self(), :repeated_filesync])
    %{state | :rep_sync_tref => tRef}
  end

  defp set_repeated_filesync(state) do
    state
  end

  defp cancel_repeated_filesync(state) do
    case :maps.take(:rep_sync_tref, state) do
      {tRef, state1} ->
        _ = :timer.cancel(tRef)
        state1

      :error ->
        state
    end
  end

  def error_notify(term) do
    :logger.internal_log(:error, term)
  end
end
