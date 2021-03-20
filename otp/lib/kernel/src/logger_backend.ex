defmodule :m_logger_backend do
  use Bitwise

  def log_allowed(log, tid) do
    {:ok, config} = :logger_config.get(tid, :primary)
    filters = :maps.get(:filters, config, [])

    case apply_filters(:primary, log, filters, config) do
      :stop ->
        :ok

      log1 ->
        handlers = :maps.get(:handlers, config, [])
        call_handlers(log1, handlers, tid)
    end

    :ok
  end

  defp call_handlers(%{level: level} = log, [id | handlers], tid) do
    case :logger_config.get(tid, id, level) do
      {:ok, %{module: module} = config} ->
        filters = :maps.get(:filters, config, [])

        case apply_filters(id, log, filters, config) do
          :stop ->
            :ok

          log1 ->
            config1 =
              :maps.without(
                [:level, :filters, :filter_default, :handlers],
                config
              )

            try do
              module.log(log1, config1)
            catch
              c, r ->
                case :logger.remove_handler(id) do
                  :ok ->
                    :logger.internal_log(
                      :error,
                      {:removed_failing_handler, id}
                    )

                    case :logger.allow(:debug, :logger_backend) do
                      true ->
                        _ =
                          :logger_server.do_internal_log(
                            :debug,
                            %{
                              mfa: {:logger_backend, :call_handlers, 3},
                              line: 58,
                              file: 'otp/lib/kernel/src/logger_backend.erl'
                            },
                            log1,
                            [
                              [
                                {:logger, :removed_failing_handler},
                                {:handler, {id, module}},
                                {:log_event, log1},
                                {:config, config1},
                                {:reason, {c, r, filter_stacktrace(__STACKTRACE__)}}
                              ]
                            ]
                          )

                        :ok

                      false ->
                        :ok
                    end

                  {:error, {:not_found, _}} ->
                    :ok

                  {:error, reason} ->
                    case :logger.allow(:debug, :logger_backend) do
                      true ->
                        _ =
                          :logger_server.do_internal_log(
                            :debug,
                            %{
                              mfa: {:logger_backend, :call_handlers, 3},
                              line: 71,
                              file: 'otp/lib/kernel/src/logger_backend.erl'
                            },
                            log1,
                            [[{:logger, :remove_handler_failed}, {:reason, reason}]]
                          )

                        :ok

                      false ->
                        :ok
                    end
                end
            end
        end

      _ ->
        :ok
    end

    call_handlers(log, handlers, tid)
  end

  defp call_handlers(_Log, [], _Tid) do
    :ok
  end

  defp apply_filters(owner, log, filters, config) do
    case do_apply_filters(owner, log, filters, :ignore) do
      :stop ->
        :stop

      :ignore ->
        case :maps.get(:filter_default, config) do
          :log ->
            log

          :stop ->
            :stop
        end

      log1 ->
        log1
    end
  end

  defp do_apply_filters(owner, log, [{_Id, {filterFun, filterArgs}} = filter | filters], state) do
    try do
      filterFun.(log, filterArgs)
    catch
      c, r ->
        handle_filter_failed(filter, owner, log, {c, r, filter_stacktrace(__STACKTRACE__)})
    else
      :stop ->
        :stop

      :ignore ->
        do_apply_filters(owner, log, filters, state)

      log1 = %{level: level, msg: msg, meta: meta}
      when (is_atom(level) and
              (is_tuple(msg) and tuple_size(msg) == 2) and
              is_list(
                :erlang.element(
                  1,
                  msg
                )
              ) and
              is_list(
                :erlang.element(
                  2,
                  msg
                )
              )) or
             (:erlang.element(
                1,
                msg
              ) == :report and
                (is_map(
                   :erlang.element(
                     2,
                     msg
                   )
                 ) or
                   (is_list(
                      :erlang.element(
                        2,
                        msg
                      )
                    ) and
                      is_tuple(
                        hd(
                          :erlang.element(
                            2,
                            msg
                          )
                        )
                      )))) or
             (:erlang.element(
                1,
                msg
              ) == :string and
                (is_list(
                   :erlang.element(
                     2,
                     msg
                   )
                 ) or
                   is_binary(
                     :erlang.element(
                       2,
                       msg
                     )
                   )) and
                is_map(meta)) ->
        do_apply_filters(owner, log1, filters, :log)

      bad ->
        handle_filter_failed(filter, owner, log, {:bad_return_value, bad})
    end
  end

  defp do_apply_filters(_Owner, _Log, [], :ignore) do
    :ignore
  end

  defp do_apply_filters(_Owner, log, [], :log) do
    log
  end

  defp handle_filter_failed({id, _} = filter, owner, log, reason) do
    case :logger_server.remove_filter(owner, id) do
      :ok ->
        :logger.internal_log(
          :error,
          {:removed_failing_filter, id}
        )

        case :logger.allow(:debug, :logger_backend) do
          true ->
            _ =
              :logger_server.do_internal_log(
                :debug,
                %{
                  mfa: {:logger_backend, :handle_filter_failed, 4},
                  line: 123,
                  file: 'otp/lib/kernel/src/logger_backend.erl'
                },
                log,
                [
                  [
                    {:logger, :removed_failing_filter},
                    {:filter, filter},
                    {:owner, owner},
                    {:log_event, log},
                    {:reason, reason}
                  ]
                ]
              )

            :ok

          false ->
            :ok
        end

      _ ->
        :ok
    end

    :ignore
  end

  defp filter_stacktrace(stacktrace) do
    :logger.filter_stacktrace(:logger_backend, stacktrace)
  end
end
