defmodule :m_error_logger do
  use Bitwise

  def warning_map() do
    :erlang.nif_error(:undef)
  end

  def start() do
    case :erlang.whereis(:error_logger) do
      :undefined ->
        errorLogger = %{
          :id => :error_logger,
          :start => {:error_logger, :start_link, []},
          :restart => :transient,
          :shutdown => 2000,
          :type => :worker,
          :modules => :dynamic
        }

        case :supervisor.start_child(
               :logger_sup,
               errorLogger
             ) do
          {:ok, pid} ->
            :ok =
              :logger_handler_watcher.register_handler(
                :error_logger,
                pid
              )

          error ->
            error
        end

      _ ->
        :ok
    end
  end

  def start_link() do
    :gen_event.start_link(
      {:local, :error_logger},
      [{:spawn_opt, [{:message_queue_data, :off_heap}]}]
    )
  end

  def stop() do
    case :erlang.whereis(:error_logger) do
      :undefined ->
        :ok

      _Pid ->
        _ = :gen_event.stop(:error_logger, {:shutdown, :stopped}, :infinity)
        _ = :supervisor.delete_child(:logger_sup, :error_logger)
        :ok
    end
  end

  def adding_handler(%{:id => :error_logger} = config) do
    case start() do
      :ok ->
        {:ok, config}

      error ->
        error
    end
  end

  def removing_handler(%{:id => :error_logger}) do
    stop()
    :ok
  end

  def log(
        %{:level => level, :msg => msg, :meta => meta},
        _Config
      ) do
    do_log(level, msg, meta)
  end

  defp do_log(level, {:report, msg}, %{:error_logger => %{:tag => tag, :type => type}} = meta) do
    report =
      case msg do
        %{:label => _, :report => r} ->
          r

        _ ->
          msg
      end

    notify(level, tag, type, report, meta)
  end

  defp do_log(level, {:report, msg}, %{:error_logger => %{:tag => tag}} = meta) do
    {format, args} =
      case msg do
        %{:label => _, :format => f, :args => a} ->
          {f, a}

        _ ->
          case get_report_cb(meta) do
            rCBFun when is_function(rCBFun, 1) ->
              try do
                rCBFun.(msg)
              catch
                c, r ->
                  {'REPORT_CB CRASH: ~tp; Reason: ~tp', [msg, {c, r, __STACKTRACE__}]}
              else
                {f, a} when is_list(f) and is_list(a) ->
                  {f, a}

                other ->
                  {'REPORT_CB ERROR: ~tp; Returned: ~tp', [msg, other]}
              end

            rCBFun when is_function(rCBFun, 2) ->
              try do
                rCBFun.(
                  msg,
                  %{
                    :depth => get_format_depth(),
                    :chars_limit => :unlimited,
                    :single_line => false
                  }
                )
              catch
                c, r ->
                  {'REPORT_CB CRASH: ~tp; Reason: ~tp', [msg, {c, r}]}
              else
                chardata
                when is_list(chardata) or is_binary(chardata) ->
                  {'~ts', [chardata]}

                other ->
                  {'REPORT_CB ERROR: ~tp; Returned: ~tp', [msg, other]}
              end
          end
      end

    notify(level, tag, format, args, meta)
  end

  defp do_log(level, {format, args}, %{:error_logger => %{:tag => tag}} = meta)
       when is_list(format) and is_list(args) do
    notify(level, tag, format, args, meta)
  end

  defp do_log(_Level, _Msg, _Meta) do
    :ok
  end

  defp notify(level, tag0, formatOrType0, argsOrReport, %{
         :pid => pid0,
         :gl => gL,
         :error_logger => my
       }) do
    {tag, formatOrType} = maybe_map_warnings(level, tag0, formatOrType0)

    pid =
      case :maps.get(:emulator, my, false) do
        true ->
          :emulator

        _ ->
          pid0
      end

    :gen_event.notify(
      :error_logger,
      {tag, gL, {pid, formatOrType, argsOrReport}}
    )
  end

  defp maybe_map_warnings(:warning, tag, formatOrType) do
    case :error_logger.warning_map() do
      :warning ->
        {tag, formatOrType}

      level ->
        {fix_warning_tag(level, tag), fix_warning_type(level, formatOrType)}
    end
  end

  defp maybe_map_warnings(_, tag, formatOrType) do
    {tag, formatOrType}
  end

  defp fix_warning_tag(:error, :warning_msg) do
    :error
  end

  defp fix_warning_tag(:error, :warning_report) do
    :error_report
  end

  defp fix_warning_tag(:info, :warning_msg) do
    :info_msg
  end

  defp fix_warning_tag(:info, :warning_report) do
    :info_report
  end

  defp fix_warning_tag(_, tag) do
    tag
  end

  defp fix_warning_type(:error, :std_warning) do
    :std_error
  end

  defp fix_warning_type(:info, :std_warning) do
    :std_info
  end

  defp fix_warning_type(_, type) do
    type
  end

  defp get_report_cb(%{:error_logger => %{:report_cb => rBFun}}) do
    rBFun
  end

  defp get_report_cb(%{:report_cb => rBFun}) do
    rBFun
  end

  defp get_report_cb(_) do
    &:logger.format_report/1
  end

  def error_msg(format) do
    error_msg(format, [])
  end

  def error_msg(format, args) do
    :logger.log(
      :error,
      %{:label => {:error_logger, :error_msg}, :format => format, :args => args},
      meta(:error)
    )
  end

  def format(format, args) do
    error_msg(format, args)
  end

  def error_report(report) do
    error_report(:std_error, report)
  end

  def error_report(type, report) do
    :logger.log(
      :error,
      %{:label => {:error_logger, :error_report}, :report => report},
      meta(:error_report, type)
    )
  end

  def warning_report(report) do
    warning_report(:std_warning, report)
  end

  def warning_report(type, report) do
    :logger.log(
      :warning,
      %{:label => {:error_logger, :warning_report}, :report => report},
      meta(:warning_report, type)
    )
  end

  def warning_msg(format) do
    warning_msg(format, [])
  end

  def warning_msg(format, args) do
    :logger.log(
      :warning,
      %{:label => {:error_logger, :warning_msg}, :format => format, :args => args},
      meta(:warning_msg)
    )
  end

  def info_report(report) do
    info_report(:std_info, report)
  end

  def info_report(type, report) do
    :logger.log(
      :notice,
      %{:label => {:error_logger, :info_report}, :report => report},
      meta(:info_report, type)
    )
  end

  def info_msg(format) do
    info_msg(format, [])
  end

  def info_msg(format, args) do
    :logger.log(
      :notice,
      %{:label => {:error_logger, :info_msg}, :format => format, :args => args},
      meta(:info_msg)
    )
  end

  def error_info(error) do
    {format, args} =
      case string_p(error) do
        true ->
          {error, []}

        false ->
          {'~p', [error]}
      end

    myMeta = %{:tag => :info, :type => error}
    :logger.log(:notice, format, args, %{:error_logger => myMeta, :domain => [error]})
  end

  defp meta(tag) do
    meta(tag, :undefined)
  end

  defp meta(tag, type) do
    meta(tag, type, %{:report_cb => &report_to_format/1})
  end

  defp meta(tag, :undefined, meta0) do
    %{meta0 | :error_logger => %{:tag => tag}}
  end

  defp meta(tag, type, meta0) do
    maybe_add_domain(tag, type, %{meta0 | :error_logger => %{:tag => tag, :type => type}})
  end

  defp maybe_add_domain(:error_report, :std_error, meta) do
    meta
  end

  defp maybe_add_domain(:info_report, :std_info, meta) do
    meta
  end

  defp maybe_add_domain(:warning_report, :std_warning, meta) do
    meta
  end

  defp maybe_add_domain(_, type, meta) do
    %{meta | :domain => [type]}
  end

  defp report_to_format(%{:label => {:error_logger, _}, :report => report})
       when is_map(report) do
    {'~tp\n', [report]}
  end

  defp report_to_format(%{:label => {:error_logger, _}, :format => format, :args => args}) do
    try do
      :io_lib.scan_format(format, args)
    catch
      _, _ ->
        {'ERROR: ~tp - ~tp', [format, args]}
    else
      _ ->
        {format, args}
    end
  end

  defp report_to_format(term) do
    :logger.format_otp_report(term)
  end

  defp string_p(list) when is_list(list) do
    string_p1(:lists.flatten(list))
  end

  defp string_p(_) do
    false
  end

  defp string_p1([]) do
    false
  end

  defp string_p1(flatList) do
    :io_lib.printable_list(flatList)
  end

  def add_report_handler(module) when is_atom(module) do
    add_report_handler(module, [])
  end

  def add_report_handler(module, args) when is_atom(module) do
    _ =
      :logger.add_handler(:error_logger, :error_logger, %{
        :level => :info,
        :filter_default => :log
      })

    :gen_event.add_handler(:error_logger, module, args)
  end

  def delete_report_handler(module) when is_atom(module) do
    case :erlang.whereis(:error_logger) do
      pid when is_pid(pid) ->
        return = :gen_event.delete_handler(:error_logger, module, [])

        case :gen_event.which_handlers(:error_logger) do
          [] ->
            _ = :logger.remove_handler(:error_logger)
            :ok

          _ ->
            :ok
        end

        return

      _ ->
        :ok
    end
  end

  def which_report_handlers() do
    case :erlang.whereis(:error_logger) do
      pid when is_pid(pid) ->
        :gen_event.which_handlers(:error_logger)

      :undefined ->
        []
    end
  end

  def logfile({:open, file}) do
    case :lists.member(
           :error_logger_file_h,
           which_report_handlers()
         ) do
      true ->
        {:error, :allready_have_logfile}

      _ ->
        add_report_handler(:error_logger_file_h, file)
    end
  end

  def logfile(:close) do
    case :erlang.whereis(:error_logger) do
      pid when is_pid(pid) ->
        case :gen_event.delete_handler(:error_logger, :error_logger_file_h, :normal) do
          {:error, reason} ->
            {:error, reason}

          _ ->
            :ok
        end

      _ ->
        {:error, :module_not_found}
    end
  end

  def logfile(:filename) do
    case :erlang.whereis(:error_logger) do
      pid when is_pid(pid) ->
        case :gen_event.call(:error_logger, :error_logger_file_h, :filename) do
          {:error, _} ->
            {:error, :no_log_file}

          val ->
            val
        end

      _ ->
        {:error, :no_log_file}
    end
  end

  def tty(true) do
    _ =
      case :lists.member(
             :error_logger_tty_h,
             which_report_handlers()
           ) do
        false ->
          case :logger.get_handler_config(:default) do
            {:ok, %{:module => :logger_std_h, :config => %{:type => :standard_io}}} ->
              :logger.remove_handler_filter(
                :default,
                :error_logger_tty_false
              )

            _ ->
              :logger.add_handler(
                :error_logger_tty_true,
                :logger_std_h,
                %{
                  :filter_default => :stop,
                  :filters => [
                    {:remote_gl, {&:logger_filters.remote_gl/2, :stop}},
                    {:domain, {&:logger_filters.domain/2, {:log, :super, [:otp]}}},
                    {:no_domain, {&:logger_filters.domain/2, {:log, :undefined, []}}}
                  ],
                  :formatter =>
                    {:logger_formatter, %{:legacy_header => true, :single_line => false}},
                  :config => %{:type => :standard_io}
                }
              )
          end

        true ->
          :ok
      end

    :ok
  end

  def tty(false) do
    delete_report_handler(:error_logger_tty_h)
    _ = :logger.remove_handler(:error_logger_tty_true)

    _ =
      case :logger.get_handler_config(:default) do
        {:ok, %{:module => :logger_std_h, :config => %{:type => :standard_io}}} ->
          :logger.add_handler_filter(
            :default,
            :error_logger_tty_false,
            {fn _, _ ->
               :stop
             end, :ok}
          )

        _ ->
          :ok
      end

    :ok
  end

  def limit_term(term) do
    case get_format_depth() do
      :unlimited ->
        term

      d ->
        :io_lib.limit_term(term, d)
    end
  end

  def get_format_depth() do
    case :application.get_env(
           :kernel,
           :error_logger_format_depth
         ) do
      {:ok, depth} when is_integer(depth) ->
        max(10, depth)

      {:ok, :unlimited} ->
        :unlimited

      :undefined ->
        :unlimited
    end
  end
end
