defmodule :m_sasl do
  use Bitwise
  @behaviour :application
  require Record

  Record.defrecord(:r_state, :state,
    sasl_logger: :undefined,
    error_logger_mf: :undefined
  )

  def start(_, []) do
    {dest, level} = get_logger_info()
    mf = get_error_logger_mf()
    add_sasl_logger(dest, level)
    add_error_logger_mf(mf)
    state = r_state(sasl_logger: dest, error_logger_mf: mf)

    case :supervisor.start_link({:local, :sasl_sup}, :sasl, []) do
      {:ok, pid} ->
        {:ok, pid, state}

      error ->
        error
    end
  end

  def stop(state) do
    delete_sasl_logger(r_state(state, :sasl_logger))
    delete_error_logger_mf(r_state(state, :error_logger_mf))
  end

  defp get_logger_info() do
    case :application.get_env(
           :kernel,
           :logger_sasl_compatible
         ) do
      {:ok, true} ->
        {get_logger_dest(), get_logger_level()}

      _ ->
        {:std, :undefined}
    end
  end

  defp get_logger_dest() do
    case :application.get_env(
           :sasl,
           :sasl_error_logger
         ) do
      {:ok, false} ->
        :undefined

      {:ok, :tty} ->
        :standard_io

      {:ok, {:file, file}} when is_list(file) ->
        {:file, file}

      {:ok, {:file, file, modes}}
      when is_list(file) and
             is_list(modes) ->
        {:file, file, modes}

      {:ok, bad} ->
        exit({:bad_config, {:sasl, {:sasl_logger_dest, bad}}})

      :undefined ->
        :standard_io
    end
  end

  defp get_logger_level() do
    case :application.get_env(:sasl, :errlog_type) do
      {:ok, :error} ->
        :error

      {:ok, :progress} ->
        :info

      {:ok, :all} ->
        :info

      {:ok, bad} ->
        exit({:bad_config, {:sasl, {:errlog_type, bad}}})

      _ ->
        :info
    end
  end

  defp get_error_logger_mf() do
    case (try do
            get_mf()
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, reason} ->
        exit(reason)

      mf ->
        mf
    end
  end

  defp get_mf() do
    dir = get_mf_dir()
    maxB = get_mf_maxb()
    maxF = get_mf_maxf()

    case {dir, maxB, maxF} do
      {:undefined, :undefined, :undefined} ->
        :undefined

      {:undefined, _, _} ->
        exit({:missing_config, {:sasl, :error_logger_mf_dir}})

      {_, :undefined, _} ->
        exit({:missing_config, {:sasl, :error_logger_mf_maxbytes}})

      {_, _, :undefined} ->
        exit({:missing_config, {:sasl, :error_logger_mf_maxfiles}})

      r ->
        r
    end
  end

  defp get_mf_dir() do
    case :application.get_env(
           :sasl,
           :error_logger_mf_dir
         ) do
      {:ok, false} ->
        :undefined

      {:ok, dir} when is_list(dir) ->
        dir

      :undefined ->
        :undefined

      {:ok, bad} ->
        exit({:bad_config, {:sasl, {:error_logger_mf_dir, bad}}})
    end
  end

  defp get_mf_maxb() do
    case :application.get_env(
           :sasl,
           :error_logger_mf_maxbytes
         ) do
      {:ok, maxB} when is_integer(maxB) ->
        maxB

      :undefined ->
        :undefined

      {:ok, bad} ->
        exit({:bad_config, {:sasl, {:error_logger_mf_maxbytes, bad}}})
    end
  end

  defp get_mf_maxf() do
    case :application.get_env(
           :sasl,
           :error_logger_mf_maxfiles
         ) do
      {:ok, maxF}
      when is_integer(maxF) and maxF > 0 and
             maxF < 256 ->
        maxF

      :undefined ->
        :undefined

      {:ok, bad} ->
        exit({:bad_config, {:sasl, {:error_logger_mf_maxfiles, bad}}})
    end
  end

  defp add_sasl_logger(:undefined, _Level) do
    :ok
  end

  defp add_sasl_logger(:std, :undefined) do
    :ok
  end

  defp add_sasl_logger(dest, level) do
    fC = %{legacy_header: true, single_line: false}

    case level do
      :info ->
        allow_progress()

      _ ->
        :ok
    end

    :ok =
      :logger.add_handler(:sasl, :logger_std_h, %{
        level: level,
        filter_default: :stop,
        filters: [
          {:remote_gl, {&:logger_filters.remote_gl/2, :stop}},
          {:sasl_domain, {&:logger_filters.domain/2, {:log, :equal, [:otp, :sasl]}}}
        ],
        config: %{type: dest},
        formatter: {:logger_formatter, fC}
      })
  end

  defp delete_sasl_logger(:undefined) do
    :ok
  end

  defp delete_sasl_logger(:std) do
    :ok
  end

  defp delete_sasl_logger(_Type) do
    _ = :logger.remove_handler(:sasl)
    :ok
  end

  defp add_error_logger_mf(:undefined) do
    :ok
  end

  defp add_error_logger_mf({dir, maxB, maxF}) do
    allow_progress()

    :error_logger.add_report_handler(
      :log_mf_h,
      :log_mf_h.init(dir, maxB, maxF, &pred/1)
    )
  end

  defp delete_error_logger_mf(:undefined) do
    :ok
  end

  defp delete_error_logger_mf(_) do
    :error_logger.delete_report_handler(:log_mf_h)
  end

  def pred({_Type, gL, _Msg}) when node(gL) !== node() do
    false
  end

  def pred(_) do
    true
  end

  defp allow_progress() do
    %{level: pL} = :logger.get_primary_config()

    case :logger.compare_levels(:info, pL) do
      :lt ->
        :ok = :logger.set_primary_config(:level, :info)

      _ ->
        :ok
    end
  end

  def init([]) do
    supFlags = {:one_for_one, 0, 1}

    safeSupervisor =
      {:sasl_safe_sup, {:supervisor, :start_link, [{:local, :sasl_safe_sup}, :sasl, :safe]},
       :permanent, :infinity, :supervisor, [:sasl]}

    releaseH =
      {:release_handler, {:release_handler, :start_link, []}, :permanent, 2000, :worker, []}

    {:ok, {supFlags, [safeSupervisor, releaseH]}}
  end

  def init(:safe) do
    supFlags = {:one_for_one, 4, 3600}

    alarmH =
      {:alarm_handler, {:alarm_handler, :start_link, []}, :permanent, 2000, :worker, :dynamic}

    {:ok, {supFlags, [alarmH]}}
  end
end
