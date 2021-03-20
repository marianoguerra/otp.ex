defmodule :m_logger_proxy do
  use Bitwise

  def log(remoteLog) do
    olp = :persistent_term.get(:logger_proxy, :undefined)

    case olp === :undefined or :logger_olp.get_pid(olp) === self() do
      true ->
        _ = handle_load(remoteLog, :no_state)
        :ok

      false ->
        :logger_olp.load(olp, remoteLog)
    end
  end

  def start_link() do
    :logger_olp.start_link(:logger_proxy, :logger_proxy, [], :logger.get_proxy_config())
  end

  def restart() do
    case :supervisor.start_child(
           :logger_sup,
           child_spec()
         ) do
      {:ok, _Pid, olp} ->
        {:ok, olp}

      {:error, {reason, ch}}
      when is_tuple(ch) and
             :erlang.element(1, ch) == :child ->
        {:error, reason}

      error ->
        error
    end
  end

  def child_spec() do
    name = :logger_proxy

    %{
      id: name,
      start: {:logger_proxy, :start_link, []},
      restart: :temporary,
      shutdown: 2000,
      type: :worker,
      modules: [:logger_proxy]
    }
  end

  def get_default_config() do
    olpDefault = :logger_olp.get_default_opts()

    Map.merge(olpDefault, %{
      sync_mode_qlen: 500,
      drop_mode_qlen: 1000,
      flush_qlen: 5000,
      burst_limit_enable: false
    })
  end

  def init([]) do
    :erlang.process_flag(:trap_exit, true)
    _ = :erlang.system_flag(:system_logger, self())

    :persistent_term.put(
      :logger_proxy,
      :logger_olp.get_ref()
    )

    {:ok, :no_state}
  end

  def handle_load({:remote, node, log}, state) do
    case :erlang.send({:logger_proxy, node}, log, [:nosuspend]) do
      _ok_or_nosuspend ->
        :ok
    end

    state
  end

  def handle_load({:log, level, format, args, meta}, state) do
    try_log([level, format, args, meta])
    state
  end

  def handle_load({:log, level, report, meta}, state) do
    try_log([level, report, meta])
    state
  end

  def handle_info(log, state)
      when is_tuple(log) and
             :erlang.element(1, log) == :log do
    {:load, state}
  end

  def terminate(:overloaded, _State) do
    _ = :erlang.system_flag(:system_logger, :undefined)
    {:ok, &:logger_proxy.restart/0}
  end

  def terminate(_Reason, _State) do
    _ =
      :erlang.system_flag(
        :system_logger,
        :erlang.whereis(:logger)
      )

    :ok
  end

  def notify({:mode_change, mode0, mode1}, state) do
    _ =
      cond do
        mode1 === :drop ->
          :erlang.system_flag(:system_logger, :undefined)

        mode0 === :drop ->
          :erlang.system_flag(:system_logger, self())

        true ->
          :ok
      end

    case :logger.allow(:notice, :logger_proxy) do
      true ->
        _ =
          :logger_server.do_internal_log(
            :notice,
            %{
              mfa: {:logger_proxy, :notify, 2},
              line: 154,
              file: 'otp/lib/kernel/src/logger_proxy.erl'
            },
            %{},
            ['~w switched from ~w to ~w mode', [:logger_proxy, mode0, mode1]]
          )

        :ok

      false ->
        :ok
    end

    state
  end

  def notify({:flushed, flushed}, state) do
    case :logger.allow(:notice, :logger_proxy) do
      true ->
        _ =
          :logger_server.do_internal_log(
            :notice,
            %{
              mfa: {:logger_proxy, :notify, 2},
              line: 157,
              file: 'otp/lib/kernel/src/logger_proxy.erl'
            },
            %{},
            ['~w flushed ~w log events', [:logger_proxy, flushed]]
          )

        :ok

      false ->
        :ok
    end

    state
  end

  def notify(:restart, state) do
    case :logger.allow(:notice, :logger_proxy) do
      true ->
        _ =
          :logger_server.do_internal_log(
            :notice,
            %{
              mfa: {:logger_proxy, :notify, 2},
              line: 160,
              file: 'otp/lib/kernel/src/logger_proxy.erl'
            },
            %{},
            ['~w restarted', [:logger_proxy]]
          )

        :ok

      false ->
        :ok
    end

    state
  end

  def notify(_Note, state) do
    state
  end

  defp try_log(args) do
    try do
      apply(:logger, :log, args)
    catch
      c, r ->
        case :logger.allow(:debug, :logger_proxy) do
          true ->
            _ =
              :logger_server.do_internal_log(
                :debug,
                %{
                  mfa: {:logger_proxy, :try_log, 1},
                  line: 170,
                  file: 'otp/lib/kernel/src/logger_proxy.erl'
                },
                %{},
                [[{:logger_proxy, :log_failed}, {:log, args}, {:reason, {c, r, __STACKTRACE__}}]]
              )

            :ok

          false ->
            :ok
        end
    end
  end
end
