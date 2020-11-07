defmodule :m_os_sup do
  use Bitwise
  @behaviour :gen_server
  require Record

  Record.defrecord(:r_state, :state,
    port: :undefined,
    mfa: :undefined,
    config: :undefined,
    path: :undefined,
    conf: :undefined
  )

  def start_link({:win32, _OSname}) do
    identifier = :os_sup
    mFA = :os_mon.get_env(:os_sup, :os_sup_mfa)
    :gen_server.start_link({:local, :os_sup_server}, :nteventlog, [identifier, mFA], [])
  end

  def start_link(_OS) do
    :gen_server.start_link({:local, :os_sup_server}, :os_sup, [], [])
  end

  def start() do
    :gen_server.start({:local, :os_sup_server}, :os_sup, [], [])
  end

  def stop() do
    :gen_server.call(:os_sup_server, :stop)
  end

  def error_report(logData, tag) do
    :error_logger.error_report(tag, logData)
  end

  def enable() do
    command(:enable)
  end

  def enable(path, conf) do
    command(:enable, path, conf)
  end

  def disable() do
    command(:disable)
  end

  def disable(path, conf) do
    command(:disable, path, conf)
  end

  def param_type(:os_sup_errortag, val) when is_atom(val) do
    true
  end

  def param_type(:os_sup_own, val) do
    :io_lib.printable_list(val)
  end

  def param_type(:os_sup_syslogconf, val) do
    :io_lib.printable_list(val)
  end

  def param_type(:os_sup_enable, val)
      when val == true or
             val == false do
    true
  end

  def param_type(:os_sup_mfa, {mod, func, args})
      when is_atom(mod) and is_atom(func) and
             is_list(args) do
    true
  end

  def param_type(_Param, _Val) do
    false
  end

  def param_default(:os_sup_errortag) do
    :std_error
  end

  def param_default(:os_sup_own) do
    '/etc'
  end

  def param_default(:os_sup_syslogconf) do
    '/etc/syslog.conf'
  end

  def param_default(:os_sup_enable) do
    true
  end

  def param_default(:os_sup_mfa) do
    {:os_sup, :error_report, [:std_error]}
  end

  def init([]) do
    :erlang.process_flag(:trap_exit, true)
    :erlang.process_flag(:priority, :low)

    case :os.type() do
      {:unix, :sunos} ->
        init2()

      oS ->
        {:stop, {:unsupported_os, oS}}
    end
  end

  defp init2() do
    configP = :os_mon.get_env(:os_sup, :os_sup_enable)

    case configP do
      true ->
        path = :os_mon.get_env(:os_sup, :os_sup_own)
        conf = :os_mon.get_env(:os_sup, :os_sup_syslogconf)

        case enable(path, conf) do
          :ok ->
            init3(r_state(config: configP, path: path, conf: conf))

          {:error, error} ->
            {:stop, {:mod_syslog, error}}
        end

      false ->
        init3(r_state(config: configP))
    end
  end

  defp init3(state0) do
    port = start_portprogram()

    mFA =
      case :os_mon.get_env(:os_sup, :os_sup_mfa) do
        {:os_sup, :error_report, _} ->
          tag = :os_mon.get_env(:os_sup, :os_sup_errortag)
          {:os_sup, :error_report, [tag]}

        mFA0 ->
          mFA0
      end

    {:ok, r_state(state0, port: port, mfa: mFA)}
  end

  def handle_call(:stop, _From, state) do
    {:stop, :normal, :ok, state}
  end

  def handle_cast(_Msg, state) do
    {:noreply, state}
  end

  def handle_info(
        {_Port, {:data, data}},
        r_state(mfa: {m, f, a}) = state
      ) do
    apply(m, f, [data | a])
    {:noreply, state}
  end

  def handle_info({:EXIT, _Port, reason}, state) do
    {:stop, {:port_died, reason}, r_state(state, port: :not_used)}
  end

  def handle_info(_Info, state) do
    {:noreply, state}
  end

  def terminate(_Reason, r_state(port: port) = state) do
    case r_state(state, :config) do
      true when is_port(port) ->
        send(port, {self(), {:command, 'only_stdin'}})
        res = disable(r_state(state, :path), r_state(state, :conf))
        :erlang.port_close(port)

        cond do
          res != '0' ->
            exit({:mod_syslog, res})

          true ->
            :ok
        end

      true ->
        res = disable(r_state(state, :path), r_state(state, :conf))

        cond do
          res != '0' ->
            exit({:mod_syslog, res})

          true ->
            :ok
        end

      false when is_port(port) ->
        send(port, {self(), {:command, 'only_stdin'}})
        :erlang.port_close(port)

      false ->
        :ok
    end
  end

  defp start_portprogram() do
    ownPath = :os_mon.get_env(:os_sup, :os_sup_own)

    command =
      '"' ++ :filename.join([:code.priv_dir(:os_mon), 'bin', 'ferrule']) ++ '" ' ++ ownPath

    :erlang.open_port({:spawn, command}, [{:packet, 2}])
  end

  defp command(mode) do
    command(mode, '/etc', '/etc/syslog.conf')
  end

  defp command(mode, path, conf) do
    case :os.cmd(cmd_str(mode, path, conf)) do
      '0' ->
        :ok

      error ->
        {:error, error}
    end
  end

  defp cmd_str(mode, path, conf) do
    privDir = :code.priv_dir(:os_mon)

    modeSw =
      case mode do
        :enable ->
          ' otp '

        :disable ->
          ' nootp '
      end

    privDir ++ '/bin/mod_syslog' ++ modeSw ++ path ++ ' ' ++ conf
  end
end
