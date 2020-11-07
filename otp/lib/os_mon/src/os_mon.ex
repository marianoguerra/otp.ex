defmodule :m_os_mon do
  use Bitwise
  @behaviour :application
  @behaviour :supervisor
  def call(service, request) do
    call(service, request, 5000)
  end

  def call(service, request, timeout) do
    try do
      :gen_server.call(server_name(service), request, timeout)
    catch
      :exit, {:noproc, call} ->
        case :lists.keysearch(:os_mon, 1, :application.which_applications()) do
          {:value, _AppInfo} ->
            case startp(service) do
              true ->
                :erlang.exit({:noproc, call})

              false ->
                string = 'OS_MON (~p) called by ~p, unavailable~n'
                :error_logger.warning_msg(string, [service, self()])
                service.dummy_reply(request)
            end

          false ->
            string = 'OS_MON (~p) called by ~p, not started~n'
            :error_logger.warning_msg(string, [service, self()])
            service.dummy_reply(request)
        end
    end
  end

  def get_env(service, param) do
    case :application.get_env(:os_mon, param) do
      {:ok, value} ->
        case service.param_type(param, value) do
          true ->
            value

          false ->
            string =
              'OS_MON (~p), ignoring bad configuration parameter (~p=~p)~nUsing default value instead~n'

            :error_logger.warning_msg(
              string,
              [service, param, value]
            )

            service.param_default(param)
        end

      :undefined ->
        service.param_default(param)
    end
  end

  def open_port(name, opts) do
    privDir = :code.priv_dir(:os_mon)
    releasedPath = :filename.join([privDir, 'bin', name])

    case :filelib.is_regular(releasedPath) do
      true ->
        :erlang.open_port(
          {:spawn, '"' ++ releasedPath ++ '"'},
          opts
        )

      false ->
        archPath =
          :filename.join([privDir, 'bin', :erlang.system_info(:system_architecture), name])

        :erlang.open_port({:spawn, '"' ++ archPath ++ '"'}, opts)
    end
  end

  def start(_, _) do
    :supervisor.start_link({:local, :os_mon_sup}, :os_mon, [])
  end

  def stop(_) do
    :ok
  end

  def init([]) do
    supFlags =
      case :os.type() do
        {:win32, _} ->
          {:one_for_one, 5, 3600}

        _ ->
          {:one_for_one, 4, 3600}
      end

    sysInf = childspec(:sysinfo, startp(:sysinfo))
    dskSup = childspec(:disksup, startp(:disksup))
    memSup = childspec(:memsup, startp(:memsup))
    cpuSup = childspec(:cpu_sup, startp(:cpu_sup))
    osSup = childspec(:os_sup, startp(:os_sup))
    {:ok, {supFlags, sysInf ++ dskSup ++ memSup ++ cpuSup ++ osSup}}
  end

  defp childspec(_Service, false) do
    []
  end

  defp childspec(:cpu_sup, true) do
    [{:cpu_sup, {:cpu_sup, :start_link, []}, :permanent, 2000, :worker, [:cpu_sup]}]
  end

  defp childspec(:disksup, true) do
    [{:disksup, {:disksup, :start_link, []}, :permanent, 2000, :worker, [:disksup]}]
  end

  defp childspec(:memsup, true) do
    [{:memsup, {:memsup, :start_link, []}, :permanent, 2000, :worker, [:memsup]}]
  end

  defp childspec(:os_sup, true) do
    oS = :os.type()

    mod =
      case oS do
        {:win32, _} ->
          :nteventlog

        _ ->
          :os_sup
      end

    [{:os_sup, {:os_sup, :start_link, [oS]}, :permanent, 10000, :worker, [mod]}]
  end

  defp childspec(:sysinfo, true) do
    [
      {:os_mon_sysinfo, {:os_mon_sysinfo, :start_link, []}, :permanent, 2000, :worker,
       [:os_mon_sysinfo]}
    ]
  end

  defp startp(service) do
    case :lists.member(service, services(:os.type())) do
      true ->
        case start_param(service) do
          :none ->
            true

          param ->
            case :application.get_env(:os_mon, param) do
              {:ok, true} ->
                true

              _ ->
                false
            end
        end

      false ->
        false
    end
  end

  defp services({:unix, :sunos}) do
    [:cpu_sup, :disksup, :memsup, :os_sup]
  end

  defp services({:unix, _}) do
    [:cpu_sup, :disksup, :memsup]
  end

  defp services({:win32, _}) do
    [:disksup, :memsup, :os_sup, :sysinfo]
  end

  defp server_name(:cpu_sup) do
    :cpu_sup
  end

  defp server_name(:disksup) do
    :disksup
  end

  defp server_name(:memsup) do
    :memsup
  end

  defp server_name(:os_sup) do
    :os_sup_server
  end

  defp server_name(:sysinfo) do
    :os_mon_sysinfo
  end

  defp start_param(:cpu_sup) do
    :start_cpu_sup
  end

  defp start_param(:disksup) do
    :start_disksup
  end

  defp start_param(:memsup) do
    :start_memsup
  end

  defp start_param(:os_sup) do
    :start_os_sup
  end

  defp start_param(:sysinfo) do
    :none
  end
end
