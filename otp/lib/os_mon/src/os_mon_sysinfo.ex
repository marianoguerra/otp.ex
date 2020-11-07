defmodule :m_os_mon_sysinfo do
  use Bitwise
  @behaviour :gen_server
  require Record
  Record.defrecord(:r_state, :state, port: :undefined)

  def start_link() do
    :gen_server.start_link({:local, :os_mon_sysinfo}, :os_mon_sysinfo, [], [])
  end

  def get_disk_info() do
    :gen_server.call(:os_mon_sysinfo, :get_disk_info)
  end

  def get_disk_info(driveRoot) do
    :gen_server.call(
      :os_mon_sysinfo,
      {:get_disk_info, driveRoot}
    )
  end

  def get_mem_info() do
    :gen_server.call(:os_mon_sysinfo, :get_mem_info)
  end

  def init([]) do
    :erlang.process_flag(:trap_exit, true)
    :erlang.process_flag(:priority, :low)

    port =
      case :os.type() do
        {:win32, _OSname} ->
          start_portprogram()

        oS ->
          exit({:unsupported_os, oS})
      end

    {:ok, r_state(port: port)}
  end

  def handle_call(:get_disk_info, _From, state) do
    {:reply, get_disk_info1(r_state(state, :port)), state}
  end

  def handle_call({:get_disk_info, rootList}, _From, state) do
    {:reply, get_disk_info1(r_state(state, :port), rootList), state}
  end

  def handle_call(:get_mem_info, _From, state) do
    {:reply, get_mem_info1(r_state(state, :port)), state}
  end

  def handle_cast(_Msg, state) do
    {:noreply, state}
  end

  def handle_info({:EXIT, _Port, reason}, state) do
    {:stop, {:port_died, reason}, r_state(state, port: :not_used)}
  end

  def handle_info(_Info, state) do
    {:noreply, state}
  end

  def terminate(_Reason, state) do
    case r_state(state, :port) do
      :not_used ->
        :ok

      port ->
        :erlang.port_close(port)
    end

    :ok
  end

  defp start_portprogram() do
    port = :os_mon.open_port('win32sysinfo.exe', [{:packet, 1}])

    receive do
      {^port, {:data, [?o]}} ->
        port

      {^port, {:data, data}} ->
        exit({:port_error, data})

      {:EXIT, ^port, reason} ->
        exit({:port_died, reason})
    after
      5000 ->
        exit({:port_error, :timeout})
    end
  end

  defp get_disk_info1(port) do
    send(port, {self(), {:command, [?d]}})
    get_data(port, [])
  end

  defp get_disk_info1(port, pathList) do
    send(
      port,
      {self(),
       {:command,
        [
          ?d
          | for p <- pathList do
              p ++ [0]
            end
        ]}}
    )

    get_data(port, [])
  end

  defp get_mem_info1(port) do
    send(port, {self(), {:command, [?m]}})
    get_data(port, [])
  end

  defp get_data(port, sofar) do
    receive do
      {^port, {:data, [?o]}} ->
        :lists.reverse(sofar)

      {^port, {:data, bytes}} ->
        get_data(port, [bytes | sofar])

      {:EXIT, ^port, reason} ->
        exit({:port_died, reason})
    after
      5000 ->
        :lists.reverse(sofar)
    end
  end
end
