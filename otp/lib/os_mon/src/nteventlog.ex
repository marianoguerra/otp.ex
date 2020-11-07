defmodule :m_nteventlog do
  use Bitwise
  @behaviour :gen_server
  require Record

  Record.defrecord(:r_state, :state,
    port: :undefined,
    mfa: :undefined
  )

  def start_link(ident, mFA) do
    :gen_server.start_link({:local, :nteventlog}, :nteventlog, [ident, mFA], [])
  end

  def start(ident, mFA) do
    :gen_server.start({:local, :nteventlog}, :nteventlog, [ident, mFA], [])
  end

  def stop() do
    :gen_server.call(:nteventlog, :stop)
  end

  def init([identifier, mFA0]) do
    :erlang.process_flag(:trap_exit, true)
    :erlang.process_flag(:priority, :low)

    port =
      case :os.type() do
        {:win32, _OSname} ->
          start_portprogram(identifier)

        oS ->
          exit({:unsupported_os, oS})
      end

    mFA =
      case mFA0 do
        {:os_sup, :error_report, [_Tag]} ->
          tag = :os_mon.get_env(:os_sup, :os_sup_errortag)
          {:os_sup, :error_report, [tag]}

        _ ->
          mFA0
      end

    {:ok, r_state(port: port, mfa: mFA)}
  end

  def handle_call(:stop, _From, state) do
    {:stop, :normal, :stopped, state}
  end

  def handle_cast(_Msg, state) do
    {:noreply, state}
  end

  def handle_info(
        {_Port, {:data, data}},
        r_state(mfa: {m, f, a}) = state
      ) do
    t = parse_log(data)
    apply(m, f, [t | a])
    send(r_state(state, :port), {self(), {:command, 'A'}})
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
        :ignore

      port ->
        :erlang.port_close(port)
    end

    :ok
  end

  defp start_portprogram(identifier) do
    command =
      '"' ++
        :filename.join([:code.priv_dir(:os_mon), 'bin', 'nteventlog.exe']) ++
        '" ' ++ make_list(identifier)

    :erlang.open_port({:spawn, command}, [{:packet, 2}])
  end

  defp make_list(x) when is_atom(x) do
    :erlang.atom_to_list(x)
  end

  defp make_list(x) do
    x
  end

  defp holl_len([?H | rest], sum) do
    {sum, rest}
  end

  defp holl_len([n | rest], sum) do
    nN = n - ?0
    holl_len(rest, sum * 10 + nN)
  end

  defp holl_len(l) do
    holl_len(l, 0)
  end

  defp splitlist(l, n) do
    {:lists.sublist(l, n), :lists.nthtail(n, l)}
  end

  defp hollerith(str) do
    {len, rest} = holl_len(str)
    splitlist(rest, len)
  end

  defp holl_time(str) do
    {holl, rest} = hollerith(str)
    rev = :lists.reverse(holl)

    b =
      :erlang.list_to_integer(
        :lists.reverse(
          :lists.sublist(
            rev,
            6
          )
        )
      )

    a =
      :erlang.list_to_integer(
        :lists.reverse(
          :lists.nthtail(
            6,
            rev
          )
        )
      )

    {{a, b, 0}, rest}
  end

  defp parse_log(str) do
    {time, rest1} = holl_time(str)
    {category, rest2} = hollerith(rest1)
    {facility, rest3} = hollerith(rest2)
    {severity, rest4} = hollerith(rest3)
    {message, _} = hollerith(rest4)
    {time, category, facility, severity, message}
  end
end
