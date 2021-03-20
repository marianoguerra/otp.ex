defmodule :cowboy_clock do
  use Bitwise
  @behaviour :gen_server
  require Record
  Record.defrecord(:r_state, :state, universaltime: :undefined,
                                 rfc1123: <<>>, tref: :undefined)
  def start_link() do
    :gen_server.start_link({:local, :cowboy_clock},
                             :cowboy_clock, [], [])
  end

  def stop() do
    :gen_server.call(:cowboy_clock, :stop)
  end

  def rfc1123() do
    try do
      :ets.lookup_element(:cowboy_clock, :rfc1123, 2)
    catch
      :error, :badarg ->
        rfc1123(:erlang.universaltime())
    end
  end

  def rfc1123(dateTime) do
    update_rfc1123(<<>>, :undefined, dateTime)
  end

  def init([]) do
    :cowboy_clock = :ets.new(:cowboy_clock,
                               [:set, :protected, :named_table,
                                                      {:read_concurrency,
                                                         true}])
    t = :erlang.universaltime()
    b = update_rfc1123(<<>>, :undefined, t)
    tRef = :erlang.send_after(1000, self(), :update)
    :ets.insert(:cowboy_clock, {:rfc1123, b})
    {:ok, r_state(universaltime: t, rfc1123: b, tref: tRef)}
  end

  def handle_call(:stop, _From, state) do
    {:stop, :normal, :stopped, state}
  end

  def handle_call(_Request, _From, state) do
    {:reply, :ignored, state}
  end

  def handle_cast(_Msg, state) do
    {:noreply, state}
  end

  def handle_info(:update,
           r_state(universaltime: prev, rfc1123: b1, tref: tRef0)) do
    _ = :erlang.cancel_timer(tRef0)
    t = :erlang.universaltime()
    b2 = update_rfc1123(b1, prev, t)
    :ets.insert(:cowboy_clock, {:rfc1123, b2})
    tRef = :erlang.send_after(1000, self(), :update)
    {:noreply, r_state(universaltime: t, rfc1123: b2, tref: tRef)}
  end

  def handle_info(_Info, state) do
    {:noreply, state}
  end

  def terminate(_Reason, _State) do
    :ok
  end

  def code_change(_OldVsn, state, _Extra) do
    {:ok, state}
  end

  defp update_rfc1123(bin, now, now) do
    bin
  end

  defp update_rfc1123(<<keep :: size(23) - binary, _ :: bits>>,
            {date, {h, m, _}}, {date, {h, m, s}}) do
    <<keep :: binary, pad_int(s) :: binary, " GMT">>
  end

  defp update_rfc1123(<<keep :: size(20) - binary, _ :: bits>>,
            {date, {h, _, _}}, {date, {h, m, s}}) do
    <<keep :: binary, pad_int(m) :: binary, ?:,
        pad_int(s) :: binary, " GMT">>
  end

  defp update_rfc1123(<<keep :: size(17) - binary, _ :: bits>>,
            {date, _}, {date, {h, m, s}}) do
    <<keep :: binary, pad_int(h) :: binary, ?:,
        pad_int(m) :: binary, ?:, pad_int(s) :: binary, " GMT">>
  end

  defp update_rfc1123(<<_ :: size(7) - binary,
              keep :: size(10) - binary, _ :: bits>>,
            {{y, mo, _}, _}, {date = {y, mo, d}, {h, m, s}}) do
    wday = :calendar.day_of_the_week(date)
    <<weekday(wday) :: binary, ", ", pad_int(d) :: binary,
        keep :: binary, pad_int(h) :: binary, ?:,
        pad_int(m) :: binary, ?:, pad_int(s) :: binary, " GMT">>
  end

  defp update_rfc1123(<<_ :: size(11) - binary,
              keep :: size(6) - binary, _ :: bits>>,
            {{y, _, _}, _}, {date = {y, mo, d}, {h, m, s}}) do
    wday = :calendar.day_of_the_week(date)
    <<weekday(wday) :: binary, ", ", pad_int(d) :: binary, " ",
        month(mo) :: binary, keep :: binary,
        pad_int(h) :: binary, ?:, pad_int(m) :: binary, ?:,
        pad_int(s) :: binary, " GMT">>
  end

  defp update_rfc1123(_, _, {date = {y, mo, d}, {h, m, s}}) do
    wday = :calendar.day_of_the_week(date)
    <<weekday(wday) :: binary, ", ", pad_int(d) :: binary, " ",
        month(mo) :: binary, " ",
        :erlang.integer_to_binary(y) :: binary, " ",
        pad_int(h) :: binary, ?:, pad_int(m) :: binary, ?:,
        pad_int(s) :: binary, " GMT">>
  end

  defp pad_int(x) when x < 10 do
    <<?0, ?0 + x>>
  end

  defp pad_int(x) do
    :erlang.integer_to_binary(x)
  end

  defp weekday(1) do
    "Mon"
  end

  defp weekday(2) do
    "Tue"
  end

  defp weekday(3) do
    "Wed"
  end

  defp weekday(4) do
    "Thu"
  end

  defp weekday(5) do
    "Fri"
  end

  defp weekday(6) do
    "Sat"
  end

  defp weekday(7) do
    "Sun"
  end

  defp month(1) do
    "Jan"
  end

  defp month(2) do
    "Feb"
  end

  defp month(3) do
    "Mar"
  end

  defp month(4) do
    "Apr"
  end

  defp month(5) do
    "May"
  end

  defp month(6) do
    "Jun"
  end

  defp month(7) do
    "Jul"
  end

  defp month(8) do
    "Aug"
  end

  defp month(9) do
    "Sep"
  end

  defp month(10) do
    "Oct"
  end

  defp month(11) do
    "Nov"
  end

  defp month(12) do
    "Dec"
  end

end