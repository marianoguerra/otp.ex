defmodule :m_alarm_handler do
  use Bitwise

  def start_link() do
    case :gen_event.start_link({:local, :alarm_handler}) do
      {:ok, pid} ->
        :gen_event.add_handler(:alarm_handler, :alarm_handler, [])
        {:ok, pid}

      error ->
        error
    end
  end

  def set_alarm(alarm) do
    :gen_event.notify(:alarm_handler, {:set_alarm, alarm})
  end

  def clear_alarm(alarmId) do
    :gen_event.notify(
      :alarm_handler,
      {:clear_alarm, alarmId}
    )
  end

  def get_alarms() do
    :gen_event.call(:alarm_handler, :alarm_handler, :get_alarms)
  end

  def add_alarm_handler(module) when is_atom(module) do
    :gen_event.add_handler(:alarm_handler, module, [])
  end

  def add_alarm_handler(module, args) when is_atom(module) do
    :gen_event.add_handler(:alarm_handler, module, args)
  end

  def delete_alarm_handler(module) when is_atom(module) do
    :gen_event.delete_handler(:alarm_handler, module, [])
  end

  def init(_) do
    {:ok, []}
  end

  def handle_event({:set_alarm, alarm}, alarms) do
    :error_logger.info_report([{:alarm_handler, {:set, alarm}}])
    {:ok, [alarm | alarms]}
  end

  def handle_event({:clear_alarm, alarmId}, alarms) do
    :error_logger.info_report([{:alarm_handler, {:clear, alarmId}}])
    {:ok, :lists.keydelete(alarmId, 1, alarms)}
  end

  def handle_event(_, alarms) do
    {:ok, alarms}
  end

  def handle_info(_, alarms) do
    {:ok, alarms}
  end

  def handle_call(:get_alarms, alarms) do
    {:ok, alarms, alarms}
  end

  def handle_call(_Query, alarms) do
    {:ok, {:error, :bad_query}, alarms}
  end

  def terminate(:swap, alarms) do
    {:alarm_handler, alarms}
  end

  def terminate(_, _) do
    :ok
  end
end
