defmodule :m_sasl_report_tty_h do
  use Bitwise

  def init(type) do
    {:ok, type}
  end

  def handle_event({type, gL, _Msg}, type)
      when node(gL) != node() do
    {:ok, type}
  end

  def handle_event(event, type) do
    _ = :sasl_report.write_report(:standard_io, type, tag_event(event))
    {:ok, type}
  end

  def handle_info(_, type) do
    {:ok, type}
  end

  def handle_call(_Query, _Type) do
    {:error, :bad_query}
  end

  def terminate(_Reason, _Type) do
    []
  end

  defp tag_event(event) do
    {:calendar.local_time(), event}
  end
end
