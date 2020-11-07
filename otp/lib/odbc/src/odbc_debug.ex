defmodule :m_odbc_debug do
  use Bitwise

  def trace_odbc(process, onOff) do
    trace_odbc(process, onOff, :exported)
  end

  defp trace_odbc(process, :on, :exported) do
    :dbg.tracer()
    :dbg.tp(:odbc, [{:_, [], [{:return_trace}]}])
    :dbg.p(process, [:call, :m])
    :ok
  end

  defp trace_odbc(process, :on, :all) do
    :dbg.tracer()
    :dbg.tpl(:odbc, [{:_, [], [{:return_trace}]}])
    :dbg.p(process, [:call, :m])
    :ok
  end

  defp trace_odbc(_Process, :off, _Level) do
    :dbg.stop()
    :ok
  end
end
