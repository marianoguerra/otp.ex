defmodule :m_et_viewer do
  use Bitwise
  require Record

  Record.defrecord(:r_event, :event,
    detail_level: :undefined,
    trace_ts: :undefined,
    event_ts: :undefined,
    from: :undefined,
    to: :undefined,
    label: :undefined,
    contents: :undefined
  )

  Record.defrecord(:r_filter, :filter,
    name: :undefined,
    function: :undefined
  )

  def file(fileName) do
    start_link(
      [{:trace_client, {:file, fileName}}],
      :default
    )
  end

  def start() do
    start([{:trace_global, true}], :default)
  end

  def start(gUI) when gUI === :wx or gUI === :default do
    start_link([{:trace_global, true}], gUI)
  end

  def start(options) do
    start_link(
      [{:parent_pid, :undefined} | options],
      :default
    )
  end

  def start(options, gUI) do
    start_link([{:parent_pid, :undefined} | options], gUI)
  end

  def start_link(gUI) when gUI === :wx or gUI === :default do
    start_link([{:trace_global, true}], gUI)
  end

  def start_link(options) do
    start_link(options, :default)
  end

  def start_link(options, gUI) do
    case gUI do
      :wx ->
        :et_wx_viewer.start_link(options)

      :default ->
        start_link(options, which_gui())
    end
  end

  defp which_gui() do
    :wx
  end

  def get_collector_pid(viewerPid) do
    call(viewerPid, :get_collector_pid)
  end

  def stop(viewerPid) do
    call(viewerPid, :stop)
  end

  def open_event(viewerPid, n) do
    call(viewerPid, {:open_event, n})
  end

  defp call(viewerPid, request) do
    :gen_server.call(viewerPid, request, :infinity)
  end
end
