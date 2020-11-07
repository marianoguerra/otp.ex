defmodule :m_ct_event do
  use Bitwise
  @behaviour :gen_event
  require Record
  Record.defrecord(:r_event, :event, name: :undefined, node: :undefined, data: :undefined)

  Record.defrecord(:r_conn, :conn,
    handle: :undefined,
    targetref: :undefined,
    address: :undefined,
    callback: :undefined
  )

  Record.defrecord(:r_testspec, :testspec,
    spec_dir: :undefined,
    nodes: [],
    init: [],
    label: [],
    profile: [],
    logdir: ['.'],
    logopts: [],
    basic_html: [],
    esc_chars: [],
    verbosity: [],
    silent_connections: [],
    cover: [],
    cover_stop: [],
    config: [],
    userconfig: [],
    event_handler: [],
    ct_hooks: [],
    enable_builtin_hooks: true,
    release_shell: false,
    include: [],
    auto_compile: [],
    abort_if_missing_suites: [],
    stylesheet: [],
    multiply_timetraps: [],
    scale_timetraps: [],
    create_priv_dir: [],
    alias: [],
    tests: [],
    unknown: [],
    merge_tests: true
  )

  Record.defrecord(:r_cover, :cover,
    app: :none,
    local_only: false,
    level: :details,
    excl_mods: [],
    incl_mods: [],
    cross: [],
    src: []
  )

  Record.defrecord(:r_conn_log, :conn_log,
    header: true,
    client: :undefined,
    name: :undefined,
    address: :undefined,
    conn_pid: :undefined,
    action: :undefined,
    module: :undefined
  )

  Record.defrecord(:r_state, :state, receivers: [])

  def start_link() do
    :gen_event.start_link({:local, :ct_event})
  end

  def add_handler() do
    :gen_event.add_handler(:ct_event, :ct_event, [])
  end

  def add_handler(args) do
    :gen_event.add_handler(:ct_event, :ct_event, args)
  end

  def stop() do
    case :erlang.whereis(:ct_event) do
      :undefined ->
        :ok

      _Pid ->
        :gen_event.stop(:ct_event)
    end
  end

  def notify(event) do
    case (try do
            :gen_event.notify(:ct_event, event)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, reason} ->
        {:error, {:notify, reason}}

      result ->
        result
    end
  end

  def notify(name, data) do
    notify(r_event(name: name, data: data))
  end

  def sync_notify(event) do
    case (try do
            :gen_event.sync_notify(:ct_event, event)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, reason} ->
        {:error, {:sync_notify, reason}}

      result ->
        result
    end
  end

  def sync_notify(name, data) do
    sync_notify(r_event(name: name, data: data))
  end

  def is_alive() do
    case :erlang.whereis(:ct_event) do
      :undefined ->
        false

      _Pid ->
        true
    end
  end

  def init(recvPids) do
    :ct_util.mark_process()
    {:ok, r_state(receivers: recvPids)}
  end

  def handle_event(event, state = r_state(receivers: recvPids)) do
    print('~n=== ~w ===~n', [:ct_event])
    print('~tw: ~tw~n', [r_event(event, :name), r_event(event, :data)])

    :lists.foreach(
      fn recv ->
        report_event(recv, event)
      end,
      recvPids
    )

    {:ok, state}
  end

  defp report_event(
         {:master, master},
         e = r_event(name: _Name, node: _Node, data: _Data)
       ) do
    :ct_master.status(master, e)
  end

  def handle_call(_Req, state) do
    reply = :ok
    {:ok, reply, state}
  end

  def handle_info(_Info, state) do
    {:ok, state}
  end

  def terminate(_Reason, _State) do
    :ok
  end

  def code_change(_OldVsn, state, _Extra) do
    {:ok, state}
  end

  defp print(_Str, _Args) do
    :ok
  end
end
