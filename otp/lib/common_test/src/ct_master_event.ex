defmodule :m_ct_master_event do
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

  Record.defrecord(:r_state, :state, [])

  def start_link() do
    :gen_event.start_link({:local, :ct_master_event})
  end

  def add_handler() do
    :gen_event.add_handler(:ct_master_event, :ct_master_event, [])
  end

  def add_handler(args) do
    :gen_event.add_handler(:ct_master_event, :ct_master_event, args)
  end

  def stop() do
    case flush() do
      {:error, reason} ->
        :ct_master_logs.log(
          'Error',
          'No response from CT Master Event.\nReason = ~tp\nTerminating now!\n',
          [reason]
        )

        try do
          :erlang.exit(:erlang.whereis(:ct_master_event), :kill)
        catch
          :error, e -> {:EXIT, {e, __STACKTRACE__}}
          :exit, e -> {:EXIT, e}
          e -> e
        end

      _ ->
        :gen_event.stop(:ct_master_event)
    end
  end

  defp flush() do
    try do
      :gen_event.call(:ct_master_event, :ct_master_event, :flush, 1_800_000)
    catch
      _, reason ->
        {:error, reason}
    else
      :flushing ->
        :timer.sleep(1)
        flush()

      :done ->
        :ok

      error = {:error, _} ->
        error
    end
  end

  def notify(event) do
    :gen_event.notify(:ct_master_event, event)
  end

  def sync_notify(event) do
    :gen_event.sync_notify(:ct_master_event, event)
  end

  def init(_) do
    :ct_util.mark_process()
    :ct_master_logs.log('CT Master Event Handler started', '', [])
    {:ok, r_state()}
  end

  def handle_event(
        r_event(name: :start_logging, node: node, data: runDir),
        state
      ) do
    :ct_master_logs.log('CT Master Event Handler', 'Got ~ts from ~w', [runDir, node])
    :ct_master_logs.nodedir(node, runDir)
    {:ok, state}
  end

  def handle_event(r_event(name: name, node: node, data: data), state) do
    print('~n=== ~w ===~n', [:ct_master_event])
    print('~tw on ~w: ~tp~n', [name, node, data])
    {:ok, state}
  end

  def handle_call(:flush, state) do
    case :erlang.process_info(
           self(),
           :message_queue_len
         ) do
      {:message_queue_len, 0} ->
        {:ok, :done, state}

      _ ->
        {:ok, :flushing, state}
    end
  end

  def handle_info(_Info, state) do
    {:ok, state}
  end

  def terminate(_Reason, _State) do
    :ct_master_logs.log('CT Master Event Handler stopping', '', [])
    :ok
  end

  def code_change(_OldVsn, state, _Extra) do
    {:ok, state}
  end

  defp print(_Str, _Args) do
    :ok
  end
end
