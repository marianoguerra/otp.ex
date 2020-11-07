defmodule :m_mnesia_late_loader do
  use Bitwise
  require Record

  Record.defrecord(:r_tid, :tid,
    counter: :undefined,
    pid: :undefined
  )

  Record.defrecord(:r_tidstore, :tidstore, store: :undefined, up_stores: [], level: 1)

  Record.defrecord(:r_cstruct, :cstruct,
    name: :undefined,
    type: :set,
    ram_copies: [],
    disc_copies: [],
    disc_only_copies: [],
    external_copies: [],
    load_order: 0,
    access_mode: :read_write,
    majority: false,
    index: [],
    snmp: [],
    local_content: false,
    record_name: {:bad_record_name},
    attributes: [:key, :val],
    user_properties: [],
    frag_properties: [],
    storage_properties: [],
    cookie:
      {{:erlang.monotonic_time() + :erlang.time_offset(), :erlang.unique_integer(), 1}, node()},
    version: {{2, 0}, []}
  )

  Record.defrecord(:r_log_header, :log_header,
    log_kind: :undefined,
    log_version: :undefined,
    mnesia_version: :undefined,
    node: :undefined,
    now: :undefined
  )

  Record.defrecord(:r_commit, :commit,
    node: :undefined,
    decision: :undefined,
    ram_copies: [],
    disc_copies: [],
    disc_only_copies: [],
    ext: [],
    schema_ops: []
  )

  Record.defrecord(:r_decision, :decision,
    tid: :undefined,
    outcome: :undefined,
    disc_nodes: :undefined,
    ram_nodes: :undefined
  )

  Record.defrecord(:r_cyclic, :cyclic,
    node: node(),
    oid: :undefined,
    op: :undefined,
    lock: :undefined,
    lucky: :undefined
  )

  Record.defrecord(:r_state, :state, supervisor: :undefined)

  def async_late_disc_load(_, [], _) do
    :ok
  end

  def async_late_disc_load(node, tabs, reason) do
    msg = {:async_late_disc_load, tabs, reason}

    try do
      send({:mnesia_late_loader, node}, {self(), msg})
    catch
      :error, _ ->
        :ok
    end
  end

  def maybe_async_late_disc_load(_, [], _) do
    :ok
  end

  def maybe_async_late_disc_load(node, tabs, reason) do
    msg = {:maybe_async_late_disc_load, tabs, reason}

    try do
      send({:mnesia_late_loader, node}, {self(), msg})
    catch
      :error, _ ->
        :ok
    end
  end

  def start() do
    :mnesia_monitor.start_proc(:mnesia_late_loader, :mnesia_late_loader, :init, [self()])
  end

  def init(parent) do
    :erlang.register(:mnesia_late_loader, self())
    :erlang.link(:erlang.whereis(:mnesia_controller))
    :mnesia_controller.merge_schema()
    :erlang.unlink(:erlang.whereis(:mnesia_controller))
    :mnesia_lib.set(:mnesia_status, :running)
    :proc_lib.init_ack(parent, {:ok, self()})
    loop(r_state(supervisor: parent))
  end

  defp loop(state) do
    receive do
      {_From, {:async_late_disc_load, tabs, reason}} ->
        :mnesia_controller.schedule_late_disc_load(tabs, reason)
        loop(state)

      {_From, {:maybe_async_late_disc_load, tabs, reason}} ->
        checkMaster = fn tab, good ->
          case :mnesia_recover.get_master_nodes(tab) do
            [] ->
              [tab | good]

            masters ->
              case :lists.member(node(), masters) do
                true ->
                  [tab | good]

                false ->
                  good
              end
          end
        end

        goodTabs = :lists.foldl(checkMaster, [], tabs)

        :mnesia_controller.schedule_late_disc_load(
          goodTabs,
          reason
        )

        loop(state)

      {:system, from, msg} ->
        :mnesia_lib.dbg_out('~p got {system, ~p, ~tp}~n', [:mnesia_late_loader, from, msg])
        parent = r_state(state, :supervisor)
        :sys.handle_system_msg(msg, from, parent, :mnesia_late_loader, [], state)

      msg ->
        :mnesia_lib.error('~p got unexpected message: ~tp~n', [:mnesia_late_loader, msg])
        loop(state)
    end
  end

  def system_continue(_Parent, _Debug, state) do
    loop(state)
  end

  def system_terminate(reason, _Parent, _Debug, _State) do
    exit(reason)
  end

  def system_code_change(state, _Module, _OldVsn, _Extra) do
    {:ok, state}
  end
end
