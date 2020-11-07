defmodule :m_mnesia_rpc do
  use Bitwise
  @behaviour :gen_server
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

  def start() do
    :gen_server.start_link({:local, :mnesia_rpc}, :mnesia_rpc, [self()], [{:timeout, :infinity}])
  end

  def call(node, m, f, args) do
    case (try do
            :ets.lookup_element(:mnesia_gvar, {:protocol, node}, 2)
          catch
            :error, _ ->
              {:EXIT, {:badarg, []}}
          end) do
      {ver, _} when ver > {8, 4} ->
        try do
          :gen_server.call({:mnesia_rpc, node}, {:apply, m, f, args}, :infinity)
        catch
          _, reason ->
            {:badrpc, {:EXIT, reason}}
        end

      _ ->
        :rpc.call(node, m, f, args)
    end
  end

  def init([_Parent]) do
    {:ok, %{}}
  end

  def handle_call({:apply, :mnesia_lib, :db_get = func, args}, from, state) do
    apply_lib(func, args, from, state)
  end

  def handle_call({:apply, :mnesia_lib, :db_last = func, args}, from, state) do
    apply_lib(func, args, from, state)
  end

  def handle_call({:apply, :mnesia_lib, :db_first = func, args}, from, state) do
    apply_lib(func, args, from, state)
  end

  def handle_call({:apply, :mnesia_lib, :db_next_key = func, args}, from, state) do
    apply_lib(func, args, from, state)
  end

  def handle_call({:apply, :mnesia_lib, :db_prev_key = func, args}, from, state) do
    apply_lib(func, args, from, state)
  end

  def handle_call({:apply, mod, func, args}, from, state) do
    fun = apply_fun(mod, func, args, from)
    _Pid = spawn_link(fun)
    {:noreply, state}
  end

  def handle_call(msg, _From, state) do
    :mnesia_lib.error('~p got unexpected call: ~tp~n', [:mnesia_rpc, msg])
    {:reply, :badop, state}
  end

  def handle_cast(msg, state) do
    :mnesia_lib.error('~p got unexpected cast: ~tp~n', [:mnesia_rpc, msg])
    {:noreply, state}
  end

  def handle_info(msg, state) do
    :mnesia_lib.error('~p got unexpected info: ~tp~n', [:mnesia_rpc, msg])
    {:noreply, state}
  end

  def code_change(_OldVsn, state, _Extra) do
    {:ok, state}
  end

  def terminate(_Reason, _State) do
    :ok
  end

  defp apply_lib(func, [tab | _] = args, from, state) do
    try do
      ram =
        try do
          :ets.lookup_element(:mnesia_gvar, {tab, :storage_type}, 2)
        catch
          :error, _ ->
            {:EXIT, {:badarg, []}}
        end

      cond do
        ram === :ram_copies or ram === :disc_copies ->
          {:reply, apply(:mnesia_lib, func, [ram | args]), state}

        true ->
          fun = apply_fun(:mnesia_lib, func, args, from)
          _Pid = spawn_link(fun)
          {:noreply, state}
      end
    catch
      res ->
        {:reply, res, state}

      _, reason ->
        {:reply, {:badrpc, {:EXIT, reason}}, state}
    end
  end

  defp apply_fun(mod, func, args, from) do
    fn ->
      result =
        try do
          apply(mod, func, args)
        catch
          res ->
            res

          _, reason ->
            {:badrpc, {:EXIT, reason}}
        end

      :gen_server.reply(from, result)
    end
  end
end
