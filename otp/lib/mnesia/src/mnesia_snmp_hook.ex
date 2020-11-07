defmodule :m_mnesia_snmp_hook do
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

  def check_ustruct([]) do
    true
  end

  def check_ustruct([{:key, types}]) do
    is_snmp_type(to_list(types))
  end

  def check_ustruct(_) do
    false
  end

  defp to_list(tuple) when is_tuple(tuple) do
    :erlang.tuple_to_list(tuple)
  end

  defp to_list(x) do
    [x]
  end

  defp is_snmp_type([:integer | t]) do
    is_snmp_type(t)
  end

  defp is_snmp_type([:string | t]) do
    is_snmp_type(t)
  end

  defp is_snmp_type([:fix_string | t]) do
    is_snmp_type(t)
  end

  defp is_snmp_type([]) do
    true
  end

  defp is_snmp_type(_) do
    false
  end

  def create_table([], mnesiaTab, _Storage) do
    :mnesia.abort({:badarg, mnesiaTab, {:snmp, :empty_snmpstruct}})
  end

  def create_table([{:key, us}], mnesiaTab, storage) do
    tree = b_new(mnesiaTab, us)
    :mnesia_lib.db_fixtable(storage, mnesiaTab, true)
    first = :mnesia_lib.db_first(storage, mnesiaTab)
    build_table(first, mnesiaTab, tree, us, storage)
    :mnesia_lib.db_fixtable(storage, mnesiaTab, false)
    tree
  end

  defp build_table(mnesiaKey, mnesiaTab, tree, us, storage)
       when mnesiaKey != :"$end_of_table" do
    snmpKey = key_to_oid_i(mnesiaKey, us)
    b_insert(tree, snmpKey, mnesiaKey)
    next = :mnesia_lib.db_next_key(storage, mnesiaTab, mnesiaKey)
    build_table(next, mnesiaTab, tree, us, storage)
  end

  defp build_table(:"$end_of_table", _MnesiaTab, _Tree, _Us, _Storage) do
    :ok
  end

  def delete_table(_MnesiaTab, tree) do
    b_delete_tree(tree)
    :ok
  end

  def update({:clear_table, mnesiaTab}) do
    tree = :mnesia_lib.val({mnesiaTab, {:index, :snmp}})
    b_clear(tree)
    :ok
  end

  def update({op, mnesiaTab, mnesiaKey, snmpKey}) do
    tree = :mnesia_lib.val({mnesiaTab, {:index, :snmp}})
    update(op, tree, mnesiaKey, snmpKey)
  end

  defp update(op, tree, mnesiaKey, snmpKey) do
    case op do
      :write ->
        b_insert(tree, snmpKey, mnesiaKey)

      :update_counter ->
        :ignore

      :delete ->
        b_delete(tree, snmpKey)

      :delete_object ->
        b_delete(tree, snmpKey)
    end

    :ok
  end

  def key_to_oid(tab, key) do
    types = :mnesia_lib.val({tab, :snmp})
    key_to_oid(tab, key, types)
  end

  def key_to_oid(tab, key, [{:key, types}]) do
    try do
      key_to_oid_i(key, types)
    catch
      _, _ ->
        :mnesia.abort({:bad_snmp_key, {tab, key}, types})
    end
  end

  def key_to_oid_i(key, :integer) when is_integer(key) do
    [key]
  end

  def key_to_oid_i(key, :fix_string) when is_list(key) do
    key
  end

  def key_to_oid_i(key, :string) when is_list(key) do
    [length(key) | key]
  end

  def key_to_oid_i(key, types) do
    keys_to_oid(:erlang.size(key), key, [], types)
  end

  defp keys_to_oid(0, _Key, oid, _Types) do
    oid
  end

  defp keys_to_oid(n, key, oid, types) do
    oid2 =
      :lists.append(
        key_to_oid_i(
          :erlang.element(
            n,
            key
          ),
          :erlang.element(n, types)
        ),
        oid
      )

    keys_to_oid(n - 1, key, oid2, types)
  end

  def oid_to_key(oid, tab) do
    [{:key, types}] = :mnesia_lib.val({tab, :snmp})
    oid_to_key_1(types, oid)
  end

  def oid_to_key_1(:integer, [key]) do
    key
  end

  def oid_to_key_1(:fix_string, key) do
    key
  end

  def oid_to_key_1(:string, [_ | key]) do
    key
  end

  def oid_to_key_1(tuple, oid) do
    try do
      list = oid_to_key_2(1, :erlang.size(tuple), tuple, oid)
      :erlang.list_to_tuple(list)
    catch
      _, _ ->
        :unknown
    end
  end

  defp oid_to_key_2(n, sz, tuple, oid0) when n <= sz do
    case :erlang.element(n, tuple) do
      :integer ->
        [key | oid] = oid0
        [key | oid_to_key_2(n + 1, sz, tuple, oid)]

      :fix_string when n === sz ->
        [oid0]

      :fix_string ->
        throw(:fix_string)

      :string ->
        [len | oid1] = oid0
        {str, oid} = :lists.split(len, oid1)
        [str | oid_to_key_2(n + 1, sz, tuple, oid)]
    end
  end

  defp oid_to_key_2(n, sz, _, []) when n === sz + 1 do
    []
  end

  def get_row(name, rowIndex) do
    tree = :mnesia_lib.val({name, {:index, :snmp}})

    case b_lookup(tree, rowIndex) do
      {:ok, {_RowIndex, key}} ->
        [row] = :mnesia.dirty_read({name, key})
        {:ok, row}

      _ ->
        :undefined
    end
  end

  def get_next_index(name, rowIndex) do
    tree = :mnesia_lib.val({name, {:index, :snmp}})

    case b_lookup_next(tree, rowIndex) do
      {:ok, r} ->
        r

      _ ->
        {:endOfTable, :undefined}
    end
  end

  def get_mnesia_key(name, rowIndex) do
    tree = :mnesia_lib.val({name, {:index, :snmp}})

    case b_lookup(tree, rowIndex) do
      {:ok, {_RowIndex, key}} ->
        {:ok, key}

      _ ->
        :undefined
    end
  end

  defp b_new(_Tab, _Us) do
    :mnesia_monitor.unsafe_mktab(
      :mnesia_snmp_hook,
      [:public, :ordered_set]
    )
  end

  defp b_delete_tree(tree) do
    :ets.delete(tree)
  end

  defp b_clear(tree) do
    :ets.delete_all_objects(tree)
  end

  defp b_insert(tree, snmpKey, mnesiaKey) do
    :ets.insert(tree, {snmpKey, mnesiaKey})
  end

  defp b_delete(tree, snmpKey) do
    :ets.delete(tree, snmpKey)
  end

  defp b_lookup(tree, rowIndex) do
    case :ets.lookup(tree, rowIndex) do
      [x] ->
        {:ok, x}

      _ ->
        :undefined
    end
  end

  defp b_lookup_next(tree, rowIndex) do
    case :ets.next(tree, rowIndex) do
      :"$end_of_table" ->
        :undefined

      key ->
        b_lookup(tree, key)
    end
  end
end
