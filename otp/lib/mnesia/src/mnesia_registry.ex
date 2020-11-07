defmodule :m_mnesia_registry do
  use Bitwise
  import Kernel, except: [max: 2]
  require Record
  Record.defrecord(:r_state, :state, table: :undefined, ops: [], link_to: :undefined)

  Record.defrecord(:r_registry_entry, :registry_entry,
    key: :undefined,
    key_size: :undefined,
    val_type: :undefined,
    val_size: :undefined,
    val: :undefined
  )

  Record.defrecord(:r_size, :size, pid: self(), n_values: 0, largest_key: 0, largest_val: 0)

  defp start(type, tab, linkTo) do
    starter = self()
    args = [type, starter, linkTo, tab]
    pid = spawn_link(:mnesia_registry, :init, args)

    receive do
      {:ok, res} ->
        res

      {:EXIT, ^pid, reason} when linkTo == starter ->
        exit(reason)
    end
  end

  def start_dump(tab, linkTo) do
    start(:dump, tab, linkTo)
  end

  def start_restore(tab, linkTo) do
    start(:restore, tab, linkTo)
  end

  def create_table(tab) do
    storage = :mnesia.table_info(:schema, :storage_type)
    create_table(tab, [{storage, [node()]}])
  end

  def create_table(tab, tabDef) do
    attrs = Keyword.keys(r_registry_entry(r_registry_entry()))

    case :mnesia.create_table(
           tab,
           [{:attributes, attrs} | tabDef]
         ) do
      {:atomic, :ok} ->
        :ok

      {:aborted, {:already_exists, ^tab}} ->
        :ok

      {:aborted, reason} ->
        exit(reason)
    end
  end

  def init(type, starter, linkTo, tab) do
    cond do
      linkTo != starter ->
        :erlang.link(linkTo)
        :erlang.unlink(starter)

      true ->
        :ignore
    end

    case type do
      :dump ->
        send(starter, {:ok, self()})
        dump_loop(r_state(table: tab, link_to: linkTo))

      :restore ->
        restore_table(tab, starter, linkTo)
    end
  end

  defp dump_loop(s) do
    tab = r_state(s, :table)
    ops = r_state(s, :ops)

    receive do
      {:write, key, keySize, valType, valSize, val} ->
        rE =
          r_registry_entry(
            key: key,
            key_size: keySize,
            val_type: valType,
            val_size: valSize,
            val: val
          )

        dump_loop(r_state(s, ops: [{:write, rE} | ops]))

      {:delete, key} ->
        dump_loop(r_state(s, ops: [{:delete, key} | ops]))

      {:commit, replyTo} ->
        create_table(tab)
        recName = :mnesia.table_info(tab, :record_name)

        case :mnesia.transaction(
               &handle_ops/3,
               [tab, recName, ops]
             ) do
          {:atomic, :ok} ->
            send(replyTo, {:ok, self()})
            stop(r_state(s, :link_to))

          {:aborted, reason} ->
            exit({:aborted, reason})
        end

      :abort ->
        stop(r_state(s, :link_to))

      badMsg ->
        exit({:bad_message, badMsg})
    end
  end

  defp stop(linkTo) do
    :erlang.unlink(linkTo)
    exit(:normal)
  end

  defp handle_ops(tab, recName, ops) do
    :mnesia.write_lock_table(tab)
    do_handle_ops(tab, recName, ops)
  end

  defp do_handle_ops(tab, recName, [{:write, regEntry} | ops]) do
    record = :erlang.setelement(1, regEntry, recName)
    :mnesia.write(tab, record, :write)
    do_handle_ops(tab, recName, ops)
  end

  defp do_handle_ops(tab, recName, [{:delete, key} | ops]) do
    :mnesia.delete(tab, key, :write)
    do_handle_ops(tab, recName, ops)
  end

  defp do_handle_ops(_Tab, _RecName, []) do
    :ok
  end

  defp restore_table(tab, starter, linkTo) do
    pat = :mnesia.table_info(tab, :wild_pattern)

    fun = fn ->
      :mnesia.match_object(tab, pat, :read)
    end

    case :mnesia.transaction(fun) do
      {:atomic, allRecords} ->
        size = calc_size(allRecords, r_size())
        send(starter, {:ok, size})

        receive do
          {:send_records, replyTo} ->
            send_records(allRecords, replyTo)
            :erlang.unlink(linkTo)
            exit(:normal)

          badMsg ->
            exit({:bad_message, badMsg})
        end

      {:aborted, reason} ->
        exit(reason)
    end
  end

  defp calc_size([h | t], s) do
    keySize =
      max(
        :erlang.element(r_registry_entry(:key_size), h),
        r_size(s, :largest_key)
      )

    valSize =
      max(
        :erlang.element(r_registry_entry(:val_size), h),
        r_size(s, :largest_val)
      )

    n = r_size(s, :n_values) + 1

    calc_size(
      t,
      r_size(s, n_values: n, largest_key: keySize, largest_val: valSize)
    )
  end

  defp calc_size([], size) do
    size
  end

  defp max(new, old) when new > old do
    new
  end

  defp max(_New, old) do
    old
  end

  defp send_records([h | t], replyTo) do
    keySize = :erlang.element(r_registry_entry(:key_size), h)
    valSize = :erlang.element(r_registry_entry(:val_size), h)
    valType = :erlang.element(r_registry_entry(:val_type), h)
    key = :erlang.element(r_registry_entry(:key), h)
    val = :erlang.element(r_registry_entry(:val), h)
    send(replyTo, {:restore, keySize, valSize, valType, key, val})
    send_records(t, replyTo)
  end

  defp send_records([], _ReplyTo) do
    :ok
  end
end
