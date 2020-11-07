defmodule :m_asn1ct_table do
  use Bitwise

  def new(table) do
    :undefined = :erlang.get(table)
    tableId = :ets.new(table, [])
    :erlang.put(table, tableId)
  end

  def new_reuse(table) do
    not exists(table) and new(table)
  end

  def exists(table) do
    :erlang.get(table) !== :undefined
  end

  def size(table) do
    :ets.info(:erlang.get(table), :size)
  end

  def insert(table, tuple) do
    :ets.insert(:erlang.get(table), tuple)
  end

  def lookup(table, key) do
    :ets.lookup(:erlang.get(table), key)
  end

  def match(table, matchSpec) do
    :ets.match(:erlang.get(table), matchSpec)
  end

  def to_list(table) do
    :ets.tab2list(:erlang.get(table))
  end

  def delete(tables) when is_list(tables) do
    for t <- tables do
      delete(t)
    end

    true
  end

  def delete(table) when is_atom(table) do
    case :erlang.erase(table) do
      :undefined ->
        true

      tableId ->
        :ets.delete(tableId)
    end
  end
end
