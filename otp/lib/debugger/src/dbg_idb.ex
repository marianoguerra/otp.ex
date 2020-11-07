defmodule :m_dbg_idb do
  use Bitwise

  def insert(dbRef, key, value) do
    case dbRef do
      {node, modDb} ->
        :rpc.block_call(node, :ets, :insert, [modDb, {key, value}])

      modDb ->
        :ets.insert(modDb, {key, value})
    end
  end

  def lookup(dbRef, key) do
    res =
      case dbRef do
        {node, modDb} ->
          :rpc.block_call(node, :ets, :lookup, [modDb, key])

        modDb ->
          :ets.lookup(modDb, key)
      end

    case res do
      [{^key, value}] ->
        {:ok, value}

      _ ->
        :not_found
    end
  end

  def match_object(dbRef, key) do
    case dbRef do
      {node, modDb} ->
        :rpc.block_call(node, :ets, :match_object, [modDb, key])

      modDb ->
        :ets.match_object(modDb, key)
    end
  end
end
