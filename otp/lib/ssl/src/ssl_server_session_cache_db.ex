defmodule :m_ssl_server_session_cache_db do
  use Bitwise

  def init(_Options) do
    :gb_trees.empty()
  end

  def lookup(cache, key) do
    case :gb_trees.lookup(key, cache) do
      {:value, session} ->
        session

      :none ->
        :undefined
    end
  end

  def update(cache, key, session) do
    :gb_trees.insert(key, session, cache)
  end

  def delete(cache, key) do
    :gb_trees.delete(cache, key)
  end

  def size(cache) do
    :gb_trees.size(cache)
  end

  def take_oldest(cache) do
    :gb_trees.take_smallest(cache)
  end
end
