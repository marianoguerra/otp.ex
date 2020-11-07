defmodule :m_counters do
  use Bitwise

  def new(size, [:atomics]) do
    {:atomics, :atomics.new(size, [{:signed, true}])}
  end

  def new(size, [:write_concurrency]) do
    {:write_concurrency, :erts_internal.counters_new(size)}
  end

  def new(size, []) do
    new(size, [:atomics])
  end

  def new(_, _) do
    :erlang.error(:badarg)
  end

  def get({:atomics, ref}, ix) do
    :atomics.get(ref, ix)
  end

  def get({:write_concurrency, ref}, ix) do
    :erts_internal.counters_get(ref, ix)
  end

  def get(_, _) do
    :erlang.error(:badarg)
  end

  def add({:atomics, ref}, ix, incr) do
    :atomics.add(ref, ix, incr)
  end

  def add({:write_concurrency, ref}, ix, incr) do
    :erts_internal.counters_add(ref, ix, incr)
  end

  def add(_, _, _) do
    :erlang.error(:badarg)
  end

  def sub(ref, ix, decr) do
    add(ref, ix, -decr)
  end

  def put({:atomics, ref}, ix, value) do
    :atomics.put(ref, ix, value)
  end

  def put({:write_concurrency, ref}, ix, value) do
    :erts_internal.counters_put(ref, ix, value)
  end

  def put(_, _, _) do
    :erlang.error(:badarg)
  end

  def info({:atomics, ref}) do
    :atomics.info(ref)
  end

  def info({:write_concurrency, ref}) do
    :erts_internal.counters_info(ref)
  end

  def info(_) do
    :erlang.error(:badarg)
  end
end
