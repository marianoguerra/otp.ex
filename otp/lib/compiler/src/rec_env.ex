defmodule :m_rec_env do
  use Bitwise

  def empty() do
    [{:map, %{}}]
  end

  def is_empty([{:map, map} | es]) do
    n = map_size(map)

    cond do
      n !== 0 ->
        false

      es === [] ->
        true

      true ->
        is_empty(es)
    end
  end

  def is_empty([{:rec, map, _} | es]) do
    n = map_size(map)

    cond do
      n !== 0 ->
        false

      es === [] ->
        true

      true ->
        is_empty(es)
    end
  end

  def size(env) do
    env_size(env)
  end

  defp env_size([{:map, map}]) do
    map_size(map)
  end

  defp env_size([{:map, map} | env]) do
    map_size(map) + env_size(env)
  end

  defp env_size([{:rec, map, _Map0} | env]) do
    map_size(map) + env_size(env)
  end

  def is_defined(key, [{:map, map} | env]) do
    case :maps.is_key(key, map) do
      true ->
        true

      false when env === [] ->
        false

      false ->
        is_defined(key, env)
    end
  end

  def is_defined(key, [{:rec, map, _Map0} | env]) do
    :maps.is_key(key, map) or is_defined(key, env)
  end

  def keys(env) do
    :lists.sort(keys(env, []))
  end

  defp keys([{:map, map}], s) do
    :maps.keys(map) ++ s
  end

  defp keys([{:map, map} | env], s) do
    keys(env, :maps.keys(map) ++ s)
  end

  defp keys([{:rec, map, _Map0} | env], s) do
    keys(env, :maps.keys(map) ++ s)
  end

  def to_list(env) do
    :lists.sort(to_list(env, []))
  end

  defp to_list([{:map, map}], s) do
    :maps.to_list(map) ++ s
  end

  defp to_list([{:map, map} | env], s) do
    to_list(env, :maps.to_list(map) ++ s)
  end

  defp to_list([{:rec, map, _Map0} | env], s) do
    to_list(env, :maps.to_list(map) ++ s)
  end

  def bind(key, value, [{:map, map}]) do
    [{:map, :maps.put(key, value, map)}]
  end

  def bind(key, value, [{:map, map} | env]) do
    [
      {:map, :maps.put(key, value, map)}
      | delete_any(
          key,
          env
        )
    ]
  end

  def bind(key, value, env) do
    [
      {:map, :maps.put(key, value, %{})}
      | delete_any(
          key,
          env
        )
    ]
  end

  def bind_list(ks, vs, [{:map, map}]) do
    [{:map, store_list(ks, vs, map)}]
  end

  def bind_list(ks, vs, [{:map, map} | env]) do
    [{:map, store_list(ks, vs, map)} | delete_list(ks, env)]
  end

  def bind_list(ks, vs, env) do
    [{:map, store_list(ks, vs, %{})} | delete_list(ks, env)]
  end

  defp store_list([k | ks], [v | vs], map) do
    store_list(ks, vs, :maps.put(k, v, map))
  end

  defp store_list([], _, map) do
    map
  end

  defp delete_list([k | ks], env) do
    delete_list(ks, delete_any(k, env))
  end

  defp delete_list([], env) do
    env
  end

  defp delete_any(key, env) do
    case is_defined(key, env) do
      true ->
        delete(key, env)

      false ->
        env
    end
  end

  def delete(key, [{:map, map} = e | env]) do
    case :maps.take(key, map) do
      {_, map1} ->
        [{:map, map1} | env]

      :error ->
        delete_1(key, env, e)
    end
  end

  def delete(key, [{:rec, map, map0} = e | env]) do
    case :maps.take(key, map) do
      {_, map1} when map_size(map1) === 0 ->
        env

      {_, map1} ->
        [{:rec, map1, map0} | env]

      :error ->
        [e | delete(key, env)]
    end
  end

  defp delete_1(key, [{:rec, map, map0} = e | env], e1) do
    case :maps.take(key, map) do
      {_, map1} when map_size(map1) === 0 ->
        concat(e1, env)

      {_, map1} ->
        [e1, {:rec, map1, map0} | env]

      :error ->
        [e1, e | delete(key, env)]
    end
  end

  defp concat({:map, m1}, [{:map, m2} | env]) do
    [:maps.merge(m2, m1) | env]
  end

  defp concat(e1, env) do
    [e1 | env]
  end

  def bind_recursive([], [], _, env) do
    env
  end

  def bind_recursive(ks, vs, f, env) do
    f1 = fn v ->
      fn map ->
        f.(v, [{:rec, map, map} | env])
      end
    end

    map = bind_recursive_1(ks, vs, f1, %{})
    [{:rec, map, map} | env]
  end

  defp bind_recursive_1([k | ks], [v | vs], f, map) do
    bind_recursive_1(ks, vs, f, :maps.put(k, f.(v), map))
  end

  defp bind_recursive_1([], [], _, map) do
    map
  end

  def lookup(key, [{:map, map} | env]) do
    case :maps.find(key, map) do
      {:ok, _} = value ->
        value

      :error when env === [] ->
        :error

      :error ->
        lookup(key, env)
    end
  end

  def lookup(key, [{:rec, map, map0} | env]) do
    case :maps.find(key, map) do
      {:ok, f} ->
        {:ok, f.(map0)}

      :error ->
        lookup(key, env)
    end
  end

  def get(key, env) do
    case lookup(key, env) do
      {:ok, value} ->
        value

      :error ->
        throw({:undefined, key})
    end
  end

  def new_key(env) do
    new_key(
      fn x ->
        x
      end,
      env
    )
  end

  def new_key(f, env) do
    :ok
    r = start_range(env)
    new_key(r, f, env)
  end

  defp new_key(r, f, env) do
    new_key(generate(r, r), r, 0, f, env)
  end

  defp new_key(n, r, t, f, env) when t < 2 do
    a = f.(n)

    case is_defined(a, env) do
      true ->
        new_key(generate(n, r), r, t + 1, f, env)

      false ->
        :ok
        :ok
        a
    end
  end

  defp new_key(n, r, _T, f, env) do
    :ok
    r1 = trunc(r * 10)
    new_key(generate(n, r1), r1, 0, f, env)
  end

  defp start_range(env) do
    :erlang.max(env_size(env) * 50, 1000)
  end

  defp generate(_N, range) do
    case :rand.export_seed() do
      :undefined ->
        _ = :rand.seed(:exsplus, {1, 42, 2053})
        :ok

      _ ->
        :ok
    end

    :rand.uniform(range)
  end

  def new_keys(n, env) when is_integer(n) do
    new_keys(
      n,
      fn x ->
        x
      end,
      env
    )
  end

  def new_keys(n, f, env) when is_integer(n) do
    r = start_range(env)
    new_keys(n, [], r, f, env)
  end

  defp new_keys(n, ks, r, f, env) when n > 0 do
    key = new_key(r, f, env)
    env1 = bind(key, true, env)
    new_keys(n - 1, [key | ks], r, f, env1)
  end

  defp new_keys(0, ks, _, _, _) do
    ks
  end
end
