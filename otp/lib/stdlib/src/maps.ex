defmodule :m_maps do
  use Bitwise

  def get(_, _) do
    :erlang.nif_error(:undef)
  end

  def find(_, _) do
    :erlang.nif_error(:undef)
  end

  def from_list(_) do
    :erlang.nif_error(:undef)
  end

  def is_key(_, _) do
    :erlang.nif_error(:undef)
  end

  def keys(_) do
    :erlang.nif_error(:undef)
  end

  def merge(_, _) do
    :erlang.nif_error(:undef)
  end

  def put(_, _, _) do
    :erlang.nif_error(:undef)
  end

  def remove(_, _) do
    :erlang.nif_error(:undef)
  end

  def take(_, _) do
    :erlang.nif_error(:undef)
  end

  def to_list(map) when is_map(map) do
    to_list_internal(:erts_internal.map_next(0, map, []))
  end

  def to_list(map) do
    :erlang.error({:badmap, map}, [map])
  end

  defp to_list_internal([iter, map | acc]) when is_integer(iter) do
    to_list_internal(:erts_internal.map_next(iter, map, acc))
  end

  defp to_list_internal(acc) do
    acc
  end

  def update(_, _, _) do
    :erlang.nif_error(:undef)
  end

  def values(_) do
    :erlang.nif_error(:undef)
  end

  def new() do
    %{}
  end

  def update_with(key, fun, map)
      when is_function(fun, 1) and
             is_map(map) do
    case map do
      %{^key => value} ->
        %{map | key => fun.(value)}

      %{} ->
        :erlang.error({:badkey, key}, [key, fun, map])
    end
  end

  def update_with(key, fun, map) do
    :erlang.error(error_type(map), [key, fun, map])
  end

  def update_with(key, fun, init, map)
      when is_function(
             fun,
             1
           ) and
             is_map(map) do
    case map do
      %{^key => value} ->
        %{map | key => fun.(value)}

      %{} ->
        Map.put(map, key, init)
    end
  end

  def update_with(key, fun, init, map) do
    :erlang.error(error_type(map), [key, fun, init, map])
  end

  def get(key, map, default) when is_map(map) do
    case map do
      %{^key => value} ->
        value

      %{} ->
        default
    end
  end

  def get(key, map, default) do
    :erlang.error({:badmap, map}, [key, map, default])
  end

  def filter(pred, map)
      when is_function(pred, 2) and
             is_map(map) do
    :maps.from_list(filter_1(pred, iterator(map)))
  end

  def filter(pred, iterator)
      when (is_function(pred, 2) and
              is_tuple(iterator) and tuple_size(iterator) == 3) or
             iterator == :none or
             (is_integer(hd(iterator)) and is_map(tl(iterator))) do
    :maps.from_list(filter_1(pred, iterator))
  end

  def filter(pred, map) do
    :erlang.error(error_type(map), [pred, map])
  end

  defp filter_1(pred, iter) do
    case next(iter) do
      {k, v, nextIter} ->
        case pred.(k, v) do
          true ->
            [{k, v} | filter_1(pred, nextIter)]

          false ->
            filter_1(pred, nextIter)
        end

      :none ->
        []
    end
  end

  def fold(fun, init, map)
      when is_function(fun, 3) and
             is_map(map) do
    fold_1(fun, init, iterator(map))
  end

  def fold(fun, init, iterator)
      when (is_function(
              fun,
              3
            ) and
              is_tuple(iterator) and tuple_size(iterator) == 3) or
             iterator == :none or
             (is_integer(hd(iterator)) and is_map(tl(iterator))) do
    fold_1(fun, init, iterator)
  end

  def fold(fun, init, map) do
    :erlang.error(error_type_iter(map), [fun, init, map])
  end

  defp fold_1(fun, acc, iter) do
    case next(iter) do
      {k, v, nextIter} ->
        fold_1(fun, fun.(k, v, acc), nextIter)

      :none ->
        acc
    end
  end

  def map(fun, map)
      when is_function(fun, 2) and
             is_map(map) do
    :maps.from_list(map_1(fun, iterator(map)))
  end

  def map(fun, iterator)
      when (is_function(fun, 2) and
              is_tuple(iterator) and tuple_size(iterator) == 3) or
             iterator == :none or
             (is_integer(hd(iterator)) and is_map(tl(iterator))) do
    :maps.from_list(map_1(fun, iterator))
  end

  def map(fun, map) do
    :erlang.error(error_type_iter(map), [fun, map])
  end

  defp map_1(fun, iter) do
    case next(iter) do
      {k, v, nextIter} ->
        [{k, fun.(k, v)} | map_1(fun, nextIter)]

      :none ->
        []
    end
  end

  def size(map) when is_map(map) do
    :erlang.map_size(map)
  end

  def size(val) do
    :erlang.error({:badmap, val}, [val])
  end

  def iterator(m) when is_map(m) do
    [0 | m]
  end

  def iterator(m) do
    :erlang.error({:badmap, m}, [m])
  end

  def next({k, v, i}) do
    {k, v, i}
  end

  def next([path | map])
      when is_integer(path) and
             is_map(map) do
    :erts_internal.map_next(path, map, :iterator)
  end

  def next(:none) do
    :none
  end

  def next(iter) do
    :erlang.error(:badarg, [iter])
  end

  def without(ks, m) when is_list(ks) and is_map(m) do
    :lists.foldl(&:maps.remove/2, m, ks)
  end

  def without(ks, m) do
    :erlang.error(error_type(m), [ks, m])
  end

  def with(ks, map1) when is_list(ks) and is_map(map1) do
    :maps.from_list(with_1(ks, map1))
  end

  def with(ks, m) do
    :erlang.error(error_type(m), [ks, m])
  end

  defp with_1([k | ks], map) do
    case map do
      %{^k => v} ->
        [{k, v} | with_1(ks, map)]

      %{} ->
        with_1(ks, map)
    end
  end

  defp with_1([], _Map) do
    []
  end

  defp error_type(m) when is_map(m) do
    :badarg
  end

  defp error_type(v) do
    {:badmap, v}
  end

  defp error_type_iter(m)
       when is_map(m) or
              (is_tuple(m) and tuple_size(m) == 3) or m == :none or
              (is_integer(hd(m)) and is_map(tl(m))) do
    :badarg
  end

  defp error_type_iter(v) do
    {:badmap, v}
  end
end
