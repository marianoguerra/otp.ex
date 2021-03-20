defmodule :m_cerl_sets do
  use Bitwise

  def new() do
    %{}
  end

  def is_set(s) when is_map(s) do
    true
  end

  def is_set(_) do
    false
  end

  def size(s) do
    :maps.size(s)
  end

  def to_list(s) do
    :maps.keys(s)
  end

  def from_list(ls) do
    :maps.from_list(
      for k <- ls do
        {k, :ok}
      end
    )
  end

  def is_element(e, s) do
    case s do
      %{^e => _} ->
        true

      _ ->
        false
    end
  end

  def add_element(e, s) do
    Map.put(s, e, :ok)
  end

  def del_element(e, s) do
    :maps.remove(e, s)
  end

  def union(s1, s2) do
    :maps.merge(s1, s2)
  end

  def union([s1, s2 | ss]) do
    union1(union(s1, s2), ss)
  end

  def union([s]) do
    s
  end

  def union([]) do
    new()
  end

  defp union1(s1, [s2 | ss]) do
    union1(union(s1, s2), ss)
  end

  defp union1(s1, []) do
    s1
  end

  def intersection(s1, s2) when map_size(s1) >= map_size(s2) do
    filter(
      fn e ->
        is_element(e, s1)
      end,
      s2
    )
  end

  def intersection(s1, s2) do
    intersection(s2, s1)
  end

  def intersection([s1, s2 | ss]) do
    intersection1(intersection(s1, s2), ss)
  end

  def intersection([s]) do
    s
  end

  defp intersection1(s1, [s2 | ss]) do
    intersection1(intersection(s1, s2), ss)
  end

  defp intersection1(s1, []) do
    s1
  end

  def is_disjoint(s1, s2) when map_size(s1) > map_size(s2) do
    is_disjoint_1(s1, :maps.iterator(s2))
  end

  def is_disjoint(s1, s2) do
    is_disjoint_1(s2, :maps.iterator(s1))
  end

  defp is_disjoint_1(set, iter) do
    case :maps.next(iter) do
      {k, _, nextIter} ->
        case set do
          %{^k => _} ->
            false

          %{} ->
            is_disjoint_1(set, nextIter)
        end

      :none ->
        true
    end
  end

  def subtract(s1, s2) do
    filter(
      fn e ->
        not is_element(e, s2)
      end,
      s1
    )
  end

  def is_subset(s1, s2) when map_size(s1) > map_size(s2) do
    false
  end

  def is_subset(s1, s2) do
    is_subset_1(s2, :maps.iterator(s1))
  end

  defp is_subset_1(set, iter) do
    case :maps.next(iter) do
      {k, _, nextIter} ->
        case set do
          %{^k => _} ->
            is_subset_1(set, nextIter)

          %{} ->
            false
        end

      :none ->
        true
    end
  end

  def fold(fun, init, set) do
    fold_1(fun, init, :maps.iterator(set))
  end

  defp fold_1(fun, acc, iter) do
    case :maps.next(iter) do
      {k, _, nextIter} ->
        fold_1(fun, fun.(k, acc), nextIter)

      :none ->
        acc
    end
  end

  def filter(fun, set) do
    :maps.from_list(filter_1(fun, :maps.iterator(set)))
  end

  defp filter_1(fun, iter) do
    case :maps.next(iter) do
      {k, _, nextIter} ->
        case fun.(k) do
          true ->
            [{k, :ok} | filter_1(fun, nextIter)]

          false ->
            filter_1(fun, nextIter)
        end

      :none ->
        []
    end
  end
end
