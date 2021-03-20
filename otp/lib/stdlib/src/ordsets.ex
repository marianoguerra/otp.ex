defmodule :m_ordsets do
  use Bitwise

  def new() do
    []
  end

  def is_set([e | es]) do
    is_set(es, e)
  end

  def is_set([]) do
    true
  end

  def is_set(_) do
    false
  end

  defp is_set([e2 | es], e1) when e1 < e2 do
    is_set(es, e2)
  end

  defp is_set([_ | _], _) do
    false
  end

  defp is_set([], _) do
    true
  end

  def size(s) do
    length(s)
  end

  def is_empty(s) do
    s === []
  end

  def to_list(s) do
    s
  end

  def from_list(l) do
    :lists.usort(l)
  end

  def is_element(e, [h | es]) when e > h do
    is_element(e, es)
  end

  def is_element(e, [h | _]) when e < h do
    false
  end

  def is_element(_E, [_H | _]) do
    true
  end

  def is_element(_, []) do
    false
  end

  def add_element(e, [h | es]) when e > h do
    [h | add_element(e, es)]
  end

  def add_element(e, [h | _] = set) when e < h do
    [e | set]
  end

  def add_element(_E, [_H | _] = set) do
    set
  end

  def add_element(e, []) do
    [e]
  end

  def del_element(e, [h | es]) when e > h do
    [h | del_element(e, es)]
  end

  def del_element(e, [h | _] = set) when e < h do
    set
  end

  def del_element(_E, [_H | es]) do
    es
  end

  def del_element(_, []) do
    []
  end

  def union([e1 | es1], [e2 | _] = set2) when e1 < e2 do
    [e1 | union(es1, set2)]
  end

  def union([e1 | _] = set1, [e2 | es2]) when e1 > e2 do
    [e2 | union(es2, set1)]
  end

  def union([e1 | es1], [_E2 | es2]) do
    [e1 | union(es1, es2)]
  end

  def union([], es2) do
    es2
  end

  def union(es1, []) do
    es1
  end

  def union(ordsetList) do
    :lists.umerge(ordsetList)
  end

  def intersection([e1 | es1], [e2 | _] = set2) when e1 < e2 do
    intersection(es1, set2)
  end

  def intersection([e1 | _] = set1, [e2 | es2]) when e1 > e2 do
    intersection(es2, set1)
  end

  def intersection([e1 | es1], [_E2 | es2]) do
    [e1 | intersection(es1, es2)]
  end

  def intersection([], _) do
    []
  end

  def intersection(_, []) do
    []
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

  def is_disjoint([e1 | es1], [e2 | _] = set2) when e1 < e2 do
    is_disjoint(es1, set2)
  end

  def is_disjoint([e1 | _] = set1, [e2 | es2]) when e1 > e2 do
    is_disjoint(es2, set1)
  end

  def is_disjoint([_E1 | _Es1], [_E2 | _Es2]) do
    false
  end

  def is_disjoint([], _) do
    true
  end

  def is_disjoint(_, []) do
    true
  end

  def subtract([e1 | es1], [e2 | _] = set2) when e1 < e2 do
    [e1 | subtract(es1, set2)]
  end

  def subtract([e1 | _] = set1, [e2 | es2]) when e1 > e2 do
    subtract(set1, es2)
  end

  def subtract([_E1 | es1], [_E2 | es2]) do
    subtract(es1, es2)
  end

  def subtract([], _) do
    []
  end

  def subtract(es1, []) do
    es1
  end

  def is_subset([e1 | _], [e2 | _]) when e1 < e2 do
    false
  end

  def is_subset([e1 | _] = set1, [e2 | es2]) when e1 > e2 do
    is_subset(set1, es2)
  end

  def is_subset([_E1 | es1], [_E2 | es2]) do
    is_subset(es1, es2)
  end

  def is_subset([], _) do
    true
  end

  def is_subset(_, []) do
    false
  end

  def fold(f, acc, set) do
    :lists.foldl(f, acc, set)
  end

  def filter(f, set) do
    :lists.filter(f, set)
  end
end
