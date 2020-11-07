defmodule :m_gb_trees do
  use Bitwise

  def empty() do
    {0, nil}
  end

  def is_empty({0, nil}) do
    true
  end

  def is_empty(_) do
    false
  end

  def size({size, _})
      when is_integer(size) and
             size >= 0 do
    size
  end

  def lookup(key, {_, t}) do
    lookup_1(key, t)
  end

  defp lookup_1(key, {key1, _, smaller, _}) when key < key1 do
    lookup_1(key, smaller)
  end

  defp lookup_1(key, {key1, _, _, bigger}) when key > key1 do
    lookup_1(key, bigger)
  end

  defp lookup_1(_, {_, value, _, _}) do
    {:value, value}
  end

  defp lookup_1(_, nil) do
    :none
  end

  def is_defined(key, {_, t}) do
    is_defined_1(key, t)
  end

  defp is_defined_1(key, {key1, _, smaller, _}) when key < key1 do
    is_defined_1(key, smaller)
  end

  defp is_defined_1(key, {key1, _, _, bigger}) when key > key1 do
    is_defined_1(key, bigger)
  end

  defp is_defined_1(_, {_, _, _, _}) do
    true
  end

  defp is_defined_1(_, nil) do
    false
  end

  def get(key, {_, t}) do
    get_1(key, t)
  end

  defp get_1(key, {key1, _, smaller, _}) when key < key1 do
    get_1(key, smaller)
  end

  defp get_1(key, {key1, _, _, bigger}) when key > key1 do
    get_1(key, bigger)
  end

  defp get_1(_, {_, value, _, _}) do
    value
  end

  def update(key, val, {s, t}) do
    t1 = update_1(key, val, t)
    {s, t1}
  end

  defp update_1(key, value, {key1, v, smaller, bigger})
       when key < key1 do
    {key1, v, update_1(key, value, smaller), bigger}
  end

  defp update_1(key, value, {key1, v, smaller, bigger})
       when key > key1 do
    {key1, v, smaller, update_1(key, value, bigger)}
  end

  defp update_1(key, value, {_, _, smaller, bigger}) do
    {key, value, smaller, bigger}
  end

  def insert(key, val, {s, t}) when is_integer(s) do
    s1 = s + 1
    {s1, insert_1(key, val, t, s1 * s1)}
  end

  defp insert_1(key, value, {key1, v, smaller, bigger}, s)
       when key < key1 do
    case insert_1(key, value, smaller, s >>> 1) do
      {t1, h1, s1} ->
        t = {key1, v, t1, bigger}
        {h2, s2} = count(bigger)
        h = :erlang.max(h1, h2) <<< 1
        sS = s1 + s2 + 1
        p = sS * sS

        cond do
          h > p ->
            balance(t, sS)

          true ->
            {t, h, sS}
        end

      t1 ->
        {key1, v, t1, bigger}
    end
  end

  defp insert_1(key, value, {key1, v, smaller, bigger}, s)
       when key > key1 do
    case insert_1(key, value, bigger, s >>> 1) do
      {t1, h1, s1} ->
        t = {key1, v, smaller, t1}
        {h2, s2} = count(smaller)
        h = :erlang.max(h1, h2) <<< 1
        sS = s1 + s2 + 1
        p = sS * sS

        cond do
          h > p ->
            balance(t, sS)

          true ->
            {t, h, sS}
        end

      t1 ->
        {key1, v, smaller, t1}
    end
  end

  defp insert_1(key, value, nil, s) when s === 0 do
    {{key, value, nil, nil}, 1, 1}
  end

  defp insert_1(key, value, nil, _S) do
    {key, value, nil, nil}
  end

  defp insert_1(key, _, _, _) do
    :erlang.error({:key_exists, key})
  end

  def enter(key, val, t) do
    case is_defined(key, t) do
      true ->
        update(key, val, t)

      false ->
        insert(key, val, t)
    end
  end

  defp count({_, _, nil, nil}) do
    {1, 1}
  end

  defp count({_, _, sm, bi}) do
    {h1, s1} = count(sm)
    {h2, s2} = count(bi)
    {:erlang.max(h1, h2) <<< 1, s1 + s2 + 1}
  end

  defp count(nil) do
    {1, 0}
  end

  def balance({s, t}) do
    {s, balance(t, s)}
  end

  defp balance(t, s) do
    balance_list(to_list_1(t), s)
  end

  defp balance_list(l, s) do
    {t, []} = balance_list_1(l, s)
    t
  end

  defp balance_list_1(l, s) when s > 1 do
    sm = s - 1
    s2 = div(sm, 2)
    s1 = sm - s2
    {t1, [{k, v} | l1]} = balance_list_1(l, s1)
    {t2, l2} = balance_list_1(l1, s2)
    t = {k, v, t1, t2}
    {t, l2}
  end

  defp balance_list_1([{key, val} | l], 1) do
    {{key, val, nil, nil}, l}
  end

  defp balance_list_1(l, 0) do
    {nil, l}
  end

  def from_orddict(l) do
    s = length(l)
    {s, balance_list(l, s)}
  end

  def delete_any(key, t) do
    case is_defined(key, t) do
      true ->
        delete(key, t)

      false ->
        t
    end
  end

  def delete(key, {s, t}) when is_integer(s) and s >= 0 do
    {s - 1, delete_1(key, t)}
  end

  defp delete_1(key, {key1, value, smaller, larger})
       when key < key1 do
    smaller1 = delete_1(key, smaller)
    {key1, value, smaller1, larger}
  end

  defp delete_1(key, {key1, value, smaller, bigger})
       when key > key1 do
    bigger1 = delete_1(key, bigger)
    {key1, value, smaller, bigger1}
  end

  defp delete_1(_, {_, _, smaller, larger}) do
    merge(smaller, larger)
  end

  defp merge(smaller, nil) do
    smaller
  end

  defp merge(nil, larger) do
    larger
  end

  defp merge(smaller, larger) do
    {key, value, larger1} = take_smallest1(larger)
    {key, value, smaller, larger1}
  end

  def take_any(key, tree) do
    case is_defined(key, tree) do
      true ->
        take(key, tree)

      false ->
        :error
    end
  end

  def take(key, {s, t}) when is_integer(s) and s >= 0 do
    {value, res} = take_1(key, t)
    {value, {s - 1, res}}
  end

  defp take_1(key, {key1, value, smaller, larger})
       when key < key1 do
    {value2, smaller1} = take_1(key, smaller)
    {value2, {key1, value, smaller1, larger}}
  end

  defp take_1(key, {key1, value, smaller, bigger})
       when key > key1 do
    {value2, bigger1} = take_1(key, bigger)
    {value2, {key1, value, smaller, bigger1}}
  end

  defp take_1(_, {_Key, value, smaller, larger}) do
    {value, merge(smaller, larger)}
  end

  def take_smallest({size, tree})
      when is_integer(size) and
             size >= 0 do
    {key, value, larger} = take_smallest1(tree)
    {key, value, {size - 1, larger}}
  end

  defp take_smallest1({key, value, nil, larger}) do
    {key, value, larger}
  end

  defp take_smallest1({key, value, smaller, larger}) do
    {key1, value1, smaller1} = take_smallest1(smaller)
    {key1, value1, {key, value, smaller1, larger}}
  end

  def smallest({_, tree}) do
    smallest_1(tree)
  end

  defp smallest_1({key, value, nil, _Larger}) do
    {key, value}
  end

  defp smallest_1({_Key, _Value, smaller, _Larger}) do
    smallest_1(smaller)
  end

  def take_largest({size, tree})
      when is_integer(size) and
             size >= 0 do
    {key, value, smaller} = take_largest1(tree)
    {key, value, {size - 1, smaller}}
  end

  defp take_largest1({key, value, smaller, nil}) do
    {key, value, smaller}
  end

  defp take_largest1({key, value, smaller, larger}) do
    {key1, value1, larger1} = take_largest1(larger)
    {key1, value1, {key, value, smaller, larger1}}
  end

  def largest({_, tree}) do
    largest_1(tree)
  end

  defp largest_1({key, value, _Smaller, nil}) do
    {key, value}
  end

  defp largest_1({_Key, _Value, _Smaller, larger}) do
    largest_1(larger)
  end

  def to_list({_, t}) do
    to_list(t, [])
  end

  defp to_list_1(t) do
    to_list(t, [])
  end

  defp to_list({key, value, small, big}, l) do
    to_list(small, [{key, value} | to_list(big, l)])
  end

  defp to_list(nil, l) do
    l
  end

  def keys({_, t}) do
    keys(t, [])
  end

  defp keys({key, _Value, small, big}, l) do
    keys(small, [key | keys(big, l)])
  end

  defp keys(nil, l) do
    l
  end

  def values({_, t}) do
    values(t, [])
  end

  defp values({_Key, value, small, big}, l) do
    values(small, [value | values(big, l)])
  end

  defp values(nil, l) do
    l
  end

  def iterator({_, t}) do
    iterator_1(t)
  end

  defp iterator_1(t) do
    iterator(t, [])
  end

  defp iterator({_, _, nil, _} = t, as) do
    [t | as]
  end

  defp iterator({_, _, l, _} = t, as) do
    iterator(l, [t | as])
  end

  defp iterator(nil, as) do
    as
  end

  def iterator_from(s, {_, t}) do
    iterator_1_from(s, t)
  end

  defp iterator_1_from(s, t) do
    iterator_from(s, t, [])
  end

  defp iterator_from(s, {k, _, _, t}, as) when k < s do
    iterator_from(s, t, as)
  end

  defp iterator_from(_, {_, _, nil, _} = t, as) do
    [t | as]
  end

  defp iterator_from(s, {_, _, l, _} = t, as) do
    iterator_from(s, l, [t | as])
  end

  defp iterator_from(_, nil, as) do
    as
  end

  def next([{x, v, _, t} | as]) do
    {x, v, iterator(t, as)}
  end

  def next([]) do
    :none
  end

  def map(f, {size, tree}) when is_function(f, 2) do
    {size, map_1(f, tree)}
  end

  defp map_1(_, nil) do
    nil
  end

  defp map_1(f, {k, v, smaller, larger}) do
    {k, f.(k, v), map_1(f, smaller), map_1(f, larger)}
  end
end
