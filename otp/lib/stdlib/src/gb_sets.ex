defmodule :m_gb_sets do
  use Bitwise

  def empty() do
    {0, nil}
  end

  def new() do
    empty()
  end

  def is_empty({0, nil}) do
    true
  end

  def is_empty(_) do
    false
  end

  def size({size, _}) do
    size
  end

  def singleton(key) do
    {1, {key, nil, nil}}
  end

  def is_element(key, s) do
    is_member(key, s)
  end

  def is_member(key, {_, t}) do
    is_member_1(key, t)
  end

  defp is_member_1(key, {key1, smaller, _}) when key < key1 do
    is_member_1(key, smaller)
  end

  defp is_member_1(key, {key1, _, bigger}) when key > key1 do
    is_member_1(key, bigger)
  end

  defp is_member_1(_, {_, _, _}) do
    true
  end

  defp is_member_1(_, nil) do
    false
  end

  def insert(key, {s, t}) do
    s1 = s + 1
    {s1, insert_1(key, t, s1 * s1)}
  end

  defp insert_1(key, {key1, smaller, bigger}, s)
       when key < key1 do
    case insert_1(key, smaller, s >>> 1) do
      {t1, h1, s1} when is_integer(h1) ->
        t = {key1, t1, bigger}
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
        {key1, t1, bigger}
    end
  end

  defp insert_1(key, {key1, smaller, bigger}, s)
       when key > key1 do
    case insert_1(key, bigger, s >>> 1) do
      {t1, h1, s1} when is_integer(h1) ->
        t = {key1, smaller, t1}
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
        {key1, smaller, t1}
    end
  end

  defp insert_1(key, nil, 0) do
    {{key, nil, nil}, 1, 1}
  end

  defp insert_1(key, nil, _) do
    {key, nil, nil}
  end

  defp insert_1(key, _, _) do
    :erlang.error({:key_exists, key})
  end

  defp count({_, nil, nil}) do
    {1, 1}
  end

  defp count({_, sm, bi}) do
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
    {t, _} = balance_list_1(l, s)
    t
  end

  defp balance_list_1(l, s) when s > 1 do
    sm = s - 1
    s2 = div(sm, 2)
    s1 = sm - s2
    {t1, [k | l1]} = balance_list_1(l, s1)
    {t2, l2} = balance_list_1(l1, s2)
    t = {k, t1, t2}
    {t, l2}
  end

  defp balance_list_1([key | l], 1) do
    {{key, nil, nil}, l}
  end

  defp balance_list_1(l, 0) do
    {nil, l}
  end

  def add_element(x, s) do
    add(x, s)
  end

  def add(x, s) do
    case is_member(x, s) do
      true ->
        s

      false ->
        insert(x, s)
    end
  end

  def from_list(l) do
    from_ordset(:ordsets.from_list(l))
  end

  def from_ordset(l) do
    s = length(l)
    {s, balance_list(l, s)}
  end

  def del_element(key, s) do
    delete_any(key, s)
  end

  def delete_any(key, s) do
    case is_member(key, s) do
      true ->
        delete(key, s)

      false ->
        s
    end
  end

  def delete(key, {s, t}) do
    {s - 1, delete_1(key, t)}
  end

  defp delete_1(key, {key1, smaller, larger}) when key < key1 do
    smaller1 = delete_1(key, smaller)
    {key1, smaller1, larger}
  end

  defp delete_1(key, {key1, smaller, bigger}) when key > key1 do
    bigger1 = delete_1(key, bigger)
    {key1, smaller, bigger1}
  end

  defp delete_1(_, {_, smaller, larger}) do
    merge(smaller, larger)
  end

  defp merge(smaller, nil) do
    smaller
  end

  defp merge(nil, larger) do
    larger
  end

  defp merge(smaller, larger) do
    {key, larger1} = take_smallest1(larger)
    {key, smaller, larger1}
  end

  def take_smallest({s, t}) do
    {key, larger} = take_smallest1(t)
    {key, {s - 1, larger}}
  end

  defp take_smallest1({key, nil, larger}) do
    {key, larger}
  end

  defp take_smallest1({key, smaller, larger}) do
    {key1, smaller1} = take_smallest1(smaller)
    {key1, {key, smaller1, larger}}
  end

  def smallest({_, t}) do
    smallest_1(t)
  end

  defp smallest_1({key, nil, _Larger}) do
    key
  end

  defp smallest_1({_Key, smaller, _Larger}) do
    smallest_1(smaller)
  end

  def take_largest({s, t}) do
    {key, smaller} = take_largest1(t)
    {key, {s - 1, smaller}}
  end

  defp take_largest1({key, smaller, nil}) do
    {key, smaller}
  end

  defp take_largest1({key, smaller, larger}) do
    {key1, larger1} = take_largest1(larger)
    {key1, {key, smaller, larger1}}
  end

  def largest({_, t}) do
    largest_1(t)
  end

  defp largest_1({key, _Smaller, nil}) do
    key
  end

  defp largest_1({_Key, _Smaller, larger}) do
    largest_1(larger)
  end

  def to_list({_, t}) do
    to_list(t, [])
  end

  defp to_list_1(t) do
    to_list(t, [])
  end

  defp to_list({key, small, big}, l) do
    to_list(small, [key | to_list(big, l)])
  end

  defp to_list(nil, l) do
    l
  end

  def iterator({_, t}) do
    iterator(t, [])
  end

  defp iterator({_, nil, _} = t, as) do
    [t | as]
  end

  defp iterator({_, l, _} = t, as) do
    iterator(l, [t | as])
  end

  defp iterator(nil, as) do
    as
  end

  def iterator_from(s, {_, t}) do
    iterator_from(s, t, [])
  end

  defp iterator_from(s, {k, _, t}, as) when k < s do
    iterator_from(s, t, as)
  end

  defp iterator_from(_, {_, nil, _} = t, as) do
    [t | as]
  end

  defp iterator_from(s, {_, l, _} = t, as) do
    iterator_from(s, l, [t | as])
  end

  defp iterator_from(_, nil, as) do
    as
  end

  def next([{x, _, t} | as]) do
    {x, iterator(t, as)}
  end

  def next([]) do
    :none
  end

  def union({n1, t1}, {n2, t2}) when n2 < n1 do
    union(to_list_1(t2), n2, t1, n1)
  end

  def union({n1, t1}, {n2, t2}) do
    union(to_list_1(t1), n1, t2, n2)
  end

  defp union(l, n1, t2, n2) when n2 < 10 do
    union_2(l, to_list_1(t2), n1 + n2)
  end

  defp union(l, n1, t2, n2) do
    x = n1 * round(1.46 * :math.log(n2))

    cond do
      n2 < x ->
        union_2(l, to_list_1(t2), n1 + n2)

      true ->
        union_1(l, mk_set(n2, t2))
    end
  end

  defp mk_set(n, t) do
    {n, t}
  end

  defp union_1([x | xs], s) do
    union_1(xs, add(x, s))
  end

  defp union_1([], s) do
    s
  end

  defp union_2(xs, ys, s) do
    union_2(xs, ys, [], s)
  end

  defp union_2([x | xs1], [y | _] = ys, as, s) when x < y do
    union_2(xs1, ys, [x | as], s)
  end

  defp union_2([x | _] = xs, [y | ys1], as, s) when x > y do
    union_2(ys1, xs, [y | as], s)
  end

  defp union_2([x | xs1], [_ | ys1], as, s) do
    union_2(xs1, ys1, [x | as], s - 1)
  end

  defp union_2([], ys, as, s) do
    {s, balance_revlist(push(ys, as), s)}
  end

  defp union_2(xs, [], as, s) do
    {s, balance_revlist(push(xs, as), s)}
  end

  defp push([x | xs], as) do
    push(xs, [x | as])
  end

  defp push([], as) do
    as
  end

  defp balance_revlist(l, s) do
    {t, _} = balance_revlist_1(l, s)
    t
  end

  defp balance_revlist_1(l, s) when s > 1 do
    sm = s - 1
    s2 = div(sm, 2)
    s1 = sm - s2
    {t2, [k | l1]} = balance_revlist_1(l, s1)
    {t1, l2} = balance_revlist_1(l1, s2)
    t = {k, t1, t2}
    {t, l2}
  end

  defp balance_revlist_1([key | l], 1) do
    {{key, nil, nil}, l}
  end

  defp balance_revlist_1(l, 0) do
    {nil, l}
  end

  def union([s | ss]) do
    union_list(s, ss)
  end

  def union([]) do
    empty()
  end

  defp union_list(s, [s1 | ss]) do
    union_list(union(s, s1), ss)
  end

  defp union_list(s, []) do
    s
  end

  def intersection({n1, t1}, {n2, t2}) when n2 < n1 do
    intersection(to_list_1(t2), n2, t1, n1)
  end

  def intersection({n1, t1}, {n2, t2}) do
    intersection(to_list_1(t1), n1, t2, n2)
  end

  defp intersection(l, _N1, t2, n2) when n2 < 10 do
    intersection_2(l, to_list_1(t2))
  end

  defp intersection(l, n1, t2, n2) do
    x = n1 * round(1.46 * :math.log(n2))

    cond do
      n2 < x ->
        intersection_2(l, to_list_1(t2))

      true ->
        intersection_1(l, t2)
    end
  end

  defp intersection_1(xs, t) do
    intersection_1(xs, t, [], 0)
  end

  defp intersection_1([x | xs], t, as, n) do
    case is_member_1(x, t) do
      true ->
        intersection_1(xs, t, [x | as], n + 1)

      false ->
        intersection_1(xs, t, as, n)
    end
  end

  defp intersection_1([], _, as, n) do
    {n, balance_revlist(as, n)}
  end

  defp intersection_2(xs, ys) do
    intersection_2(xs, ys, [], 0)
  end

  defp intersection_2([x | xs1], [y | _] = ys, as, s) when x < y do
    intersection_2(xs1, ys, as, s)
  end

  defp intersection_2([x | _] = xs, [y | ys1], as, s) when x > y do
    intersection_2(ys1, xs, as, s)
  end

  defp intersection_2([x | xs1], [_ | ys1], as, s) do
    intersection_2(xs1, ys1, [x | as], s + 1)
  end

  defp intersection_2([], _, as, s) do
    {s, balance_revlist(as, s)}
  end

  defp intersection_2(_, [], as, s) do
    {s, balance_revlist(as, s)}
  end

  def intersection([s | ss]) do
    intersection_list(s, ss)
  end

  defp intersection_list(s, [s1 | ss]) do
    intersection_list(intersection(s, s1), ss)
  end

  defp intersection_list(s, []) do
    s
  end

  def is_disjoint({n1, t1}, {n2, t2}) when n1 < n2 do
    is_disjoint_1(t1, t2)
  end

  def is_disjoint({_, t1}, {_, t2}) do
    is_disjoint_1(t2, t1)
  end

  defp is_disjoint_1(
         {k1, smaller1, bigger},
         {k2, smaller2, _} = tree
       )
       when k1 < k2 do
    not is_member_1(k1, smaller2) and
      is_disjoint_1(
        smaller1,
        smaller2
      ) and
      is_disjoint_1(
        bigger,
        tree
      )
  end

  defp is_disjoint_1({k1, smaller, bigger1}, {k2, _, bigger2} = tree)
       when k1 > k2 do
    not is_member_1(k1, bigger2) and
      is_disjoint_1(
        bigger1,
        bigger2
      ) and
      is_disjoint_1(
        smaller,
        tree
      )
  end

  defp is_disjoint_1({_K1, _, _}, {_K2, _, _}) do
    false
  end

  defp is_disjoint_1(nil, _) do
    true
  end

  defp is_disjoint_1(_, nil) do
    true
  end

  def subtract(s1, s2) do
    difference(s1, s2)
  end

  def difference({n1, t1}, {n2, t2}) do
    difference(to_list_1(t1), n1, t2, n2)
  end

  defp difference(l, n1, t2, n2) when n2 < 10 do
    difference_2(l, to_list_1(t2), n1)
  end

  defp difference(l, n1, t2, n2) do
    x = n1 * round(1.46 * :math.log(n2))

    cond do
      n2 < x ->
        difference_2(l, to_list_1(t2), n1)

      true ->
        difference_1(l, t2)
    end
  end

  defp difference_1(xs, t) do
    difference_1(xs, t, [], 0)
  end

  defp difference_1([x | xs], t, as, n) do
    case is_member_1(x, t) do
      true ->
        difference_1(xs, t, as, n)

      false ->
        difference_1(xs, t, [x | as], n + 1)
    end
  end

  defp difference_1([], _, as, n) do
    {n, balance_revlist(as, n)}
  end

  defp difference_2(xs, ys, s) do
    difference_2(xs, ys, [], s)
  end

  defp difference_2([x | xs1], [y | _] = ys, as, s) when x < y do
    difference_2(xs1, ys, [x | as], s)
  end

  defp difference_2([x | _] = xs, [y | ys1], as, s) when x > y do
    difference_2(xs, ys1, as, s)
  end

  defp difference_2([_X | xs1], [_Y | ys1], as, s) do
    difference_2(xs1, ys1, as, s - 1)
  end

  defp difference_2([], _Ys, as, s) do
    {s, balance_revlist(as, s)}
  end

  defp difference_2(xs, [], as, s) do
    {s, balance_revlist(push(xs, as), s)}
  end

  def is_subset({n1, t1}, {n2, t2}) do
    is_subset(to_list_1(t1), n1, t2, n2)
  end

  defp is_subset(l, _N1, t2, n2) when n2 < 10 do
    is_subset_2(l, to_list_1(t2))
  end

  defp is_subset(l, n1, t2, n2) do
    x = n1 * round(1.46 * :math.log(n2))

    cond do
      n2 < x ->
        is_subset_2(l, to_list_1(t2))

      true ->
        is_subset_1(l, t2)
    end
  end

  defp is_subset_1([x | xs], t) do
    case is_member_1(x, t) do
      true ->
        is_subset_1(xs, t)

      false ->
        false
    end
  end

  defp is_subset_1([], _) do
    true
  end

  defp is_subset_2([x | _], [y | _]) when x < y do
    false
  end

  defp is_subset_2([x | _] = xs, [y | ys1]) when x > y do
    is_subset_2(xs, ys1)
  end

  defp is_subset_2([_ | xs1], [_ | ys1]) do
    is_subset_2(xs1, ys1)
  end

  defp is_subset_2([], _) do
    true
  end

  defp is_subset_2(_, []) do
    false
  end

  def is_set({0, nil}) do
    true
  end

  def is_set({n, {_, _, _}})
      when is_integer(n) and
             n >= 0 do
    true
  end

  def is_set(_) do
    false
  end

  def filter(f, s) do
    from_ordset(
      for x <- to_list(s), f.(x) do
        x
      end
    )
  end

  def fold(f, a, {_, t}) when is_function(f, 2) do
    fold_1(f, a, t)
  end

  defp fold_1(f, acc0, {key, small, big}) do
    acc1 = fold_1(f, acc0, small)
    acc = f.(key, acc1)
    fold_1(f, acc, big)
  end

  defp fold_1(_, acc, _) do
    acc
  end
end
