defmodule :m_lists do
  use Bitwise
  import Kernel, except: [max: 2, min: 2]

  def keyfind(_, _, _) do
    :erlang.nif_error(:undef)
  end

  def keymember(_, _, _) do
    :erlang.nif_error(:undef)
  end

  def keysearch(_, _, _) do
    :erlang.nif_error(:undef)
  end

  def member(_, _) do
    :erlang.nif_error(:undef)
  end

  def reverse(_, _) do
    :erlang.nif_error(:undef)
  end

  def append(l1, l2) do
    l1 ++ l2
  end

  def append([e]) do
    e
  end

  def append([h | t]) do
    h ++ append(t)
  end

  def append([]) do
    []
  end

  def subtract(l1, l2) do
    l1 -- l2
  end

  def reverse([] = l) do
    l
  end

  def reverse([_] = l) do
    l
  end

  def reverse([a, b]) do
    [b, a]
  end

  def reverse([a, b | l]) do
    :lists.reverse(l, [b, a])
  end

  def nth(1, [h | _]) do
    h
  end

  def nth(n, [_ | t]) when n > 1 do
    nth(n - 1, t)
  end

  def nthtail(1, [_ | t]) do
    t
  end

  def nthtail(n, [_ | t]) when n > 1 do
    nthtail(n - 1, t)
  end

  def nthtail(0, l) when is_list(l) do
    l
  end

  def prefix([x | preTail], [x | tail]) do
    prefix(preTail, tail)
  end

  def prefix([], list) when is_list(list) do
    true
  end

  def prefix([_ | _], list) when is_list(list) do
    false
  end

  def suffix(suffix, list) do
    delta = length(list) - length(suffix)
    delta >= 0 and nthtail(delta, list) === suffix
  end

  def droplast([_T]) do
    []
  end

  def droplast([h | t]) do
    [h | droplast(t)]
  end

  def last([e | es]) do
    last(e, es)
  end

  defp last(_, [e | es]) do
    last(e, es)
  end

  defp last(e, []) do
    e
  end

  def seq(first, last)
      when is_integer(first) and
             is_integer(last) and first - 1 <= last do
    seq_loop(last - first + 1, last, [])
  end

  defp seq_loop(n, x, l) when n >= 4 do
    seq_loop(n - 4, x - 4, [x - 3, x - 2, x - 1, x | l])
  end

  defp seq_loop(n, x, l) when n >= 2 do
    seq_loop(n - 2, x - 2, [x - 1, x | l])
  end

  defp seq_loop(1, x, l) do
    [x | l]
  end

  defp seq_loop(0, _, l) do
    l
  end

  def seq(first, last, inc)
      when is_integer(first) and
             is_integer(last) and is_integer(inc) do
    cond do
      (inc > 0 and first - inc <= last) or
          (inc < 0 and first - inc >= last) ->
        n = div(last - first + inc, inc)
        seq_loop(n, inc * (n - 1) + first, inc, [])

      inc === 0 and first === last ->
        seq_loop(1, first, inc, [])
    end
  end

  defp seq_loop(n, x, d, l) when n >= 4 do
    y = x - d
    z = y - d
    w = z - d
    seq_loop(n - 4, w - d, d, [w, z, y, x | l])
  end

  defp seq_loop(n, x, d, l) when n >= 2 do
    y = x - d
    seq_loop(n - 2, y - d, d, [y, x | l])
  end

  defp seq_loop(1, x, _, l) do
    [x | l]
  end

  defp seq_loop(0, _, _, l) do
    l
  end

  def sum(l) do
    sum(l, 0)
  end

  defp sum([h | t], sum) do
    sum(t, sum + h)
  end

  defp sum([], sum) do
    sum
  end

  def duplicate(n, x) when is_integer(n) and n >= 0 do
    duplicate(n, x, [])
  end

  defp duplicate(0, _, l) do
    l
  end

  defp duplicate(n, x, l) do
    duplicate(n - 1, x, [x | l])
  end

  def min([h | t]) do
    min(t, h)
  end

  defp min([h | t], min) when h < min do
    min(t, h)
  end

  defp min([_ | t], min) do
    min(t, min)
  end

  defp min([], min) do
    min
  end

  def max([h | t]) do
    max(t, h)
  end

  defp max([h | t], max) when h > max do
    max(t, h)
  end

  defp max([_ | t], max) do
    max(t, max)
  end

  defp max([], max) do
    max
  end

  def sublist(list, 1, l)
      when is_list(list) and
             is_integer(l) and l >= 0 do
    sublist(list, l)
  end

  def sublist([], s, _L) when is_integer(s) and s >= 2 do
    []
  end

  def sublist([_H | t], s, l)
      when is_integer(s) and
             s >= 2 do
    sublist(t, s - 1, l)
  end

  def sublist(list, l)
      when is_integer(l) and
             is_list(list) do
    sublist_2(list, l)
  end

  defp sublist_2([h | t], l) when l > 0 do
    [h | sublist_2(t, l - 1)]
  end

  defp sublist_2(_, 0) do
    []
  end

  defp sublist_2(list, l) when is_list(list) and l > 0 do
    []
  end

  def delete(item, [item | rest]) do
    rest
  end

  def delete(item, [h | rest]) do
    [h | delete(item, rest)]
  end

  def delete(_, []) do
    []
  end

  def zip([x | xs], [y | ys]) do
    [{x, y} | zip(xs, ys)]
  end

  def zip([], []) do
    []
  end

  def unzip(ts) do
    unzip(ts, [], [])
  end

  defp unzip([{x, y} | ts], xs, ys) do
    unzip(ts, [x | xs], [y | ys])
  end

  defp unzip([], xs, ys) do
    {reverse(xs), reverse(ys)}
  end

  def zip3([x | xs], [y | ys], [z | zs]) do
    [{x, y, z} | zip3(xs, ys, zs)]
  end

  def zip3([], [], []) do
    []
  end

  def unzip3(ts) do
    unzip3(ts, [], [], [])
  end

  defp unzip3([{x, y, z} | ts], xs, ys, zs) do
    unzip3(ts, [x | xs], [y | ys], [z | zs])
  end

  defp unzip3([], xs, ys, zs) do
    {reverse(xs), reverse(ys), reverse(zs)}
  end

  def zipwith(f, [x | xs], [y | ys]) do
    [f.(x, y) | zipwith(f, xs, ys)]
  end

  def zipwith(f, [], []) when is_function(f, 2) do
    []
  end

  def zipwith3(f, [x | xs], [y | ys], [z | zs]) do
    [f.(x, y, z) | zipwith3(f, xs, ys, zs)]
  end

  def zipwith3(f, [], [], []) when is_function(f, 3) do
    []
  end

  def sort([x, y | l] = l0) when x <= y do
    case l do
      [] ->
        l0

      [z] when y <= z ->
        l0

      [z] when x <= z ->
        [x, z, y]

      [z] ->
        [z, x, y]

      _ when x == y ->
        sort_1(y, l, [x])

      _ ->
        split_1(x, y, l, [], [])
    end
  end

  def sort([x, y | l]) do
    case l do
      [] ->
        [y, x]

      [z] when x <= z ->
        [y, x | l]

      [z] when y <= z ->
        [y, z, x]

      [z] ->
        [z, y, x]

      _ ->
        split_2(x, y, l, [], [])
    end
  end

  def sort([_] = l) do
    l
  end

  def sort([] = l) do
    l
  end

  defp sort_1(x, [y | l], r) when x == y do
    sort_1(y, l, [x | r])
  end

  defp sort_1(x, [y | l], r) when x < y do
    split_1(x, y, l, r, [])
  end

  defp sort_1(x, [y | l], r) do
    split_2(x, y, l, r, [])
  end

  defp sort_1(x, [], r) do
    :lists.reverse(r, [x])
  end

  def merge(l) do
    mergel(l, [])
  end

  def merge3(l1, [], l3) do
    merge(l1, l3)
  end

  def merge3(l1, l2, []) do
    merge(l1, l2)
  end

  def merge3(l1, [h2 | t2], [h3 | t3]) do
    :lists.reverse(merge3_1(l1, [], h2, t2, h3, t3), [])
  end

  def rmerge3(l1, [], l3) do
    rmerge(l1, l3)
  end

  def rmerge3(l1, l2, []) do
    rmerge(l1, l2)
  end

  def rmerge3(l1, [h2 | t2], [h3 | t3]) do
    :lists.reverse(rmerge3_1(l1, [], h2, t2, h3, t3), [])
  end

  def merge(t1, []) do
    t1
  end

  def merge(t1, [h2 | t2]) do
    :lists.reverse(merge2_1(t1, h2, t2, []), [])
  end

  def rmerge(t1, []) do
    t1
  end

  def rmerge(t1, [h2 | t2]) do
    :lists.reverse(rmerge2_1(t1, h2, t2, []), [])
  end

  def concat(list) do
    flatmap(&thing_to_list/1, list)
  end

  defp thing_to_list(x) when is_integer(x) do
    :erlang.integer_to_list(x)
  end

  defp thing_to_list(x) when is_float(x) do
    :erlang.float_to_list(x)
  end

  defp thing_to_list(x) when is_atom(x) do
    :erlang.atom_to_list(x)
  end

  defp thing_to_list(x) when is_list(x) do
    x
  end

  def flatten(list) when is_list(list) do
    do_flatten(list, [])
  end

  def flatten(list, tail)
      when is_list(list) and
             is_list(tail) do
    do_flatten(list, tail)
  end

  defp do_flatten([h | t], tail) when is_list(h) do
    do_flatten(h, do_flatten(t, tail))
  end

  defp do_flatten([h | t], tail) do
    [h | do_flatten(t, tail)]
  end

  defp do_flatten([], tail) do
    tail
  end

  def flatlength(list) do
    flatlength(list, 0)
  end

  defp flatlength([h | t], l) when is_list(h) do
    flatlength(h, flatlength(t, l))
  end

  defp flatlength([_ | t], l) do
    flatlength(t, l + 1)
  end

  defp flatlength([], l) do
    l
  end

  def keydelete(k, n, l) when is_integer(n) and n > 0 do
    keydelete3(k, n, l)
  end

  defp keydelete3(key, n, [h | t])
       when :erlang.element(
              n,
              h
            ) == key do
    t
  end

  defp keydelete3(key, n, [h | t]) do
    [h | keydelete3(key, n, t)]
  end

  defp keydelete3(_, _, []) do
    []
  end

  def keyreplace(k, n, l, new)
      when is_integer(n) and n > 0 and
             is_tuple(new) do
    keyreplace3(k, n, l, new)
  end

  defp keyreplace3(key, pos, [tup | tail], new)
       when :erlang.element(pos, tup) == key do
    [new | tail]
  end

  defp keyreplace3(key, pos, [h | t], new) do
    [h | keyreplace3(key, pos, t, new)]
  end

  defp keyreplace3(_, _, [], _) do
    []
  end

  def keytake(key, n, l) when is_integer(n) and n > 0 do
    keytake(key, n, l, [])
  end

  defp keytake(key, n, [h | t], l)
       when :erlang.element(
              n,
              h
            ) == key do
    {:value, h, :lists.reverse(l, t)}
  end

  defp keytake(key, n, [h | t], l) do
    keytake(key, n, t, [h | l])
  end

  defp keytake(_K, _N, [], _L) do
    false
  end

  def keystore(k, n, l, new)
      when is_integer(n) and n > 0 and
             is_tuple(new) do
    keystore2(k, n, l, new)
  end

  defp keystore2(key, n, [h | t], new)
       when :erlang.element(
              n,
              h
            ) == key do
    [new | t]
  end

  defp keystore2(key, n, [h | t], new) do
    [h | keystore2(key, n, t, new)]
  end

  defp keystore2(_Key, _N, [], new) do
    [new]
  end

  def keysort(i, l) when is_integer(i) and i > 0 do
    case l do
      [] ->
        l

      [_] ->
        l

      [x, y | t] ->
        case {:erlang.element(i, x), :erlang.element(i, y)} do
          {eX, eY} when eX <= eY ->
            case t do
              [] ->
                l

              [z] ->
                case :erlang.element(i, z) do
                  eZ when eY <= eZ ->
                    l

                  eZ when eX <= eZ ->
                    [x, z, y]

                  _EZ ->
                    [z, x, y]
                end

              _ when x == y ->
                keysort_1(i, y, eY, t, [x])

              _ ->
                keysplit_1(i, x, eX, y, eY, t, [], [])
            end

          {eX, eY} ->
            case t do
              [] ->
                [y, x]

              [z] ->
                case :erlang.element(i, z) do
                  eZ when eX <= eZ ->
                    [y, x | t]

                  eZ when eY <= eZ ->
                    [y, z, x]

                  _EZ ->
                    [z, y, x]
                end

              _ ->
                keysplit_2(i, x, eX, y, eY, t, [], [])
            end
        end
    end
  end

  defp keysort_1(i, x, eX, [y | l], r) when x == y do
    keysort_1(i, y, eX, l, [x | r])
  end

  defp keysort_1(i, x, eX, [y | l], r) do
    case :erlang.element(i, y) do
      eY when eX <= eY ->
        keysplit_1(i, x, eX, y, eY, l, r, [])

      eY ->
        keysplit_2(i, x, eX, y, eY, l, r, [])
    end
  end

  defp keysort_1(_I, x, _EX, [], r) do
    :lists.reverse(r, [x])
  end

  def keymerge(index, t1, l2)
      when is_integer(index) and
             index > 0 do
    case l2 do
      [] ->
        t1

      [h2 | t2] ->
        e2 = :erlang.element(index, h2)
        m = keymerge2_1(index, t1, e2, h2, t2, [])
        :lists.reverse(m, [])
    end
  end

  def rkeymerge(index, t1, l2)
      when is_integer(index) and
             index > 0 do
    case l2 do
      [] ->
        t1

      [h2 | t2] ->
        e2 = :erlang.element(index, h2)
        m = rkeymerge2_1(index, t1, e2, h2, t2, [])
        :lists.reverse(m, [])
    end
  end

  def ukeysort(i, l) when is_integer(i) and i > 0 do
    case l do
      [] ->
        l

      [_] ->
        l

      [x, y | t] ->
        case {:erlang.element(i, x), :erlang.element(i, y)} do
          {eX, eY} when eX == eY ->
            ukeysort_1(i, x, eX, t)

          {eX, eY} when eX < eY ->
            case t do
              [] ->
                l

              [z] ->
                case :erlang.element(i, z) do
                  eZ when eY == eZ ->
                    [x, y]

                  eZ when eY < eZ ->
                    [x, y, z]

                  eZ when eZ == eX ->
                    [x, y]

                  eZ when eX <= eZ ->
                    [x, z, y]

                  _EZ ->
                    [z, x, y]
                end

              _ ->
                ukeysplit_1(i, x, eX, y, eY, t, [], [])
            end

          {eX, eY} ->
            case t do
              [] ->
                [y, x]

              [z] ->
                case :erlang.element(i, z) do
                  eZ when eX == eZ ->
                    [y, x]

                  eZ when eX < eZ ->
                    [y, x, z]

                  eZ when eY == eZ ->
                    [y, x]

                  eZ when eY <= eZ ->
                    [y, z, x]

                  _EZ ->
                    [z, y, x]
                end

              _ ->
                ukeysplit_2(i, y, eY, t, [x])
            end
        end
    end
  end

  defp ukeysort_1(i, x, eX, [y | l]) do
    case :erlang.element(i, y) do
      eY when eX == eY ->
        ukeysort_1(i, x, eX, l)

      eY when eX < eY ->
        ukeysplit_1(i, x, eX, y, eY, l, [], [])

      eY ->
        ukeysplit_2(i, y, eY, l, [x])
    end
  end

  defp ukeysort_1(_I, x, _EX, []) do
    [x]
  end

  def ukeymerge(index, l1, t2)
      when is_integer(index) and
             index > 0 do
    case l1 do
      [] ->
        t2

      [h1 | t1] ->
        e1 = :erlang.element(index, h1)
        m = ukeymerge2_2(index, t1, e1, h1, t2, [])
        :lists.reverse(m, [])
    end
  end

  def rukeymerge(index, t1, l2)
      when is_integer(index) and
             index > 0 do
    case l2 do
      [] ->
        t1

      [h2 | t2] ->
        e2 = :erlang.element(index, h2)
        m = rukeymerge2_1(index, t1, e2, t2, [], h2)
        :lists.reverse(m, [])
    end
  end

  def keymap(fun, index, [tup | tail]) do
    [
      :erlang.setelement(index, tup, fun.(:erlang.element(index, tup)))
      | keymap(fun, index, tail)
    ]
  end

  def keymap(fun, index, [])
      when is_integer(index) and
             index >= 1 and is_function(fun, 1) do
    []
  end

  def sort(fun, []) when is_function(fun, 2) do
    []
  end

  def sort(fun, [_] = l) when is_function(fun, 2) do
    l
  end

  def sort(fun, [x, y | t]) do
    case fun.(x, y) do
      true ->
        fsplit_1(y, x, fun, t, [], [])

      false ->
        fsplit_2(y, x, fun, t, [], [])
    end
  end

  def merge(fun, t1, [h2 | t2]) when is_function(fun, 2) do
    :lists.reverse(fmerge2_1(t1, h2, fun, t2, []), [])
  end

  def merge(fun, t1, []) when is_function(fun, 2) do
    t1
  end

  def rmerge(fun, t1, [h2 | t2]) when is_function(fun, 2) do
    :lists.reverse(rfmerge2_1(t1, h2, fun, t2, []), [])
  end

  def rmerge(fun, t1, []) when is_function(fun, 2) do
    t1
  end

  def usort(fun, [_] = l) when is_function(fun, 2) do
    l
  end

  def usort(fun, [] = l) when is_function(fun, 2) do
    l
  end

  def usort(fun, [x | l]) when is_function(fun, 2) do
    usort_1(fun, x, l)
  end

  defp usort_1(fun, x, [y | l]) do
    case fun.(x, y) do
      true ->
        case fun.(y, x) do
          true ->
            case l do
              [] ->
                [x]

              _ ->
                usort_1(fun, x, l)
            end

          false ->
            ufsplit_1(y, x, fun, l, [], [])
        end

      false ->
        ufsplit_2(y, l, fun, [x])
    end
  end

  def umerge(fun, [], t2) when is_function(fun, 2) do
    t2
  end

  def umerge(fun, [h1 | t1], t2) when is_function(fun, 2) do
    :lists.reverse(ufmerge2_2(h1, t1, fun, t2, []), [])
  end

  def rumerge(fun, t1, []) when is_function(fun, 2) do
    t1
  end

  def rumerge(fun, t1, [h2 | t2]) when is_function(fun, 2) do
    :lists.reverse(rufmerge2_1(t1, h2, fun, t2, []), [])
  end

  def usort([x, y | l] = l0) when x < y do
    case l do
      [] ->
        l0

      [z] when y < z ->
        l0

      [z] when y == z ->
        [x, y]

      [z] when z < x ->
        [z, x, y]

      [z] when z == x ->
        [x, y]

      [z] ->
        [x, z, y]

      _ ->
        usplit_1(x, y, l, [], [])
    end
  end

  def usort([x, y | l]) when x > y do
    case l do
      [] ->
        [y, x]

      [z] when x < z ->
        [y, x | l]

      [z] when x == z ->
        [y, x]

      [z] when z < y ->
        [z, y, x]

      [z] when z == y ->
        [y, x]

      [z] ->
        [y, z, x]

      _ ->
        usplit_2(x, y, l, [], [])
    end
  end

  def usort([x, _Y | l]) do
    usort_1(x, l)
  end

  def usort([_] = l) do
    l
  end

  def usort([]) do
    []
  end

  defp usort_1(x, [y | l]) when x == y do
    usort_1(x, l)
  end

  defp usort_1(x, [y | l]) when x < y do
    usplit_1(x, y, l, [], [])
  end

  defp usort_1(x, [y | l]) do
    usplit_2(x, y, l, [], [])
  end

  defp usort_1(x, []) do
    [x]
  end

  def umerge(l) do
    umergel(l)
  end

  def umerge3(l1, [], l3) do
    umerge(l1, l3)
  end

  def umerge3(l1, l2, []) do
    umerge(l1, l2)
  end

  def umerge3(l1, [h2 | t2], [h3 | t3]) do
    :lists.reverse(
      umerge3_1(l1, [h2 | h3], t2, h2, [], t3, h3),
      []
    )
  end

  def rumerge3(l1, [], l3) do
    rumerge(l1, l3)
  end

  def rumerge3(l1, l2, []) do
    rumerge(l1, l2)
  end

  def rumerge3(l1, [h2 | t2], [h3 | t3]) do
    :lists.reverse(rumerge3_1(l1, t2, h2, [], t3, h3), [])
  end

  def umerge([], t2) do
    t2
  end

  def umerge([h1 | t1], t2) do
    :lists.reverse(umerge2_2(t1, t2, [], h1), [])
  end

  def rumerge(t1, []) do
    t1
  end

  def rumerge(t1, [h2 | t2]) do
    :lists.reverse(rumerge2_1(t1, t2, [], h2), [])
  end

  def all(pred, [hd | tail]) do
    case pred.(hd) do
      true ->
        all(pred, tail)

      false ->
        false
    end
  end

  def all(pred, []) when is_function(pred, 1) do
    true
  end

  def any(pred, [hd | tail]) do
    case pred.(hd) do
      true ->
        true

      false ->
        any(pred, tail)
    end
  end

  def any(pred, []) when is_function(pred, 1) do
    false
  end

  def map(f, [h | t]) do
    [f.(h) | map(f, t)]
  end

  def map(f, []) when is_function(f, 1) do
    []
  end

  def flatmap(f, [hd | tail]) do
    f.(hd) ++ flatmap(f, tail)
  end

  def flatmap(f, []) when is_function(f, 1) do
    []
  end

  def foldl(f, accu, [hd | tail]) do
    foldl(f, f.(hd, accu), tail)
  end

  def foldl(f, accu, []) when is_function(f, 2) do
    accu
  end

  def foldr(f, accu, [hd | tail]) do
    f.(hd, foldr(f, accu, tail))
  end

  def foldr(f, accu, []) when is_function(f, 2) do
    accu
  end

  def filter(pred, list) when is_function(pred, 1) do
    for e <- list, pred.(e) do
      e
    end
  end

  def partition(pred, l) do
    partition(pred, l, [], [])
  end

  defp partition(pred, [h | t], as, bs) do
    case pred.(h) do
      true ->
        partition(pred, t, [h | as], bs)

      false ->
        partition(pred, t, as, [h | bs])
    end
  end

  defp partition(pred, [], as, bs) when is_function(pred, 1) do
    {reverse(as), reverse(bs)}
  end

  def filtermap(f, [hd | tail]) do
    case f.(hd) do
      true ->
        [hd | filtermap(f, tail)]

      {true, val} ->
        [val | filtermap(f, tail)]

      false ->
        filtermap(f, tail)
    end
  end

  def filtermap(f, []) when is_function(f, 1) do
    []
  end

  def zf(f, l) do
    filtermap(f, l)
  end

  def foreach(f, [hd | tail]) do
    f.(hd)
    foreach(f, tail)
  end

  def foreach(f, []) when is_function(f, 1) do
    :ok
  end

  def mapfoldl(f, accu0, [hd | tail]) do
    {r, accu1} = f.(hd, accu0)
    {rs, accu2} = mapfoldl(f, accu1, tail)
    {[r | rs], accu2}
  end

  def mapfoldl(f, accu, []) when is_function(f, 2) do
    {[], accu}
  end

  def mapfoldr(f, accu0, [hd | tail]) do
    {rs, accu1} = mapfoldr(f, accu0, tail)
    {r, accu2} = f.(hd, accu1)
    {[r | rs], accu2}
  end

  def mapfoldr(f, accu, []) when is_function(f, 2) do
    {[], accu}
  end

  def takewhile(pred, [hd | tail]) do
    case pred.(hd) do
      true ->
        [hd | takewhile(pred, tail)]

      false ->
        []
    end
  end

  def takewhile(pred, []) when is_function(pred, 1) do
    []
  end

  def dropwhile(pred, [hd | tail] = rest) do
    case pred.(hd) do
      true ->
        dropwhile(pred, tail)

      false ->
        rest
    end
  end

  def dropwhile(pred, []) when is_function(pred, 1) do
    []
  end

  def search(pred, [hd | tail]) do
    case pred.(hd) do
      true ->
        {:value, hd}

      false ->
        search(pred, tail)
    end
  end

  def search(pred, []) when is_function(pred, 1) do
    false
  end

  def splitwith(pred, list) when is_function(pred, 1) do
    splitwith(pred, list, [])
  end

  defp splitwith(pred, [hd | tail], taken) do
    case pred.(hd) do
      true ->
        splitwith(pred, tail, [hd | taken])

      false ->
        {reverse(taken), [hd | tail]}
    end
  end

  defp splitwith(pred, [], taken) when is_function(pred, 1) do
    {reverse(taken), []}
  end

  def split(n, list)
      when is_integer(n) and n >= 0 and
             is_list(list) do
    case split(n, list, []) do
      {_, _} = result ->
        result

      fault when is_atom(fault) ->
        :erlang.error(fault, [n, list])
    end
  end

  def split(n, list) do
    :erlang.error(:badarg, [n, list])
  end

  defp split(0, l, r) do
    {:lists.reverse(r, []), l}
  end

  defp split(n, [h | t], r) do
    split(n - 1, t, [h | r])
  end

  defp split(_, [], _) do
    :badarg
  end

  def join(_Sep, []) do
    []
  end

  def join(sep, [h | t]) do
    [h | join_prepend(sep, t)]
  end

  defp join_prepend(_Sep, []) do
    []
  end

  defp join_prepend(sep, [h | t]) do
    [sep, h | join_prepend(sep, t)]
  end

  defp split_1(x, y, [z | l], r, rs) when z >= y do
    split_1(y, z, l, [x | r], rs)
  end

  defp split_1(x, y, [z | l], r, rs) when z >= x do
    split_1(z, y, l, [x | r], rs)
  end

  defp split_1(x, y, [z | l], [], rs) do
    split_1(x, y, l, [z], rs)
  end

  defp split_1(x, y, [z | l], r, rs) do
    split_1_1(x, y, l, r, rs, z)
  end

  defp split_1(x, y, [], r, rs) do
    rmergel([[y, x | r] | rs], [])
  end

  defp split_1_1(x, y, [z | l], r, rs, s) when z >= y do
    split_1_1(y, z, l, [x | r], rs, s)
  end

  defp split_1_1(x, y, [z | l], r, rs, s) when z >= x do
    split_1_1(z, y, l, [x | r], rs, s)
  end

  defp split_1_1(x, y, [z | l], r, rs, s) when s <= z do
    split_1(s, z, l, [], [[y, x | r] | rs])
  end

  defp split_1_1(x, y, [z | l], r, rs, s) do
    split_1(z, s, l, [], [[y, x | r] | rs])
  end

  defp split_1_1(x, y, [], r, rs, s) do
    rmergel([[s], [y, x | r] | rs], [])
  end

  defp split_2(x, y, [z | l], r, rs) when z <= y do
    split_2(y, z, l, [x | r], rs)
  end

  defp split_2(x, y, [z | l], r, rs) when z <= x do
    split_2(z, y, l, [x | r], rs)
  end

  defp split_2(x, y, [z | l], [], rs) do
    split_2(x, y, l, [z], rs)
  end

  defp split_2(x, y, [z | l], r, rs) do
    split_2_1(x, y, l, r, rs, z)
  end

  defp split_2(x, y, [], r, rs) do
    mergel([[y, x | r] | rs], [])
  end

  defp split_2_1(x, y, [z | l], r, rs, s) when z <= y do
    split_2_1(y, z, l, [x | r], rs, s)
  end

  defp split_2_1(x, y, [z | l], r, rs, s) when z <= x do
    split_2_1(z, y, l, [x | r], rs, s)
  end

  defp split_2_1(x, y, [z | l], r, rs, s) when s > z do
    split_2(s, z, l, [], [[y, x | r] | rs])
  end

  defp split_2_1(x, y, [z | l], r, rs, s) do
    split_2(z, s, l, [], [[y, x | r] | rs])
  end

  defp split_2_1(x, y, [], r, rs, s) do
    mergel([[s], [y, x | r] | rs], [])
  end

  defp mergel([[] | l], acc) do
    mergel(l, acc)
  end

  defp mergel([t1, [h2 | t2], [h3 | t3] | l], acc) do
    mergel(l, [merge3_1(t1, [], h2, t2, h3, t3) | acc])
  end

  defp mergel([t1, [h2 | t2]], acc) do
    rmergel([merge2_1(t1, h2, t2, []) | acc], [])
  end

  defp mergel([l], []) do
    l
  end

  defp mergel([l], acc) do
    rmergel([:lists.reverse(l, []) | acc], [])
  end

  defp mergel([], []) do
    []
  end

  defp mergel([], acc) do
    rmergel(acc, [])
  end

  defp mergel([a, [] | l], acc) do
    mergel([a | l], acc)
  end

  defp mergel([a, b, [] | l], acc) do
    mergel([a, b | l], acc)
  end

  defp rmergel([[h3 | t3], [h2 | t2], t1 | l], acc) do
    rmergel(l, [rmerge3_1(t1, [], h2, t2, h3, t3) | acc])
  end

  defp rmergel([[h2 | t2], t1], acc) do
    mergel([rmerge2_1(t1, h2, t2, []) | acc], [])
  end

  defp rmergel([l], acc) do
    mergel([:lists.reverse(l, []) | acc], [])
  end

  defp rmergel([], acc) do
    mergel(acc, [])
  end

  defp merge3_1([h1 | t1], m, h2, t2, h3, t3) when h1 <= h2 do
    merge3_12(t1, h1, h2, t2, h3, t3, m)
  end

  defp merge3_1([h1 | t1], m, h2, t2, h3, t3) do
    merge3_21(t1, h1, h2, t2, h3, t3, m)
  end

  defp merge3_1([], m, h2, t2, h3, t3) when h2 <= h3 do
    merge2_1(t2, h3, t3, [h2 | m])
  end

  defp merge3_1([], m, h2, t2, h3, t3) do
    merge2_2(t2, h3, t3, m, h2)
  end

  defp merge3_2(t1, h1, m, [h2 | t2], h3, t3) when h1 <= h2 do
    merge3_12(t1, h1, h2, t2, h3, t3, m)
  end

  defp merge3_2(t1, h1, m, [h2 | t2], h3, t3) do
    merge3_21(t1, h1, h2, t2, h3, t3, m)
  end

  defp merge3_2(t1, h1, m, [], h3, t3) when h1 <= h3 do
    merge2_1(t1, h3, t3, [h1 | m])
  end

  defp merge3_2(t1, h1, m, [], h3, t3) do
    merge2_2(t1, h3, t3, m, h1)
  end

  defp merge3_12(t1, h1, h2, t2, h3, t3, m) when h1 <= h3 do
    merge3_1(t1, [h1 | m], h2, t2, h3, t3)
  end

  defp merge3_12(t1, h1, h2, t2, h3, t3, m) do
    merge3_12_3(t1, h1, h2, t2, [h3 | m], t3)
  end

  defp merge3_12_3(t1, h1, h2, t2, m, [h3 | t3]) when h1 <= h3 do
    merge3_1(t1, [h1 | m], h2, t2, h3, t3)
  end

  defp merge3_12_3(t1, h1, h2, t2, m, [h3 | t3]) do
    merge3_12_3(t1, h1, h2, t2, [h3 | m], t3)
  end

  defp merge3_12_3(t1, h1, h2, t2, m, []) do
    merge2_1(t1, h2, t2, [h1 | m])
  end

  defp merge3_21(t1, h1, h2, t2, h3, t3, m) when h2 <= h3 do
    merge3_2(t1, h1, [h2 | m], t2, h3, t3)
  end

  defp merge3_21(t1, h1, h2, t2, h3, t3, m) do
    merge3_21_3(t1, h1, h2, t2, [h3 | m], t3)
  end

  defp merge3_21_3(t1, h1, h2, t2, m, [h3 | t3]) when h2 <= h3 do
    merge3_2(t1, h1, [h2 | m], t2, h3, t3)
  end

  defp merge3_21_3(t1, h1, h2, t2, m, [h3 | t3]) do
    merge3_21_3(t1, h1, h2, t2, [h3 | m], t3)
  end

  defp merge3_21_3(t1, h1, h2, t2, m, []) do
    merge2_2(t1, h2, t2, m, h1)
  end

  defp rmerge3_1([h1 | t1], m, h2, t2, h3, t3) when h1 <= h2 do
    rmerge3_12(t1, h1, h2, t2, h3, t3, m)
  end

  defp rmerge3_1([h1 | t1], m, h2, t2, h3, t3) do
    rmerge3_21(t1, h1, h2, t2, h3, t3, m)
  end

  defp rmerge3_1([], m, h2, t2, h3, t3) when h2 <= h3 do
    rmerge2_2(t2, h3, t3, m, h2)
  end

  defp rmerge3_1([], m, h2, t2, h3, t3) do
    rmerge2_1(t2, h3, t3, [h2 | m])
  end

  defp rmerge3_2(t1, h1, m, [h2 | t2], h3, t3) when h1 <= h2 do
    rmerge3_12(t1, h1, h2, t2, h3, t3, m)
  end

  defp rmerge3_2(t1, h1, m, [h2 | t2], h3, t3) do
    rmerge3_21(t1, h1, h2, t2, h3, t3, m)
  end

  defp rmerge3_2(t1, h1, m, [], h3, t3) when h1 <= h3 do
    rmerge2_2(t1, h3, t3, m, h1)
  end

  defp rmerge3_2(t1, h1, m, [], h3, t3) do
    rmerge2_1(t1, h3, t3, [h1 | m])
  end

  defp rmerge3_12(t1, h1, h2, t2, h3, t3, m) when h2 <= h3 do
    rmerge3_12_3(t1, h1, h2, t2, [h3 | m], t3)
  end

  defp rmerge3_12(t1, h1, h2, t2, h3, t3, m) do
    rmerge3_2(t1, h1, [h2 | m], t2, h3, t3)
  end

  defp rmerge3_12_3(t1, h1, h2, t2, m, [h3 | t3]) when h2 <= h3 do
    rmerge3_12_3(t1, h1, h2, t2, [h3 | m], t3)
  end

  defp rmerge3_12_3(t1, h1, h2, t2, m, [h3 | t3]) do
    rmerge3_2(t1, h1, [h2 | m], t2, h3, t3)
  end

  defp rmerge3_12_3(t1, h1, h2, t2, m, []) do
    rmerge2_2(t1, h2, t2, m, h1)
  end

  defp rmerge3_21(t1, h1, h2, t2, h3, t3, m) when h1 <= h3 do
    rmerge3_21_3(t1, h1, h2, t2, [h3 | m], t3)
  end

  defp rmerge3_21(t1, h1, h2, t2, h3, t3, m) do
    rmerge3_1(t1, [h1 | m], h2, t2, h3, t3)
  end

  defp rmerge3_21_3(t1, h1, h2, t2, m, [h3 | t3]) when h1 <= h3 do
    rmerge3_21_3(t1, h1, h2, t2, [h3 | m], t3)
  end

  defp rmerge3_21_3(t1, h1, h2, t2, m, [h3 | t3]) do
    rmerge3_1(t1, [h1 | m], h2, t2, h3, t3)
  end

  defp rmerge3_21_3(t1, h1, h2, t2, m, []) do
    rmerge2_1(t1, h2, t2, [h1 | m])
  end

  defp merge2_1([h1 | t1], h2, t2, m) when h1 <= h2 do
    merge2_1(t1, h2, t2, [h1 | m])
  end

  defp merge2_1([h1 | t1], h2, t2, m) do
    merge2_2(t1, h2, t2, m, h1)
  end

  defp merge2_1([], h2, t2, m) do
    :lists.reverse(t2, [h2 | m])
  end

  defp merge2_2(t1, hdM, [h2 | t2], m, h1) when h1 <= h2 do
    merge2_1(t1, h2, t2, [h1, hdM | m])
  end

  defp merge2_2(t1, hdM, [h2 | t2], m, h1) do
    merge2_2(t1, h2, t2, [hdM | m], h1)
  end

  defp merge2_2(t1, hdM, [], m, h1) do
    :lists.reverse(t1, [h1, hdM | m])
  end

  defp rmerge2_1([h1 | t1], h2, t2, m) when h1 <= h2 do
    rmerge2_2(t1, h2, t2, m, h1)
  end

  defp rmerge2_1([h1 | t1], h2, t2, m) do
    rmerge2_1(t1, h2, t2, [h1 | m])
  end

  defp rmerge2_1([], h2, t2, m) do
    :lists.reverse(t2, [h2 | m])
  end

  defp rmerge2_2(t1, hdM, [h2 | t2], m, h1) when h1 <= h2 do
    rmerge2_2(t1, h2, t2, [hdM | m], h1)
  end

  defp rmerge2_2(t1, hdM, [h2 | t2], m, h1) do
    rmerge2_1(t1, h2, t2, [h1, hdM | m])
  end

  defp rmerge2_2(t1, hdM, [], m, h1) do
    :lists.reverse(t1, [h1, hdM | m])
  end

  defp usplit_1(x, y, [z | l], r, rs) when z > y do
    usplit_1(y, z, l, [x | r], rs)
  end

  defp usplit_1(x, y, [z | l], r, rs) when z == y do
    usplit_1(x, y, l, r, rs)
  end

  defp usplit_1(x, y, [z | l], r, rs) when z > x do
    usplit_1(z, y, l, [x | r], rs)
  end

  defp usplit_1(x, y, [z | l], r, rs) when z == x do
    usplit_1(x, y, l, r, rs)
  end

  defp usplit_1(x, y, [z | l], [], rs) do
    usplit_1(x, y, l, [z], rs)
  end

  defp usplit_1(x, y, [z | l], r, rs) do
    usplit_1_1(x, y, l, r, rs, z)
  end

  defp usplit_1(x, y, [], r, rs) do
    rumergel([[y, x | r] | rs], [], :asc)
  end

  defp usplit_1_1(x, y, [z | l], r, rs, s) when z > y do
    usplit_1_1(y, z, l, [x | r], rs, s)
  end

  defp usplit_1_1(x, y, [z | l], r, rs, s) when z == y do
    usplit_1_1(x, y, l, r, rs, s)
  end

  defp usplit_1_1(x, y, [z | l], r, rs, s) when z > x do
    usplit_1_1(z, y, l, [x | r], rs, s)
  end

  defp usplit_1_1(x, y, [z | l], r, rs, s) when z == x do
    usplit_1_1(x, y, l, r, rs, s)
  end

  defp usplit_1_1(x, y, [z | l], r, rs, s) when z > s do
    usplit_1(s, z, l, [], [[y, x | r] | rs])
  end

  defp usplit_1_1(x, y, [z | l], r, rs, s) when z == s do
    usplit_1_1(x, y, l, r, rs, s)
  end

  defp usplit_1_1(x, y, [z | l], r, rs, s) do
    usplit_1(z, s, l, [], [[y, x | r] | rs])
  end

  defp usplit_1_1(x, y, [], r, rs, s) do
    rumergel([[s], [y, x | r] | rs], [], :asc)
  end

  defp usplit_2(x, y, [z | l], r, rs) when z < y do
    usplit_2(y, z, l, [x | r], rs)
  end

  defp usplit_2(x, y, [z | l], r, rs) when z == y do
    usplit_2(x, y, l, r, rs)
  end

  defp usplit_2(x, y, [z | l], r, rs) when z < x do
    usplit_2(z, y, l, [x | r], rs)
  end

  defp usplit_2(x, y, [z | l], r, rs) when z == x do
    usplit_2(x, y, l, r, rs)
  end

  defp usplit_2(x, y, [z | l], [], rs) do
    usplit_2(x, y, l, [z], rs)
  end

  defp usplit_2(x, y, [z | l], r, rs) do
    usplit_2_1(x, y, l, r, rs, z)
  end

  defp usplit_2(x, y, [], r, rs) do
    umergel([[y, x | r] | rs], [], :desc)
  end

  defp usplit_2_1(x, y, [z | l], r, rs, s) when z < y do
    usplit_2_1(y, z, l, [x | r], rs, s)
  end

  defp usplit_2_1(x, y, [z | l], r, rs, s) when z == y do
    usplit_2_1(x, y, l, r, rs, s)
  end

  defp usplit_2_1(x, y, [z | l], r, rs, s) when z < x do
    usplit_2_1(z, y, l, [x | r], rs, s)
  end

  defp usplit_2_1(x, y, [z | l], r, rs, s) when z == x do
    usplit_2_1(x, y, l, r, rs, s)
  end

  defp usplit_2_1(x, y, [z | l], r, rs, s) when z < s do
    usplit_2(s, z, l, [], [[y, x | r] | rs])
  end

  defp usplit_2_1(x, y, [z | l], r, rs, s) when z == s do
    usplit_2_1(x, y, l, r, rs, s)
  end

  defp usplit_2_1(x, y, [z | l], r, rs, s) do
    usplit_2(z, s, l, [], [[y, x | r] | rs])
  end

  defp usplit_2_1(x, y, [], r, rs, s) do
    umergel([[s], [y, x | r] | rs], [], :desc)
  end

  defp umergel(l) do
    umergel(l, [], :asc)
  end

  defp umergel([[] | l], acc, o) do
    umergel(l, acc, o)
  end

  defp umergel([t1, [h2 | t2], [h3 | t3] | l], acc, :asc) do
    umergel(
      l,
      [umerge3_1(t1, [h2 | h3], t2, h2, [], t3, h3) | acc],
      :asc
    )
  end

  defp umergel([[h3 | t3], [h2 | t2], t1 | l], acc, :desc) do
    umergel(
      l,
      [umerge3_1(t1, [h2 | h3], t2, h2, [], t3, h3) | acc],
      :desc
    )
  end

  defp umergel([a, [] | l], acc, o) do
    umergel([a | l], acc, o)
  end

  defp umergel([a, b, [] | l], acc, o) do
    umergel([a, b | l], acc, o)
  end

  defp umergel([[h1 | t1], t2 | l], acc, :asc) do
    umergel(l, [umerge2_2(t1, t2, [], h1) | acc], :asc)
  end

  defp umergel([t2, [h1 | t1] | l], acc, :desc) do
    umergel(l, [umerge2_2(t1, t2, [], h1) | acc], :desc)
  end

  defp umergel([l], [], _O) do
    l
  end

  defp umergel([l], acc, o) do
    rumergel([:lists.reverse(l, []) | acc], [], o)
  end

  defp umergel([], [], _O) do
    []
  end

  defp umergel([], acc, o) do
    rumergel(acc, [], o)
  end

  defp rumergel([[h3 | t3], [h2 | t2], t1 | l], acc, :asc) do
    rumergel(l, [rumerge3_1(t1, t2, h2, [], t3, h3) | acc], :asc)
  end

  defp rumergel([t1, [h2 | t2], [h3 | t3] | l], acc, :desc) do
    rumergel(l, [rumerge3_1(t1, t2, h2, [], t3, h3) | acc], :desc)
  end

  defp rumergel([[h2 | t2], t1 | l], acc, :asc) do
    rumergel(l, [rumerge2_1(t1, t2, [], h2) | acc], :asc)
  end

  defp rumergel([t1, [h2 | t2] | l], acc, :desc) do
    rumergel(l, [rumerge2_1(t1, t2, [], h2) | acc], :desc)
  end

  defp rumergel([l], acc, o) do
    umergel([:lists.reverse(l, []) | acc], [], o)
  end

  defp rumergel([], acc, o) do
    umergel(acc, [], o)
  end

  defp umerge3_1([h1 | t1], hdM, t2, h2, m, t3, h3)
       when h1 <= h2 do
    umerge3_12(t1, h1, t2, h2, m, t3, h3, hdM)
  end

  defp umerge3_1([h1 | t1], hdM, t2, h2, m, t3, h3)
       when h2 == hdM do
    umerge3_2(t1, h1, t2, h2, m, t3, h3)
  end

  defp umerge3_1([h1 | t1], hdM, t2, h2, m, t3, h3) do
    umerge3_21(t1, h1, t2, h2, m, t3, h3, hdM)
  end

  defp umerge3_1([], hdM, t2, h2, m, t3, h3) when h2 == hdM do
    umerge2_1(t2, t3, m, hdM, h3)
  end

  defp umerge3_1([], _HdM, t2, h2, m, t3, h3) when h2 <= h3 do
    umerge2_1(t2, t3, [h2 | m], h2, h3)
  end

  defp umerge3_1([], hdM, t2, h2, m, t3, h3) when h3 == hdM do
    umerge2_2(t2, t3, m, h2)
  end

  defp umerge3_1([], _HdM, t2, h2, m, t3, h3) do
    umerge2_2(t2, t3, [h3 | m], h2)
  end

  defp umerge3_2(t1, h1, [h2 | t2], hdM, m, t3, h3)
       when h1 <= h2 do
    umerge3_12(t1, h1, t2, h2, m, t3, h3, hdM)
  end

  defp umerge3_2(t1, h1, [h2 | t2], hdM, m, t3, h3) do
    umerge3_21(t1, h1, t2, h2, m, t3, h3, hdM)
  end

  defp umerge3_2(t1, h1, [], _HdM, m, t3, h3) when h1 <= h3 do
    umerge2_1(t1, t3, [h1 | m], h1, h3)
  end

  defp umerge3_2(t1, h1, [], hdM, m, t3, h3) when h3 == hdM do
    umerge2_2(t1, t3, m, h1)
  end

  defp umerge3_2(t1, h1, [], _HdM, m, t3, h3) do
    umerge2_2(t1, t3, [h3 | m], h1)
  end

  defp umerge3_12(t1, h1, t2, h2, m, t3, h3, _HdM)
       when h1 <= h3 do
    umerge3_1(t1, h1, t2, h2, [h1 | m], t3, h3)
  end

  defp umerge3_12(t1, h1, t2, h2, m, t3, h3, hdM)
       when h3 == hdM do
    umerge3_12_3(t1, h1, t2, h2, m, t3)
  end

  defp umerge3_12(t1, h1, t2, h2, m, t3, h3, _HdM) do
    umerge3_12_3(t1, h1, t2, h2, [h3 | m], t3)
  end

  defp umerge3_12_3(t1, h1, t2, h2, m, [h3 | t3]) when h1 <= h3 do
    umerge3_1(t1, h1, t2, h2, [h1 | m], t3, h3)
  end

  defp umerge3_12_3(t1, h1, t2, h2, m, [h3 | t3]) do
    umerge3_12_3(t1, h1, t2, h2, [h3 | m], t3)
  end

  defp umerge3_12_3(t1, h1, t2, h2, m, []) do
    umerge2_1(t1, t2, [h1 | m], h1, h2)
  end

  defp umerge3_21(t1, h1, t2, h2, m, t3, h3, _HdM)
       when h2 <= h3 do
    umerge3_2(t1, h1, t2, h2, [h2 | m], t3, h3)
  end

  defp umerge3_21(t1, h1, t2, h2, m, t3, h3, hdM)
       when h3 == hdM do
    umerge3_21_3(t1, h1, t2, h2, m, t3)
  end

  defp umerge3_21(t1, h1, t2, h2, m, t3, h3, _HdM) do
    umerge3_21_3(t1, h1, t2, h2, [h3 | m], t3)
  end

  defp umerge3_21_3(t1, h1, t2, h2, m, [h3 | t3]) when h2 <= h3 do
    umerge3_2(t1, h1, t2, h2, [h2 | m], t3, h3)
  end

  defp umerge3_21_3(t1, h1, t2, h2, m, [h3 | t3]) do
    umerge3_21_3(t1, h1, t2, h2, [h3 | m], t3)
  end

  defp umerge3_21_3(t1, h1, t2, h2, m, []) do
    umerge2_2(t1, t2, [h2 | m], h1)
  end

  defp rumerge3_1([h1 | t1], t2, h2, m, t3, h3) when h1 <= h2 do
    rumerge3_12a(t1, h1, t2, h2, m, t3, h3)
  end

  defp rumerge3_1([h1 | t1], t2, h2, m, t3, h3) when h1 <= h3 do
    rumerge3_21_3(t1, t2, h2, m, t3, h3, h1)
  end

  defp rumerge3_1([h1 | t1], t2, h2, m, t3, h3) do
    rumerge3_1(t1, t2, h2, [h1 | m], t3, h3)
  end

  defp rumerge3_1([], t2, h2, m, t3, h3) when h2 <= h3 do
    rumerge2_2(t2, t3, m, h3, h2)
  end

  defp rumerge3_1([], t2, h2, m, t3, h3) do
    rumerge2_1(t2, t3, [h2 | m], h3)
  end

  defp rumerge3_12a(t1, h1, t2, h2, m, t3, h3) when h2 <= h3 do
    rumerge3_12_3(t1, t2, h2, m, t3, h3, h1)
  end

  defp rumerge3_12a(t1, h1, t2, h2, m, t3, h3) do
    rumerge3_2(t1, t2, h2, m, t3, h3, h1)
  end

  defp rumerge3_2(t1, [h2 | t2], h2M, m, t3, h3, h1)
       when h1 <= h2 do
    rumerge3_12b(t1, h1, t2, h2, m, t3, h3, h2M)
  end

  defp rumerge3_2(t1, [h2 | t2], h2M, m, t3, h3, h1)
       when h1 == h2M do
    rumerge3_1(t1, t2, h2, [h1 | m], t3, h3)
  end

  defp rumerge3_2(t1, [h2 | t2], h2M, m, t3, h3, h1)
       when h1 <= h3 do
    rumerge3_21_3(t1, t2, h2, [h2M | m], t3, h3, h1)
  end

  defp rumerge3_2(t1, [h2 | t2], h2M, m, t3, h3, h1) do
    rumerge3_1(t1, t2, h2, [h1, h2M | m], t3, h3)
  end

  defp rumerge3_2(t1, [], h2M, m, t3, h3, h1) when h1 == h2M do
    rumerge2_1(t1, t3, [h1 | m], h3)
  end

  defp rumerge3_2(t1, [], h2M, m, t3, h3, h1) when h1 <= h3 do
    rumerge2_2(t1, t3, [h2M | m], h3, h1)
  end

  defp rumerge3_2(t1, [], h2M, m, t3, h3, h1) do
    rumerge2_1(t1, t3, [h1, h2M | m], h3)
  end

  defp rumerge3_12b(t1, h1, t2, h2, m, t3, h3, h2M) when h2 <= h3 do
    rumerge3_12_3(t1, t2, h2, [h2M | m], t3, h3, h1)
  end

  defp rumerge3_12b(t1, h1, t2, h2, m, t3, h3, h2M) do
    rumerge3_2(t1, t2, h2, [h2M | m], t3, h3, h1)
  end

  defp rumerge3_12_3(t1, t2, h2, m, [h3 | t3], h3M, h1)
       when h2 <= h3 do
    rumerge3_12_3(t1, t2, h2, [h3M | m], t3, h3, h1)
  end

  defp rumerge3_12_3(t1, t2, h2, m, [h3 | t3], h3M, h1)
       when h2 == h3M do
    rumerge3_2(t1, t2, h2, m, t3, h3, h1)
  end

  defp rumerge3_12_3(t1, t2, h2, m, [h3 | t3], h3M, h1) do
    rumerge3_2(t1, t2, h2, [h3M | m], t3, h3, h1)
  end

  defp rumerge3_12_3(t1, t2, h2, m, [], h3M, h1) when h2 == h3M do
    rumerge2_2(t1, t2, m, h2, h1)
  end

  defp rumerge3_12_3(t1, t2, h2, m, [], h3M, h1) do
    rumerge2_2(t1, t2, [h3M | m], h2, h1)
  end

  defp rumerge3_21_3(t1, t2, h2, m, [h3 | t3], h3M, h1)
       when h1 <= h3 do
    rumerge3_21_3(t1, t2, h2, [h3M | m], t3, h3, h1)
  end

  defp rumerge3_21_3(t1, t2, h2, m, [h3 | t3], h3M, h1)
       when h1 == h3M do
    rumerge3_1(t1, t2, h2, [h1 | m], t3, h3)
  end

  defp rumerge3_21_3(t1, t2, h2, m, [h3 | t3], h3M, h1) do
    rumerge3_1(t1, t2, h2, [h1, h3M | m], t3, h3)
  end

  defp rumerge3_21_3(t1, t2, h2, m, [], h3M, h1) when h1 == h3M do
    rumerge2_1(t1, t2, [h1 | m], h2)
  end

  defp rumerge3_21_3(t1, t2, h2, m, [], h3M, h1) do
    rumerge2_1(t1, t2, [h1, h3M | m], h2)
  end

  defp umerge2_1([h1 | t1], t2, m, _HdM, h2) when h1 <= h2 do
    umerge2_1(t1, t2, [h1 | m], h1, h2)
  end

  defp umerge2_1([h1 | t1], t2, m, hdM, h2) when h2 == hdM do
    umerge2_2(t1, t2, m, h1)
  end

  defp umerge2_1([h1 | t1], t2, m, _HdM, h2) do
    umerge2_2(t1, t2, [h2 | m], h1)
  end

  defp umerge2_1([], t2, m, hdM, h2) when h2 == hdM do
    :lists.reverse(t2, m)
  end

  defp umerge2_1([], t2, m, _HdM, h2) do
    :lists.reverse(t2, [h2 | m])
  end

  defp umerge2_2(t1, [h2 | t2], m, h1) when h1 <= h2 do
    umerge2_1(t1, t2, [h1 | m], h1, h2)
  end

  defp umerge2_2(t1, [h2 | t2], m, h1) do
    umerge2_2(t1, t2, [h2 | m], h1)
  end

  defp umerge2_2(t1, [], m, h1) do
    :lists.reverse(t1, [h1 | m])
  end

  defp rumerge2_1([h1 | t1], t2, m, h2) when h1 <= h2 do
    rumerge2_2(t1, t2, m, h2, h1)
  end

  defp rumerge2_1([h1 | t1], t2, m, h2) do
    rumerge2_1(t1, t2, [h1 | m], h2)
  end

  defp rumerge2_1([], t2, m, h2) do
    :lists.reverse(t2, [h2 | m])
  end

  defp rumerge2_2(t1, [h2 | t2], m, h2M, h1) when h1 <= h2 do
    rumerge2_2(t1, t2, [h2M | m], h2, h1)
  end

  defp rumerge2_2(t1, [h2 | t2], m, h2M, h1) when h1 == h2M do
    rumerge2_1(t1, t2, [h1 | m], h2)
  end

  defp rumerge2_2(t1, [h2 | t2], m, h2M, h1) do
    rumerge2_1(t1, t2, [h1, h2M | m], h2)
  end

  defp rumerge2_2(t1, [], m, h2M, h1) when h1 == h2M do
    :lists.reverse(t1, [h1 | m])
  end

  defp rumerge2_2(t1, [], m, h2M, h1) do
    :lists.reverse(t1, [h1, h2M | m])
  end

  defp keysplit_1(i, x, eX, y, eY, [z | l], r, rs) do
    case :erlang.element(i, z) do
      eZ when eY <= eZ ->
        keysplit_1(i, y, eY, z, eZ, l, [x | r], rs)

      eZ when eX <= eZ ->
        keysplit_1(i, z, eZ, y, eY, l, [x | r], rs)

      _EZ when r == [] ->
        keysplit_1(i, x, eX, y, eY, l, [z], rs)

      eZ ->
        keysplit_1_1(i, x, eX, y, eY, eZ, r, rs, z, l)
    end
  end

  defp keysplit_1(i, x, _EX, y, _EY, [], r, rs) do
    rkeymergel(i, [[y, x | r] | rs], [], :asc)
  end

  defp keysplit_1_1(i, x, eX, y, eY, eS, r, rs, s, [z | l]) do
    case :erlang.element(i, z) do
      eZ when eY <= eZ ->
        keysplit_1_1(i, y, eY, z, eZ, eS, [x | r], rs, s, l)

      eZ when eX <= eZ ->
        keysplit_1_1(i, z, eZ, y, eY, eS, [x | r], rs, s, l)

      eZ when eS <= eZ ->
        keysplit_1(i, s, eS, z, eZ, l, [], [[y, x | r] | rs])

      eZ ->
        keysplit_1(i, z, eZ, s, eS, l, [], [[y, x | r] | rs])
    end
  end

  defp keysplit_1_1(i, x, _EX, y, _EY, _ES, r, rs, s, []) do
    rkeymergel(i, [[s], [y, x | r] | rs], [], :asc)
  end

  defp keysplit_2(i, x, eX, y, eY, [z | l], r, rs) do
    case :erlang.element(i, z) do
      eZ when eY > eZ ->
        keysplit_2(i, y, eY, z, eZ, l, [x | r], rs)

      eZ when eX > eZ ->
        keysplit_2(i, z, eZ, y, eY, l, [x | r], rs)

      _EZ when r == [] ->
        keysplit_2(i, x, eX, y, eY, l, [z], rs)

      eZ ->
        keysplit_2_1(i, x, eX, y, eY, eZ, r, rs, z, l)
    end
  end

  defp keysplit_2(i, x, _EX, y, _EY, [], r, rs) do
    keymergel(i, [[y, x | r] | rs], [], :desc)
  end

  defp keysplit_2_1(i, x, eX, y, eY, eS, r, rs, s, [z | l]) do
    case :erlang.element(i, z) do
      eZ when eY > eZ ->
        keysplit_2_1(i, y, eY, z, eZ, eS, [x | r], rs, s, l)

      eZ when eX > eZ ->
        keysplit_2_1(i, z, eZ, y, eY, eS, [x | r], rs, s, l)

      eZ when eS > eZ ->
        keysplit_2(i, s, eS, z, eZ, l, [], [[y, x | r] | rs])

      eZ ->
        keysplit_2(i, z, eZ, s, eS, l, [], [[y, x | r] | rs])
    end
  end

  defp keysplit_2_1(i, x, _EX, y, _EY, _ES, r, rs, s, []) do
    keymergel(i, [[s], [y, x | r] | rs], [], :desc)
  end

  defp keymergel(i, [t1, [h2 | t2], [h3 | t3] | l], acc, o)
       when o == :asc do
    m = keymerge3_1(i, t1, [], o, :erlang.element(i, h2), h2, t2, :erlang.element(i, h3), h3, t3)
    keymergel(i, l, [m | acc], o)
  end

  defp keymergel(i, [[h3 | t3], [h2 | t2], t1 | l], acc, o)
       when o == :desc do
    m = keymerge3_1(i, t1, [], o, :erlang.element(i, h2), h2, t2, :erlang.element(i, h3), h3, t3)
    keymergel(i, l, [m | acc], o)
  end

  defp keymergel(i, [t1, [h2 | t2] | l], acc, :asc) do
    keymergel(
      i,
      l,
      [
        keymerge2_1(i, t1, :erlang.element(i, h2), h2, t2, [])
        | acc
      ],
      :asc
    )
  end

  defp keymergel(i, [[h2 | t2], t1 | l], acc, :desc) do
    keymergel(
      i,
      l,
      [
        keymerge2_1(i, t1, :erlang.element(i, h2), h2, t2, [])
        | acc
      ],
      :desc
    )
  end

  defp keymergel(_I, [l], [], _O) do
    l
  end

  defp keymergel(i, [l], acc, o) do
    rkeymergel(i, [:lists.reverse(l, []) | acc], [], o)
  end

  defp keymergel(i, [], acc, o) do
    rkeymergel(i, acc, [], o)
  end

  defp rkeymergel(i, [[h3 | t3], [h2 | t2], t1 | l], acc, o)
       when o == :asc do
    m = rkeymerge3_1(i, t1, [], o, :erlang.element(i, h2), h2, t2, :erlang.element(i, h3), h3, t3)
    rkeymergel(i, l, [m | acc], o)
  end

  defp rkeymergel(i, [t1, [h2 | t2], [h3 | t3] | l], acc, o)
       when o == :desc do
    m = rkeymerge3_1(i, t1, [], o, :erlang.element(i, h2), h2, t2, :erlang.element(i, h3), h3, t3)
    rkeymergel(i, l, [m | acc], o)
  end

  defp rkeymergel(i, [[h2 | t2], t1 | l], acc, :asc) do
    rkeymergel(
      i,
      l,
      [
        rkeymerge2_1(i, t1, :erlang.element(i, h2), h2, t2, [])
        | acc
      ],
      :asc
    )
  end

  defp rkeymergel(i, [t1, [h2 | t2] | l], acc, :desc) do
    rkeymergel(
      i,
      l,
      [
        rkeymerge2_1(i, t1, :erlang.element(i, h2), h2, t2, [])
        | acc
      ],
      :desc
    )
  end

  defp rkeymergel(i, [l], acc, o) do
    keymergel(i, [:lists.reverse(l, []) | acc], [], o)
  end

  defp rkeymergel(i, [], acc, o) do
    keymergel(i, acc, [], o)
  end

  defp keymerge3_1(i, [h1 | t1], m, d, e2, h2, t2, e3, h3, t3) do
    case :erlang.element(i, h1) do
      e1 when e1 <= e2 ->
        keymerge3_12(i, e1, h1, t1, e2, h2, t2, e3, h3, t3, m, d)

      e1 ->
        keymerge3_21(i, e1, h1, t1, e2, h2, t2, e3, h3, t3, m, t2)
    end
  end

  defp keymerge3_1(i, [], m, _D, e2, h2, t2, e3, h3, t3)
       when e2 <= e3 do
    keymerge2_1(i, t2, e3, h3, t3, [h2 | m])
  end

  defp keymerge3_1(i, [], m, _D, e2, h2, t2, _E3, h3, t3) do
    keymerge2_2(i, t2, e2, h3, t3, m, h2)
  end

  defp keymerge3_2(i, e1, h1, t1, [h2 | t2], m, d, e3, h3, t3) do
    case :erlang.element(i, h2) do
      e2 when e1 <= e2 ->
        keymerge3_12(i, e1, h1, t1, e2, h2, t2, e3, h3, t3, m, t1)

      e2 ->
        keymerge3_21(i, e1, h1, t1, e2, h2, t2, e3, h3, t3, m, d)
    end
  end

  defp keymerge3_2(i, e1, h1, t1, [], m, _D, e3, h3, t3)
       when e1 <= e3 do
    keymerge2_1(i, t1, e3, h3, t3, [h1 | m])
  end

  defp keymerge3_2(i, e1, h1, t1, [], m, _D, _E3, h3, t3) do
    keymerge2_2(i, t1, e1, h3, t3, m, h1)
  end

  defp keymerge3_12(i, e1, h1, t1, e2, h2, t2, e3, h3, t3, m, d)
       when e1 <= e3 do
    keymerge3_1(i, t1, [h1 | m], d, e2, h2, t2, e3, h3, t3)
  end

  defp keymerge3_12(i, e1, h1, t1, e2, h2, t2, _E3, h3, t3, m, _D) do
    keymerge3_12_3(i, e1, h1, t1, e2, h2, t2, t3, [h3 | m])
  end

  defp keymerge3_12_3(i, e1, h1, t1, e2, h2, t2, [h3 | t3], m) do
    case :erlang.element(i, h3) do
      e3 when e1 <= e3 ->
        keymerge3_1(i, t1, [h1 | m], t1, e2, h2, t2, e3, h3, t3)

      _E3 ->
        keymerge3_12_3(i, e1, h1, t1, e2, h2, t2, t3, [h3 | m])
    end
  end

  defp keymerge3_12_3(i, _E1, h1, t1, e2, h2, t2, [], m) do
    keymerge2_1(i, t1, e2, h2, t2, [h1 | m])
  end

  defp keymerge3_21(i, e1, h1, t1, e2, h2, t2, e3, h3, t3, m, d)
       when e2 <= e3 do
    keymerge3_2(i, e1, h1, t1, t2, [h2 | m], d, e3, h3, t3)
  end

  defp keymerge3_21(i, e1, h1, t1, e2, h2, t2, _E3, h3, t3, m, _D) do
    keymerge3_21_3(i, e1, h1, t1, e2, h2, t2, t3, [h3 | m])
  end

  defp keymerge3_21_3(i, e1, h1, t1, e2, h2, t2, [h3 | t3], m) do
    case :erlang.element(i, h3) do
      e3 when e2 <= e3 ->
        keymerge3_2(i, e1, h1, t1, t2, [h2 | m], t2, e3, h3, t3)

      _E3 ->
        keymerge3_21_3(i, e1, h1, t1, e2, h2, t2, t3, [h3 | m])
    end
  end

  defp keymerge3_21_3(i, e1, h1, t1, _E2, h2, t2, [], m) do
    keymerge2_2(i, t1, e1, h2, t2, m, h1)
  end

  defp rkeymerge3_1(i, [h1 | t1], m, d, e2, h2, t2, e3, h3, t3) do
    case :erlang.element(i, h1) do
      e1 when e1 <= e2 ->
        rkeymerge3_12(i, e1, h1, t1, e2, h2, t2, e3, h3, t3, m, t2)

      e1 ->
        rkeymerge3_21(i, e1, h1, t1, e2, h2, t2, e3, h3, t3, m, d)
    end
  end

  defp rkeymerge3_1(i, [], m, _D, e2, h2, t2, e3, h3, t3)
       when e2 <= e3 do
    rkeymerge2_2(i, e2, t2, h3, t3, m, h2)
  end

  defp rkeymerge3_1(i, [], m, _D, _E2, h2, t2, e3, h3, t3) do
    rkeymerge2_1(i, t2, e3, h3, t3, [h2 | m])
  end

  defp rkeymerge3_2(i, e1, h1, t1, [h2 | t2], m, d, e3, h3, t3) do
    case :erlang.element(i, h2) do
      e2 when e1 <= e2 ->
        rkeymerge3_12(i, e1, h1, t1, e2, h2, t2, e3, h3, t3, m, d)

      e2 ->
        rkeymerge3_21(i, e1, h1, t1, e2, h2, t2, e3, h3, t3, m, t1)
    end
  end

  defp rkeymerge3_2(i, e1, h1, t1, [], m, _D, e3, h3, t3)
       when e1 <= e3 do
    rkeymerge2_2(i, e1, t1, h3, t3, m, h1)
  end

  defp rkeymerge3_2(i, _E1, h1, t1, [], m, _D, e3, h3, t3) do
    rkeymerge2_1(i, t1, e3, h3, t3, [h1 | m])
  end

  defp rkeymerge3_12(i, e1, h1, t1, e2, h2, t2, e3, h3, t3, m, _D)
       when e2 <= e3 do
    rkeymerge3_12_3(i, e1, h1, t1, e2, h2, t2, t3, [h3 | m])
  end

  defp rkeymerge3_12(i, e1, h1, t1, _E2, h2, t2, e3, h3, t3, m, d) do
    rkeymerge3_2(i, e1, h1, t1, t2, [h2 | m], d, e3, h3, t3)
  end

  defp rkeymerge3_12_3(i, e1, h1, t1, e2, h2, t2, [h3 | t3], m) do
    case :erlang.element(i, h3) do
      e3 when e2 <= e3 ->
        rkeymerge3_12_3(i, e1, h1, t1, e2, h2, t2, t3, [h3 | m])

      e3 ->
        rkeymerge3_2(i, e1, h1, t1, t2, [h2 | m], t2, e3, h3, t3)
    end
  end

  defp rkeymerge3_12_3(i, e1, h1, t1, _E2, h2, t2, [], m) do
    rkeymerge2_2(i, e1, t1, h2, t2, m, h1)
  end

  defp rkeymerge3_21(i, e1, h1, t1, e2, h2, t2, e3, h3, t3, m, _D)
       when e1 <= e3 do
    rkeymerge3_21_3(i, e1, h1, t1, e2, h2, t2, t3, [h3 | m])
  end

  defp rkeymerge3_21(i, _E1, h1, t1, e2, h2, t2, e3, h3, t3, m, d) do
    rkeymerge3_1(i, t1, [h1 | m], d, e2, h2, t2, e3, h3, t3)
  end

  defp rkeymerge3_21_3(i, e1, h1, t1, e2, h2, t2, [h3 | t3], m) do
    case :erlang.element(i, h3) do
      e3 when e1 <= e3 ->
        rkeymerge3_21_3(i, e1, h1, t1, e2, h2, t2, t3, [h3 | m])

      e3 ->
        rkeymerge3_1(i, t1, [h1 | m], t1, e2, h2, t2, e3, h3, t3)
    end
  end

  defp rkeymerge3_21_3(i, _E1, h1, t1, e2, h2, t2, [], m) do
    rkeymerge2_1(i, t1, e2, h2, t2, [h1 | m])
  end

  defp keymerge2_1(i, [h1 | t1], e2, h2, t2, m) do
    case :erlang.element(i, h1) do
      e1 when e1 <= e2 ->
        keymerge2_1(i, t1, e2, h2, t2, [h1 | m])

      e1 ->
        keymerge2_2(i, t1, e1, h2, t2, m, h1)
    end
  end

  defp keymerge2_1(_I, [], _E2, h2, t2, m) do
    :lists.reverse(t2, [h2 | m])
  end

  defp keymerge2_2(i, t1, e1, hdM, [h2 | t2], m, h1) do
    case :erlang.element(i, h2) do
      e2 when e1 <= e2 ->
        keymerge2_1(i, t1, e2, h2, t2, [h1, hdM | m])

      _E2 ->
        keymerge2_2(i, t1, e1, h2, t2, [hdM | m], h1)
    end
  end

  defp keymerge2_2(_I, t1, _E1, hdM, [], m, h1) do
    :lists.reverse(t1, [h1, hdM | m])
  end

  defp rkeymerge2_1(i, [h1 | t1], e2, h2, t2, m) do
    case :erlang.element(i, h1) do
      e1 when e1 <= e2 ->
        rkeymerge2_2(i, e1, t1, h2, t2, m, h1)

      _E1 ->
        rkeymerge2_1(i, t1, e2, h2, t2, [h1 | m])
    end
  end

  defp rkeymerge2_1(_I, [], _E2, h2, t2, m) do
    :lists.reverse(t2, [h2 | m])
  end

  defp rkeymerge2_2(i, e1, t1, hdM, [h2 | t2], m, h1) do
    case :erlang.element(i, h2) do
      e2 when e1 <= e2 ->
        rkeymerge2_2(i, e1, t1, h2, t2, [hdM | m], h1)

      e2 ->
        rkeymerge2_1(i, t1, e2, h2, t2, [h1, hdM | m])
    end
  end

  defp rkeymerge2_2(_I, _E1, t1, hdM, [], m, h1) do
    :lists.reverse(t1, [h1, hdM | m])
  end

  defp ukeysplit_1(i, x, eX, y, eY, [z | l], r, rs) do
    case :erlang.element(i, z) do
      eZ when eY == eZ ->
        ukeysplit_1(i, x, eX, y, eY, l, r, rs)

      eZ when eY < eZ ->
        ukeysplit_1(i, y, eY, z, eZ, l, [x | r], rs)

      eZ when eX == eZ ->
        ukeysplit_1(i, x, eX, y, eY, l, r, rs)

      eZ when eX < eZ ->
        ukeysplit_1(i, z, eZ, y, eY, l, [x | r], rs)

      _EZ when r == [] ->
        ukeysplit_1(i, x, eX, y, eY, l, [z], rs)

      eZ ->
        ukeysplit_1_1(i, x, eX, y, eY, l, r, rs, z, eZ)
    end
  end

  defp ukeysplit_1(i, x, _EX, y, _EY, [], r, rs) do
    rukeymergel(i, [[y, x | r] | rs], [])
  end

  defp ukeysplit_1_1(i, x, eX, y, eY, [z | l], r, rs, s, eS) do
    case :erlang.element(i, z) do
      eZ when eY == eZ ->
        ukeysplit_1_1(i, x, eX, y, eY, l, r, rs, s, eS)

      eZ when eY < eZ ->
        ukeysplit_1_1(i, y, eY, z, eZ, l, [x | r], rs, s, eS)

      eZ when eX == eZ ->
        ukeysplit_1_1(i, x, eX, y, eY, l, r, rs, s, eS)

      eZ when eX < eZ ->
        ukeysplit_1_1(i, z, eZ, y, eY, l, [x | r], rs, s, eS)

      eZ when eS == eZ ->
        ukeysplit_1_1(i, x, eX, y, eY, l, r, rs, s, eS)

      eZ when eS < eZ ->
        ukeysplit_1(i, s, eS, z, eZ, l, [], [[y, x | r] | rs])

      eZ ->
        ukeysplit_1(i, z, eZ, s, eS, l, [], [[y, x | r] | rs])
    end
  end

  defp ukeysplit_1_1(i, x, _EX, y, _EY, [], r, rs, s, _ES) do
    rukeymergel(i, [[s], [y, x | r] | rs], [])
  end

  defp ukeysplit_2(i, y, eY, [z | l], r) do
    case :erlang.element(i, z) do
      eZ when eY == eZ ->
        ukeysplit_2(i, y, eY, l, r)

      eZ when eY < eZ ->
        ukeysplit_1(i, y, eY, z, eZ, l, [], [:lists.reverse(r, [])])

      eZ ->
        ukeysplit_2(i, z, eZ, l, [y | r])
    end
  end

  defp ukeysplit_2(_I, y, _EY, [], r) do
    [y | r]
  end

  defp ukeymergel(i, [t1, [h2 | t2], [h3 | t3] | l], acc) do
    m =
      ukeymerge3_1(
        i,
        t1,
        acc,
        [h2 | h3],
        :erlang.element(i, h2),
        h2,
        t2,
        [],
        :erlang.element(i, h3),
        h3,
        t3
      )

    ukeymergel(i, l, [m | acc])
  end

  defp ukeymergel(i, [[h1 | t1], t2 | l], acc) do
    ukeymergel(i, l, [
      ukeymerge2_2(i, t1, :erlang.element(i, h1), h1, t2, [])
      | acc
    ])
  end

  defp ukeymergel(_I, [l], []) do
    l
  end

  defp ukeymergel(i, [l], acc) do
    rukeymergel(i, [:lists.reverse(l, []) | acc], [])
  end

  defp ukeymergel(i, [], acc) do
    rukeymergel(i, acc, [])
  end

  defp rukeymergel(i, [[h3 | t3], [h2 | t2], t1 | l], acc) do
    m =
      rukeymerge3_1(
        i,
        t1,
        acc,
        [],
        :erlang.element(i, h2),
        h2,
        t2,
        [],
        :erlang.element(i, h3),
        h3,
        t3
      )

    rukeymergel(i, l, [m | acc])
  end

  defp rukeymergel(i, [[h2 | t2], t1 | l], acc) do
    rukeymergel(i, l, [
      rukeymerge2_1(i, t1, :erlang.element(i, h2), t2, [], h2)
      | acc
    ])
  end

  defp rukeymergel(i, [l], acc) do
    ukeymergel(i, [:lists.reverse(l, []) | acc], [])
  end

  defp rukeymergel(i, [], acc) do
    ukeymergel(i, acc, [])
  end

  defp ukeymerge3_1(i, [h1 | t1], d, hdM, e2, h2, t2, m, e3, h3, t3) do
    case :erlang.element(i, h1) do
      e1 when e1 <= e2 ->
        ukeymerge3_12(i, e1, t1, h1, e2, h2, t2, e3, h3, t3, m, hdM, d)

      e1 when e2 == hdM ->
        ukeymerge3_2(i, e1, t1, h1, t2, hdM, t2, m, e3, h3, t3)

      e1 ->
        ukeymerge3_21(i, e1, t1, h1, e2, h2, t2, e3, h3, t3, m, hdM, t2)
    end
  end

  defp ukeymerge3_1(i, [], _D, hdM, e2, _H2, t2, m, e3, h3, t3)
       when e2 == hdM do
    ukeymerge2_1(i, t2, e3, hdM, t3, m, h3)
  end

  defp ukeymerge3_1(i, [], _D, _HdM, e2, h2, t2, m, e3, h3, t3)
       when e2 <= e3 do
    ukeymerge2_1(i, t2, e3, e2, t3, [h2 | m], h3)
  end

  defp ukeymerge3_1(i, [], _D, hdM, e2, h2, t2, m, e3, _H3, t3)
       when e3 == hdM do
    ukeymerge2_2(i, t2, e2, h2, t3, m)
  end

  defp ukeymerge3_1(i, [], _D, _HdM, e2, h2, t2, m, _E3, h3, t3) do
    ukeymerge2_2(i, t2, e2, h2, t3, [h3 | m])
  end

  defp ukeymerge3_2(i, e1, t1, h1, [h2 | t2], hdM, d, m, e3, h3, t3) do
    case :erlang.element(i, h2) do
      e2 when e1 <= e2 ->
        ukeymerge3_12(i, e1, t1, h1, e2, h2, t2, e3, h3, t3, m, hdM, t1)

      e2 ->
        ukeymerge3_21(i, e1, t1, h1, e2, h2, t2, e3, h3, t3, m, hdM, d)
    end
  end

  defp ukeymerge3_2(i, e1, t1, h1, [], _HdM, _D, m, e3, h3, t3)
       when e1 <= e3 do
    ukeymerge2_1(i, t1, e3, e1, t3, [h1 | m], h3)
  end

  defp ukeymerge3_2(i, e1, t1, h1, [], hdM, _D, m, e3, _H3, t3)
       when e3 == hdM do
    ukeymerge2_2(i, t1, e1, h1, t3, m)
  end

  defp ukeymerge3_2(i, e1, t1, h1, [], _HdM, _D, m, _E3, h3, t3) do
    ukeymerge2_2(i, t1, e1, h1, t3, [h3 | m])
  end

  defp ukeymerge3_12(i, e1, t1, h1, e2, h2, t2, e3, h3, t3, m, _HdM, d)
       when e1 <= e3 do
    ukeymerge3_1(i, t1, d, e1, e2, h2, t2, [h1 | m], e3, h3, t3)
  end

  defp ukeymerge3_12(i, e1, t1, h1, e2, h2, t2, e3, _H3, t3, m, hdM, _D)
       when e3 == hdM do
    ukeymerge3_12_3(i, e1, t1, h1, e2, h2, t2, m, t3)
  end

  defp ukeymerge3_12(i, e1, t1, h1, e2, h2, t2, _E3, h3, t3, m, _HdM, _D) do
    ukeymerge3_12_3(i, e1, t1, h1, e2, h2, t2, [h3 | m], t3)
  end

  defp ukeymerge3_12_3(i, e1, t1, h1, e2, h2, t2, m, [h3 | t3]) do
    case :erlang.element(i, h3) do
      e3 when e1 <= e3 ->
        ukeymerge3_1(i, t1, t1, e1, e2, h2, t2, [h1 | m], e3, h3, t3)

      _E3 ->
        ukeymerge3_12_3(i, e1, t1, h1, e2, h2, t2, [h3 | m], t3)
    end
  end

  defp ukeymerge3_12_3(i, e1, t1, h1, e2, h2, t2, m, []) do
    ukeymerge2_1(i, t1, e2, e1, t2, [h1 | m], h2)
  end

  defp ukeymerge3_21(i, e1, t1, h1, e2, h2, t2, e3, h3, t3, m, _HdM, d)
       when e2 <= e3 do
    ukeymerge3_2(i, e1, t1, h1, t2, e2, d, [h2 | m], e3, h3, t3)
  end

  defp ukeymerge3_21(i, e1, t1, h1, e2, h2, t2, e3, _H3, t3, m, hdM, _D)
       when e3 == hdM do
    ukeymerge3_21_3(i, e1, t1, h1, e2, h2, t2, m, t3)
  end

  defp ukeymerge3_21(i, e1, t1, h1, e2, h2, t2, _E3, h3, t3, m, _HdM, _D) do
    ukeymerge3_21_3(i, e1, t1, h1, e2, h2, t2, [h3 | m], t3)
  end

  defp ukeymerge3_21_3(i, e1, t1, h1, e2, h2, t2, m, [h3 | t3]) do
    case :erlang.element(i, h3) do
      e3 when e2 <= e3 ->
        ukeymerge3_2(i, e1, t1, h1, t2, e2, t2, [h2 | m], e3, h3, t3)

      _E3 ->
        ukeymerge3_21_3(i, e1, t1, h1, e2, h2, t2, [h3 | m], t3)
    end
  end

  defp ukeymerge3_21_3(i, e1, t1, h1, _E2, h2, t2, m, []) do
    ukeymerge2_2(i, t1, e1, h1, t2, [h2 | m])
  end

  defp rukeymerge3_1(i, [h1 | t1], d1, d2, e2, h2, t2, m, e3, h3, t3) do
    case :erlang.element(i, h1) do
      e1 when e1 <= e2 ->
        rukeymerge3_12a(i, e1, h1, t1, e2, h2, t2, e3, h3, t3, m)

      e1 ->
        rukeymerge3_21a(i, e1, h1, t1, e2, h2, t2, e3, h3, t3, m, d1, d2)
    end
  end

  defp rukeymerge3_1(i, [], _D1, _D2, e2, h2, t2, m, e3, h3, t3)
       when e2 <= e3 do
    rukeymerge2_2(i, t2, e2, t3, m, e3, h3, h2)
  end

  defp rukeymerge3_1(i, [], _D1, _D2, _E2, h2, t2, m, e3, h3, t3) do
    rukeymerge2_1(i, t2, e3, t3, [h2 | m], h3)
  end

  defp rukeymerge3_12a(i, e1, h1, t1, e2, h2, t2, e3, h3, t3, m)
       when e2 <= e3 do
    rukeymerge3_12_3(i, e1, h1, t1, e2, h2, t2, m, e3, h3, t3)
  end

  defp rukeymerge3_12a(i, e1, h1, t1, e2, h2, t2, e3, h3, t3, m) do
    rukeymerge3_2(i, e1, h1, t1, t2, h2, e2, m, e3, h3, t3)
  end

  defp rukeymerge3_21a(i, e1, h1, t1, e2, h2, t2, e3, h3, t3, m, _D1, _D2)
       when e1 <= e3 do
    rukeymerge3_21_3(i, e1, h1, t1, e2, h2, t2, m, e3, h3, t3)
  end

  defp rukeymerge3_21a(i, _E1, h1, t1, e2, h2, t2, e3, h3, t3, m, d1, d2) do
    rukeymerge3_1(i, t1, d1, d2, e2, h2, t2, [h1 | m], e3, h3, t3)
  end

  defp rukeymerge3_2(i, e1, h1, t1, [h2 | t2], h2M, e2M, m, e3, h3, t3) do
    case :erlang.element(i, h2) do
      e2 when e1 <= e2 ->
        rukeymerge3_12b(i, e1, h1, t1, e2, h2, t2, e3, h3, t3, m, h2M)

      e2 when e1 == e2M ->
        rukeymerge3_1(i, t1, h1, t1, e2, h2, t2, [h1 | m], e3, h3, t3)

      e2 ->
        rukeymerge3_21b(i, e1, h1, t1, e2, h2, t2, e3, h3, t3, m, h2M)
    end
  end

  defp rukeymerge3_2(i, e1, h1, t1, [], _H2M, e2M, m, e3, h3, t3)
       when e1 == e2M do
    rukeymerge2_1(i, t1, e3, t3, [h1 | m], h3)
  end

  defp rukeymerge3_2(i, e1, h1, t1, [], h2M, _E2M, m, e3, h3, t3)
       when e1 <= e3 do
    rukeymerge2_2(i, t1, e1, t3, [h2M | m], e3, h3, h1)
  end

  defp rukeymerge3_2(i, _E1, h1, t1, [], h2M, _E2M, m, e3, h3, t3) do
    rukeymerge2_1(i, t1, e3, t3, [h1, h2M | m], h3)
  end

  defp rukeymerge3_12b(i, e1, h1, t1, e2, h2, t2, e3, h3, t3, m, h2M)
       when e2 <= e3 do
    rukeymerge3_12_3(i, e1, h1, t1, e2, h2, t2, [h2M | m], e3, h3, t3)
  end

  defp rukeymerge3_12b(i, e1, h1, t1, e2, h2, t2, e3, h3, t3, m, h2M) do
    rukeymerge3_2(i, e1, h1, t1, t2, h2, e2, [h2M | m], e3, h3, t3)
  end

  defp rukeymerge3_21b(i, e1, h1, t1, e2, h2, t2, e3, h3, t3, m, h2M)
       when e1 <= e3 do
    rukeymerge3_21_3(i, e1, h1, t1, e2, h2, t2, [h2M | m], e3, h3, t3)
  end

  defp rukeymerge3_21b(i, _E1, h1, t1, e2, h2, t2, e3, h3, t3, m, h2M) do
    rukeymerge3_1(i, t1, h1, t1, e2, h2, t2, [h1, h2M | m], e3, h3, t3)
  end

  defp rukeymerge3_12_3(i, e1, h1, t1, e2, h2, t2, m, e3M, h3M, [h3 | t3]) do
    case :erlang.element(i, h3) do
      e3 when e2 <= e3 ->
        rukeymerge3_12_3(i, e1, h1, t1, e2, h2, t2, [h3M | m], e3, h3, t3)

      e3 when e2 == e3M ->
        rukeymerge3_2(i, e1, h1, t1, t2, h2, e2, m, e3, h3, t3)

      e3 ->
        rukeymerge3_2(i, e1, h1, t1, t2, h2, e2, [h3M | m], e3, h3, t3)
    end
  end

  defp rukeymerge3_12_3(i, e1, h1, t1, e2, h2, t2, m, e3M, _H3M, [])
       when e2 == e3M do
    rukeymerge2_2(i, t1, e1, t2, m, e2, h2, h1)
  end

  defp rukeymerge3_12_3(i, e1, h1, t1, e2, h2, t2, m, _E3M, h3M, []) do
    rukeymerge2_2(i, t1, e1, t2, [h3M | m], e2, h2, h1)
  end

  defp rukeymerge3_21_3(i, e1, h1, t1, e2, h2, t2, m, e3M, h3M, [h3 | t3]) do
    case :erlang.element(i, h3) do
      e3 when e1 <= e3 ->
        rukeymerge3_21_3(i, e1, h1, t1, e2, h2, t2, [h3M | m], e3, h3, t3)

      e3 when e1 == e3M ->
        rukeymerge3_1(i, t1, h1, t1, e2, h2, t2, [h1 | m], e3, h3, t3)

      e3 ->
        rukeymerge3_1(i, t1, h1, t1, e2, h2, t2, [h1, h3M | m], e3, h3, t3)
    end
  end

  defp rukeymerge3_21_3(i, e1, h1, t1, e2, h2, t2, m, e3M, _H3M, [])
       when e1 == e3M do
    rukeymerge2_1(i, t1, e2, t2, [h1 | m], h2)
  end

  defp rukeymerge3_21_3(i, _E1, h1, t1, e2, h2, t2, m, _E3M, h3M, []) do
    rukeymerge2_1(i, t1, e2, t2, [h1, h3M | m], h2)
  end

  defp ukeymerge2_1(i, [h1 | t1], e2, hdM, t2, m, h2) do
    case :erlang.element(i, h1) do
      e1 when e1 <= e2 ->
        ukeymerge2_1(i, t1, e2, e1, t2, [h1 | m], h2)

      e1 when e2 == hdM ->
        ukeymerge2_2(i, t1, e1, h1, t2, m)

      e1 ->
        ukeymerge2_2(i, t1, e1, h1, t2, [h2 | m])
    end
  end

  defp ukeymerge2_1(_I, [], e2, hdM, t2, m, _H2) when e2 == hdM do
    :lists.reverse(t2, m)
  end

  defp ukeymerge2_1(_I, [], _E2, _HdM, t2, m, h2) do
    :lists.reverse(t2, [h2 | m])
  end

  defp ukeymerge2_2(i, t1, e1, h1, [h2 | t2], m) do
    case :erlang.element(i, h2) do
      e2 when e1 <= e2 ->
        ukeymerge2_1(i, t1, e2, e1, t2, [h1 | m], h2)

      _E2 ->
        ukeymerge2_2(i, t1, e1, h1, t2, [h2 | m])
    end
  end

  defp ukeymerge2_2(_I, t1, _E1, h1, [], m) do
    :lists.reverse(t1, [h1 | m])
  end

  defp rukeymerge2_1(i, [h1 | t1], e2, t2, m, h2) do
    case :erlang.element(i, h1) do
      e1 when e1 <= e2 ->
        rukeymerge2_2(i, t1, e1, t2, m, e2, h2, h1)

      _E1 ->
        rukeymerge2_1(i, t1, e2, t2, [h1 | m], h2)
    end
  end

  defp rukeymerge2_1(_I, [], _E2, t2, m, h2) do
    :lists.reverse(t2, [h2 | m])
  end

  defp rukeymerge2_2(i, t1, e1, [h2 | t2], m, e2M, h2M, h1) do
    case :erlang.element(i, h2) do
      e2 when e1 <= e2 ->
        rukeymerge2_2(i, t1, e1, t2, [h2M | m], e2, h2, h1)

      e2 when e1 == e2M ->
        rukeymerge2_1(i, t1, e2, t2, [h1 | m], h2)

      e2 ->
        rukeymerge2_1(i, t1, e2, t2, [h1, h2M | m], h2)
    end
  end

  defp rukeymerge2_2(_I, t1, e1, [], m, e2M, _H2M, h1)
       when e1 == e2M do
    :lists.reverse(t1, [h1 | m])
  end

  defp rukeymerge2_2(_I, t1, _E1, [], m, _E2M, h2M, h1) do
    :lists.reverse(t1, [h1, h2M | m])
  end

  defp fsplit_1(y, x, fun, [z | l], r, rs) do
    case fun.(y, z) do
      true ->
        fsplit_1(z, y, fun, l, [x | r], rs)

      false ->
        case fun.(x, z) do
          true ->
            fsplit_1(y, z, fun, l, [x | r], rs)

          false when r == [] ->
            fsplit_1(y, x, fun, l, [z], rs)

          false ->
            fsplit_1_1(y, x, fun, l, r, rs, z)
        end
    end
  end

  defp fsplit_1(y, x, fun, [], r, rs) do
    rfmergel([[y, x | r] | rs], [], fun, :asc)
  end

  defp fsplit_1_1(y, x, fun, [z | l], r, rs, s) do
    case fun.(y, z) do
      true ->
        fsplit_1_1(z, y, fun, l, [x | r], rs, s)

      false ->
        case fun.(x, z) do
          true ->
            fsplit_1_1(y, z, fun, l, [x | r], rs, s)

          false ->
            case fun.(s, z) do
              true ->
                fsplit_1(z, s, fun, l, [], [[y, x | r] | rs])

              false ->
                fsplit_1(s, z, fun, l, [], [[y, x | r] | rs])
            end
        end
    end
  end

  defp fsplit_1_1(y, x, fun, [], r, rs, s) do
    rfmergel([[s], [y, x | r] | rs], [], fun, :asc)
  end

  defp fsplit_2(y, x, fun, [z | l], r, rs) do
    case fun.(y, z) do
      false ->
        fsplit_2(z, y, fun, l, [x | r], rs)

      true ->
        case fun.(x, z) do
          false ->
            fsplit_2(y, z, fun, l, [x | r], rs)

          true when r == [] ->
            fsplit_2(y, x, fun, l, [z], rs)

          true ->
            fsplit_2_1(y, x, fun, l, r, rs, z)
        end
    end
  end

  defp fsplit_2(y, x, fun, [], r, rs) do
    fmergel([[y, x | r] | rs], [], fun, :desc)
  end

  defp fsplit_2_1(y, x, fun, [z | l], r, rs, s) do
    case fun.(y, z) do
      false ->
        fsplit_2_1(z, y, fun, l, [x | r], rs, s)

      true ->
        case fun.(x, z) do
          false ->
            fsplit_2_1(y, z, fun, l, [x | r], rs, s)

          true ->
            case fun.(s, z) do
              false ->
                fsplit_2(z, s, fun, l, [], [[y, x | r] | rs])

              true ->
                fsplit_2(s, z, fun, l, [], [[y, x | r] | rs])
            end
        end
    end
  end

  defp fsplit_2_1(y, x, fun, [], r, rs, s) do
    fmergel([[s], [y, x | r] | rs], [], fun, :desc)
  end

  defp fmergel([t1, [h2 | t2] | l], acc, fun, :asc) do
    fmergel(l, [fmerge2_1(t1, h2, fun, t2, []) | acc], fun, :asc)
  end

  defp fmergel([[h2 | t2], t1 | l], acc, fun, :desc) do
    fmergel(l, [fmerge2_1(t1, h2, fun, t2, []) | acc], fun, :desc)
  end

  defp fmergel([l], [], _Fun, _O) do
    l
  end

  defp fmergel([l], acc, fun, o) do
    rfmergel([:lists.reverse(l, []) | acc], [], fun, o)
  end

  defp fmergel([], acc, fun, o) do
    rfmergel(acc, [], fun, o)
  end

  defp rfmergel([[h2 | t2], t1 | l], acc, fun, :asc) do
    rfmergel(l, [rfmerge2_1(t1, h2, fun, t2, []) | acc], fun, :asc)
  end

  defp rfmergel([t1, [h2 | t2] | l], acc, fun, :desc) do
    rfmergel(l, [rfmerge2_1(t1, h2, fun, t2, []) | acc], fun, :desc)
  end

  defp rfmergel([l], acc, fun, o) do
    fmergel([:lists.reverse(l, []) | acc], [], fun, o)
  end

  defp rfmergel([], acc, fun, o) do
    fmergel(acc, [], fun, o)
  end

  defp fmerge2_1([h1 | t1], h2, fun, t2, m) do
    case fun.(h1, h2) do
      true ->
        fmerge2_1(t1, h2, fun, t2, [h1 | m])

      false ->
        fmerge2_2(h1, t1, fun, t2, [h2 | m])
    end
  end

  defp fmerge2_1([], h2, _Fun, t2, m) do
    :lists.reverse(t2, [h2 | m])
  end

  defp fmerge2_2(h1, t1, fun, [h2 | t2], m) do
    case fun.(h1, h2) do
      true ->
        fmerge2_1(t1, h2, fun, t2, [h1 | m])

      false ->
        fmerge2_2(h1, t1, fun, t2, [h2 | m])
    end
  end

  defp fmerge2_2(h1, t1, _Fun, [], m) do
    :lists.reverse(t1, [h1 | m])
  end

  defp rfmerge2_1([h1 | t1], h2, fun, t2, m) do
    case fun.(h1, h2) do
      true ->
        rfmerge2_2(h1, t1, fun, t2, [h2 | m])

      false ->
        rfmerge2_1(t1, h2, fun, t2, [h1 | m])
    end
  end

  defp rfmerge2_1([], h2, _Fun, t2, m) do
    :lists.reverse(t2, [h2 | m])
  end

  defp rfmerge2_2(h1, t1, fun, [h2 | t2], m) do
    case fun.(h1, h2) do
      true ->
        rfmerge2_2(h1, t1, fun, t2, [h2 | m])

      false ->
        rfmerge2_1(t1, h2, fun, t2, [h1 | m])
    end
  end

  defp rfmerge2_2(h1, t1, _Fun, [], m) do
    :lists.reverse(t1, [h1 | m])
  end

  defp ufsplit_1(y, x, fun, [z | l], r, rs) do
    case fun.(y, z) do
      true ->
        case fun.(z, y) do
          true ->
            ufsplit_1(y, x, fun, l, r, rs)

          false ->
            ufsplit_1(z, y, fun, l, [x | r], rs)
        end

      false ->
        case fun.(x, z) do
          true ->
            case fun.(z, x) do
              true ->
                ufsplit_1(y, x, fun, l, r, rs)

              false ->
                ufsplit_1(y, z, fun, l, [x | r], rs)
            end

          false when r == [] ->
            ufsplit_1(y, x, fun, l, [z], rs)

          false ->
            ufsplit_1_1(y, x, fun, l, r, rs, z)
        end
    end
  end

  defp ufsplit_1(y, x, fun, [], r, rs) do
    rufmergel([[y, x | r] | rs], [], fun)
  end

  defp ufsplit_1_1(y, x, fun, [z | l], r, rs, s) do
    case fun.(y, z) do
      true ->
        case fun.(z, y) do
          true ->
            ufsplit_1_1(y, x, fun, l, r, rs, s)

          false ->
            ufsplit_1_1(z, y, fun, l, [x | r], rs, s)
        end

      false ->
        case fun.(x, z) do
          true ->
            case fun.(z, x) do
              true ->
                ufsplit_1_1(y, x, fun, l, r, rs, s)

              false ->
                ufsplit_1_1(y, z, fun, l, [x | r], rs, s)
            end

          false ->
            case fun.(s, z) do
              true ->
                case fun.(z, s) do
                  true ->
                    ufsplit_1_1(y, x, fun, l, r, rs, s)

                  false ->
                    ufsplit_1(z, s, fun, l, [], [[y, x | r] | rs])
                end

              false ->
                ufsplit_1(s, z, fun, l, [], [[y, x | r] | rs])
            end
        end
    end
  end

  defp ufsplit_1_1(y, x, fun, [], r, rs, s) do
    rufmergel([[s], [y, x | r] | rs], [], fun)
  end

  defp ufsplit_2(y, [z | l], fun, r) do
    case fun.(y, z) do
      true ->
        case fun.(z, y) do
          true ->
            ufsplit_2(y, l, fun, r)

          false ->
            ufsplit_1(z, y, fun, l, [], [:lists.reverse(r, [])])
        end

      false ->
        ufsplit_2(z, l, fun, [y | r])
    end
  end

  defp ufsplit_2(y, [], _Fun, r) do
    [y | r]
  end

  defp ufmergel([[h1 | t1], t2 | l], acc, fun) do
    ufmergel(l, [ufmerge2_2(h1, t1, fun, t2, []) | acc], fun)
  end

  defp ufmergel([l], [], _Fun) do
    l
  end

  defp ufmergel([l], acc, fun) do
    rufmergel([:lists.reverse(l, []) | acc], [], fun)
  end

  defp ufmergel([], acc, fun) do
    rufmergel(acc, [], fun)
  end

  defp rufmergel([[h2 | t2], t1 | l], acc, fun) do
    rufmergel(l, [rufmerge2_1(t1, h2, fun, t2, []) | acc], fun)
  end

  defp rufmergel([l], acc, fun) do
    ufmergel([:lists.reverse(l, []) | acc], [], fun)
  end

  defp rufmergel([], acc, fun) do
    ufmergel(acc, [], fun)
  end

  defp ufmerge2_1([h1 | t1], h2, fun, t2, m, hdM) do
    case fun.(h1, h2) do
      true ->
        ufmerge2_1(t1, h2, fun, t2, [h1 | m], h1)

      false ->
        case fun.(h2, hdM) do
          true ->
            ufmerge2_2(h1, t1, fun, t2, m)

          false ->
            ufmerge2_2(h1, t1, fun, t2, [h2 | m])
        end
    end
  end

  defp ufmerge2_1([], h2, fun, t2, m, hdM) do
    case fun.(h2, hdM) do
      true ->
        :lists.reverse(t2, m)

      false ->
        :lists.reverse(t2, [h2 | m])
    end
  end

  defp ufmerge2_2(h1, t1, fun, [h2 | t2], m) do
    case fun.(h1, h2) do
      true ->
        ufmerge2_1(t1, h2, fun, t2, [h1 | m], h1)

      false ->
        ufmerge2_2(h1, t1, fun, t2, [h2 | m])
    end
  end

  defp ufmerge2_2(h1, t1, _Fun, [], m) do
    :lists.reverse(t1, [h1 | m])
  end

  defp rufmerge2_1([h1 | t1], h2, fun, t2, m) do
    case fun.(h1, h2) do
      true ->
        rufmerge2_2(h1, t1, fun, t2, m, h2)

      false ->
        rufmerge2_1(t1, h2, fun, t2, [h1 | m])
    end
  end

  defp rufmerge2_1([], h2, _Fun, t2, m) do
    :lists.reverse(t2, [h2 | m])
  end

  defp rufmerge2_2(h1, t1, fun, [h2 | t2], m, h2M) do
    case fun.(h1, h2) do
      true ->
        rufmerge2_2(h1, t1, fun, t2, [h2M | m], h2)

      false ->
        case fun.(h2M, h1) do
          true ->
            rufmerge2_1(t1, h2, fun, t2, [h1 | m])

          false ->
            rufmerge2_1(t1, h2, fun, t2, [h1, h2M | m])
        end
    end
  end

  defp rufmerge2_2(h1, t1, fun, [], m, h2M) do
    case fun.(h2M, h1) do
      true ->
        :lists.reverse(t1, [h1 | m])

      false ->
        :lists.reverse(t1, [h1, h2M | m])
    end
  end
end
