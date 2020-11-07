defmodule :m_array do
  use Bitwise
  require Record

  Record.defrecord(:r_array, :array,
    size: :undefined,
    max: :undefined,
    default: :undefined,
    elements: :undefined
  )

  def new() do
    new([])
  end

  def new(options) do
    new_0(options, 0, false)
  end

  def new(size, options)
      when is_integer(size) and
             size >= 0 do
    new_0(options, size, true)
  end

  def new(_, _) do
    :erlang.error(:badarg)
  end

  defp new_0(options, size, fixed) when is_list(options) do
    new_1(options, size, fixed, :undefined)
  end

  defp new_0(options, size, fixed) do
    new_1([options], size, fixed, :undefined)
  end

  defp new_1([:fixed | options], size, _, default) do
    new_1(options, size, true, default)
  end

  defp new_1([{:fixed, fixed} | options], size, _, default)
       when is_boolean(fixed) do
    new_1(options, size, fixed, default)
  end

  defp new_1([{:default, default} | options], size, fixed, _) do
    new_1(options, size, fixed, default)
  end

  defp new_1([{:size, size} | options], _, _, default)
       when is_integer(size) and size >= 0 do
    new_1(options, size, true, default)
  end

  defp new_1([size | options], _, _, default)
       when is_integer(size) and size >= 0 do
    new_1(options, size, true, default)
  end

  defp new_1([], size, fixed, default) do
    new(size, fixed, default)
  end

  defp new_1(_Options, _Size, _Fixed, _Default) do
    :erlang.error(:badarg)
  end

  defp new(0, false, :undefined) do
    r_array(size: 0, max: 10, elements: 10)
  end

  defp new(size, fixed, default) do
    e = find_max(size - 1, 10)

    m =
      cond do
        fixed ->
          0

        true ->
          e
      end

    r_array(size: size, max: m, default: default, elements: e)
  end

  defp find_max(i, m) when i >= m do
    find_max(i, m * 10)
  end

  defp find_max(_I, m) do
    m
  end

  def is_array(r_array(size: size, max: max))
      when is_integer(size) and is_integer(max) do
    true
  end

  def is_array(_) do
    false
  end

  def size(r_array(size: n)) do
    n
  end

  def size(_) do
    :erlang.error(:badarg)
  end

  def default(r_array(default: d)) do
    d
  end

  def default(_) do
    :erlang.error(:badarg)
  end

  def fix(r_array() = a) do
    r_array(a, max: 0)
  end

  def is_fix(r_array(max: 0)) do
    true
  end

  def is_fix(r_array()) do
    false
  end

  def relax(r_array(size: n) = a) do
    r_array(a, max: find_max(n - 1, 10))
  end

  def resize(size, r_array(size: n, max: m, elements: e) = a)
      when is_integer(size) and size >= 0 do
    cond do
      size > n ->
        {e1, m1} =
          grow(
            size - 1,
            e,
            cond do
              m > 0 ->
                m

              true ->
                find_max(n - 1, 10)
            end
          )

        r_array(a,
          size: size,
          max:
            cond do
              m > 0 ->
                m1

              true ->
                m
            end,
          elements: e1
        )

      size < n ->
        r_array(a, size: size)

      true ->
        a
    end
  end

  def resize(_Size, _) do
    :erlang.error(:badarg)
  end

  def resize(array) do
    resize(sparse_size(array), array)
  end

  def set(i, value, r_array(size: n, max: m, default: d, elements: e) = a)
      when is_integer(i) and i >= 0 do
    cond do
      i < n ->
        r_array(a, elements: set_1(i, e, value, d))

      i < m ->
        r_array(a, size: i + 1, elements: set_1(i, e, value, d))

      m > 0 ->
        {e1, m1} = grow(i, e, m)
        r_array(a, size: i + 1, max: m1, elements: set_1(i, e1, value, d))

      true ->
        :erlang.error(:badarg)
    end
  end

  def set(_I, _V, _A) do
    :erlang.error(:badarg)
  end

  defp set_1(i, e = {_, _, _, _, _, _, _, _, _, _, s}, x, d) do
    i1 = div(i, s) + 1
    :erlang.setelement(i1, e, set_1(rem(i, s), :erlang.element(i1, e), x, d))
  end

  defp set_1(i, e, x, d) when is_integer(e) do
    expand(i, e, x, d)
  end

  defp set_1(i, e, x, _D) do
    :erlang.setelement(i + 1, e, x)
  end

  defp grow(i, e, _M) when is_integer(e) do
    m1 = find_max(i, e)
    {m1, m1}
  end

  defp grow(i, e, m) do
    grow_1(i, e, m)
  end

  defp grow_1(i, e, m) when i >= m do
    grow_1(
      i,
      :erlang.setelement(1, :erlang.make_tuple(10 + 1, m), e),
      m * 10
    )
  end

  defp grow_1(_I, e, m) do
    {e, m}
  end

  defp expand(i, s, x, d) when s > 10 do
    s1 = div(s, 10)

    :erlang.setelement(
      div(i, s1) + 1,
      :erlang.make_tuple(10 + 1, s1),
      expand(rem(i, s1), s1, x, d)
    )
  end

  defp expand(i, _S, x, d) do
    :erlang.setelement(i + 1, :erlang.make_tuple(10, d), x)
  end

  def get(i, r_array(size: n, max: m, elements: e, default: d))
      when is_integer(i) and i >= 0 do
    cond do
      i < n ->
        get_1(i, e, d)

      m > 0 ->
        d

      true ->
        :erlang.error(:badarg)
    end
  end

  def get(_I, _A) do
    :erlang.error(:badarg)
  end

  defp get_1(i, e = {_, _, _, _, _, _, _, _, _, _, s}, d) do
    get_1(rem(i, s), :erlang.element(div(i, s) + 1, e), d)
  end

  defp get_1(_I, e, d) when is_integer(e) do
    d
  end

  defp get_1(i, e, _D) do
    :erlang.element(i + 1, e)
  end

  def reset(
        i,
        r_array(size: n, max: m, default: d, elements: e) = a
      )
      when is_integer(i) and i >= 0 do
    cond do
      i < n ->
        try do
          r_array(a, elements: reset_1(i, e, d))
        catch
          :default ->
            a
        end

      m > 0 ->
        a

      true ->
        :erlang.error(:badarg)
    end
  end

  def reset(_I, _A) do
    :erlang.error(:badarg)
  end

  defp reset_1(i, e = {_, _, _, _, _, _, _, _, _, _, s}, d) do
    i1 = div(i, s) + 1
    :erlang.setelement(i1, e, reset_1(rem(i, s), :erlang.element(i1, e), d))
  end

  defp reset_1(_I, e, _D) when is_integer(e) do
    throw(:default)
  end

  defp reset_1(i, e, d) do
    indx = i + 1

    case :erlang.element(indx, e) do
      ^d ->
        throw(:default)

      _ ->
        :erlang.setelement(i + 1, e, d)
    end
  end

  def to_list(r_array(size: 0)) do
    []
  end

  def to_list(r_array(size: n, elements: e, default: d)) do
    to_list_1(e, d, n - 1)
  end

  def to_list(_) do
    :erlang.error(:badarg)
  end

  defp to_list_1(e = {_, _, _, _, _, _, _, _, _, _, s}, d, i) do
    n = div(i, s)
    to_list_3(n, d, to_list_1(:erlang.element(n + 1, e), d, rem(i, s)), e)
  end

  defp to_list_1(e, d, i) when is_integer(e) do
    push(i + 1, d, [])
  end

  defp to_list_1(e, _D, i) do
    push_tuple(i + 1, e, [])
  end

  defp to_list_2(e = {_, _, _, _, _, _, _, _, _, _, _S}, d, l) do
    to_list_3(10, d, l, e)
  end

  defp to_list_2(e, d, l) when is_integer(e) do
    push(e, d, l)
  end

  defp to_list_2(e, _D, l) do
    push_tuple(10, e, l)
  end

  defp to_list_3(0, _D, l, _E) do
    l
  end

  defp to_list_3(n, d, l, e) do
    to_list_3(n - 1, d, to_list_2(:erlang.element(n, e), d, l), e)
  end

  defp push(0, _E, l) do
    l
  end

  defp push(n, e, l) do
    push(n - 1, e, [e | l])
  end

  defp push_tuple(0, _T, l) do
    l
  end

  defp push_tuple(n, t, l) do
    push_tuple(n - 1, t, [:erlang.element(n, t) | l])
  end

  def sparse_to_list(r_array(size: 0)) do
    []
  end

  def sparse_to_list(r_array(size: n, elements: e, default: d)) do
    sparse_to_list_1(e, d, n - 1)
  end

  def sparse_to_list(_) do
    :erlang.error(:badarg)
  end

  defp sparse_to_list_1(e = {_, _, _, _, _, _, _, _, _, _, s}, d, i) do
    n = div(i, s)
    sparse_to_list_3(n, d, sparse_to_list_1(:erlang.element(n + 1, e), d, rem(i, s)), e)
  end

  defp sparse_to_list_1(e, _D, _I) when is_integer(e) do
    []
  end

  defp sparse_to_list_1(e, d, i) do
    sparse_push_tuple(i + 1, d, e, [])
  end

  defp sparse_to_list_2(e = {_, _, _, _, _, _, _, _, _, _, _S}, d, l) do
    sparse_to_list_3(10, d, l, e)
  end

  defp sparse_to_list_2(e, _D, l) when is_integer(e) do
    l
  end

  defp sparse_to_list_2(e, d, l) do
    sparse_push_tuple(10, d, e, l)
  end

  defp sparse_to_list_3(0, _D, l, _E) do
    l
  end

  defp sparse_to_list_3(n, d, l, e) do
    sparse_to_list_3(n - 1, d, sparse_to_list_2(:erlang.element(n, e), d, l), e)
  end

  defp sparse_push_tuple(0, _D, _T, l) do
    l
  end

  defp sparse_push_tuple(n, d, t, l) do
    case :erlang.element(n, t) do
      ^d ->
        sparse_push_tuple(n - 1, d, t, l)

      e ->
        sparse_push_tuple(n - 1, d, t, [e | l])
    end
  end

  def from_list(list) do
    from_list(list, :undefined)
  end

  def from_list([], default) do
    new({:default, default})
  end

  def from_list(list, default) when is_list(list) do
    {e, n, m} = from_list_1(10, list, default, 0, [], [])
    r_array(size: n, max: m, default: default, elements: e)
  end

  def from_list(_, _) do
    :erlang.error(:badarg)
  end

  defp from_list_1(0, xs, d, n, as, es) do
    e = :erlang.list_to_tuple(:lists.reverse(as))

    case xs do
      [] ->
        case es do
          [] ->
            {e, n, 10}

          _ ->
            from_list_2_0(n, [e | es], 10)
        end

      [_ | _] ->
        from_list_1(10, xs, d, n, [], [e | es])

      _ ->
        :erlang.error(:badarg)
    end
  end

  defp from_list_1(i, xs, d, n, as, es) do
    case xs do
      [x | xs1] ->
        from_list_1(i - 1, xs1, d, n + 1, [x | as], es)

      _ ->
        from_list_1(i - 1, xs, d, n, [d | as], es)
    end
  end

  defp from_list_2_0(n, es, s) do
    from_list_2(10, pad(div(n - 1, s) + 1, 10, s, es), s, n, [s], [])
  end

  defp from_list_2(0, xs, s, n, as, es) do
    e = :erlang.list_to_tuple(as)

    case xs do
      [] ->
        case es do
          [] ->
            {e, n, s * 10}

          _ ->
            from_list_2_0(n, :lists.reverse([e | es]), s * 10)
        end

      _ ->
        from_list_2(10, xs, s, n, [s], [e | es])
    end
  end

  defp from_list_2(i, [x | xs], s, n, as, es) do
    from_list_2(i - 1, xs, s, n, [x | as], es)
  end

  defp pad(n, k, p, es) do
    push(rem(k - rem(n, k), k), p, es)
  end

  def to_orddict(r_array(size: 0)) do
    []
  end

  def to_orddict(r_array(size: n, elements: e, default: d)) do
    i = n - 1
    to_orddict_1(e, i, d, i)
  end

  def to_orddict(_) do
    :erlang.error(:badarg)
  end

  defp to_orddict_1(e = {_, _, _, _, _, _, _, _, _, _, s}, r, d, i) do
    n = div(i, s)
    i1 = rem(i, s)
    to_orddict_3(n, r - i1 - 1, d, to_orddict_1(:erlang.element(n + 1, e), r, d, i1), e, s)
  end

  defp to_orddict_1(e, r, d, i) when is_integer(e) do
    push_pairs(i + 1, r, d, [])
  end

  defp to_orddict_1(e, r, _D, i) do
    push_tuple_pairs(i + 1, r, e, [])
  end

  defp to_orddict_2(e = {_, _, _, _, _, _, _, _, _, _, s}, r, d, l) do
    to_orddict_3(10, r, d, l, e, s)
  end

  defp to_orddict_2(e, r, d, l) when is_integer(e) do
    push_pairs(e, r, d, l)
  end

  defp to_orddict_2(e, r, _D, l) do
    push_tuple_pairs(10, r, e, l)
  end

  defp to_orddict_3(0, _R, _D, l, _E, _S) do
    l
  end

  defp to_orddict_3(n, r, d, l, e, s) do
    to_orddict_3(n - 1, r - s, d, to_orddict_2(:erlang.element(n, e), r, d, l), e, s)
  end

  defp push_pairs(0, _I, _E, l) do
    l
  end

  defp push_pairs(n, i, e, l) do
    push_pairs(n - 1, i - 1, e, [{i, e} | l])
  end

  defp push_tuple_pairs(0, _I, _T, l) do
    l
  end

  defp push_tuple_pairs(n, i, t, l) do
    push_tuple_pairs(n - 1, i - 1, t, [{i, :erlang.element(n, t)} | l])
  end

  def sparse_to_orddict(r_array(size: 0)) do
    []
  end

  def sparse_to_orddict(r_array(size: n, elements: e, default: d)) do
    i = n - 1
    sparse_to_orddict_1(e, i, d, i)
  end

  def sparse_to_orddict(_) do
    :erlang.error(:badarg)
  end

  defp sparse_to_orddict_1(e = {_, _, _, _, _, _, _, _, _, _, s}, r, d, i) do
    n = div(i, s)
    i1 = rem(i, s)

    sparse_to_orddict_3(
      n,
      r - i1 - 1,
      d,
      sparse_to_orddict_1(:erlang.element(n + 1, e), r, d, i1),
      e,
      s
    )
  end

  defp sparse_to_orddict_1(e, _R, _D, _I) when is_integer(e) do
    []
  end

  defp sparse_to_orddict_1(e, r, d, i) do
    sparse_push_tuple_pairs(i + 1, r, d, e, [])
  end

  defp sparse_to_orddict_2(e = {_, _, _, _, _, _, _, _, _, _, s}, r, d, l) do
    sparse_to_orddict_3(10, r, d, l, e, s)
  end

  defp sparse_to_orddict_2(e, _R, _D, l) when is_integer(e) do
    l
  end

  defp sparse_to_orddict_2(e, r, d, l) do
    sparse_push_tuple_pairs(10, r, d, e, l)
  end

  defp sparse_to_orddict_3(0, _R, _D, l, _E, _S) do
    l
  end

  defp sparse_to_orddict_3(n, r, d, l, e, s) do
    sparse_to_orddict_3(
      n - 1,
      r - s,
      d,
      sparse_to_orddict_2(:erlang.element(n, e), r, d, l),
      e,
      s
    )
  end

  defp sparse_push_tuple_pairs(0, _I, _D, _T, l) do
    l
  end

  defp sparse_push_tuple_pairs(n, i, d, t, l) do
    case :erlang.element(n, t) do
      ^d ->
        sparse_push_tuple_pairs(n - 1, i - 1, d, t, l)

      e ->
        sparse_push_tuple_pairs(n - 1, i - 1, d, t, [{i, e} | l])
    end
  end

  def from_orddict(orddict) do
    from_orddict(orddict, :undefined)
  end

  def from_orddict([], default) do
    new({:default, default})
  end

  def from_orddict(list, default) when is_list(list) do
    {e, n, m} = from_orddict_0(list, 0, 10, default, [])
    r_array(size: n, max: m, default: default, elements: e)
  end

  def from_orddict(_, _) do
    :erlang.error(:badarg)
  end

  defp from_orddict_0([], n, _Max, _D, es) do
    case es do
      [e] ->
        {e, n, 10}

      _ ->
        collect_leafs(n, es, 10)
    end
  end

  defp from_orddict_0(xs = [{ix1, _} | _], ix, max0, d, es0)
       when ix1 > max0 and is_integer(ix1) do
    hole = ix1 - ix
    step = hole - rem(hole, 10)
    next = ix + step
    from_orddict_0(xs, next, next + 10, d, [step | es0])
  end

  defp from_orddict_0(xs0 = [{_, _} | _], ix0, max, d, es) do
    {xs, e, ix} = from_orddict_1(ix0, max, xs0, ix0, d, [])
    from_orddict_0(xs, ix, ix + 10, d, [e | es])
  end

  defp from_orddict_0(xs, _, _, _, _) do
    :erlang.error({:badarg, xs})
  end

  defp from_orddict_1(ix, ix, xs, n, _D, as) do
    e = :erlang.list_to_tuple(:lists.reverse(as))
    {xs, e, n}
  end

  defp from_orddict_1(ix, max, xs, n0, d, as) do
    case xs do
      [{^ix, val} | xs1] ->
        n = ix + 1
        from_orddict_1(n, max, xs1, n, d, [val | as])

      [{ix1, _} | _] when is_integer(ix1) and ix1 > ix ->
        n = ix + 1
        from_orddict_1(n, max, xs, n, d, [d | as])

      [_ | _] ->
        :erlang.error({:badarg, xs})

      _ ->
        from_orddict_1(ix + 1, max, xs, n0, d, [d | as])
    end
  end

  defp collect_leafs(n, es, s) do
    i = div(n - 1, s) + 1
    pad = rem(10 - rem(i, 10), 10) * s

    case pad do
      0 ->
        collect_leafs(10, es, s, n, [s], [])

      _ ->
        collect_leafs(10, [pad | es], s, n, [s], [])
    end
  end

  defp collect_leafs(0, xs, s, n, as, es) do
    e = :erlang.list_to_tuple(as)

    case xs do
      [] ->
        case es do
          [] ->
            {e, n, s * 10}

          _ ->
            collect_leafs(n, :lists.reverse([e | es]), s * 10)
        end

      _ ->
        collect_leafs(10, xs, s, n, [s], [e | es])
    end
  end

  defp collect_leafs(i, [x | xs], s, n, as0, es0)
       when is_integer(x) do
    step0 = div(x, s)

    cond do
      step0 < i ->
        as = push(step0, s, as0)
        collect_leafs(i - step0, xs, s, n, as, es0)

      i === 10 ->
        step = rem(step0, 10)
        as = push(step, s, as0)
        collect_leafs(i - step, xs, s, n, as, [x | es0])

      i === step0 ->
        as = push(i, s, as0)
        collect_leafs(0, xs, s, n, as, es0)

      true ->
        as = push(i, s, as0)
        step = step0 - i
        collect_leafs(0, [step * s | xs], s, n, as, es0)
    end
  end

  defp collect_leafs(i, [x | xs], s, n, as, es) do
    collect_leafs(i - 1, xs, s, n, [x | as], es)
  end

  defp collect_leafs(10, [], s, n, [_], es) do
    collect_leafs(n, :lists.reverse(es), s * 10)
  end

  def map(
        function,
        array = r_array(size: n, elements: e, default: d)
      )
      when is_function(function, 2) do
    cond do
      n > 0 ->
        a = r_array(array, elements: [])
        r_array(a, elements: map_1(n - 1, e, 0, function, d))

      true ->
        array
    end
  end

  def map(_, _) do
    :erlang.error(:badarg)
  end

  defp map_1(n, e = {_, _, _, _, _, _, _, _, _, _, s}, ix, f, d) do
    :erlang.list_to_tuple(
      :lists.reverse([s | map_2(1, e, ix, f, d, [], div(n, s) + 1, rem(n, s), s)])
    )
  end

  defp map_1(n, e, ix, f, d) when is_integer(e) do
    map_1(n, unfold(e, d), ix, f, d)
  end

  defp map_1(n, e, ix, f, d) do
    :erlang.list_to_tuple(:lists.reverse(map_3(1, e, ix, f, d, n + 1, [])))
  end

  defp map_2(i, e, ix, f, d, l, i, r, _S) do
    map_2_1(i + 1, e, [map_1(r, :erlang.element(i, e), ix, f, d) | l])
  end

  defp map_2(i, e, ix, f, d, l, n, r, s) do
    map_2(i + 1, e, ix + s, f, d, [map_1(s - 1, :erlang.element(i, e), ix, f, d) | l], n, r, s)
  end

  defp map_2_1(i, e, l) when i <= 10 do
    map_2_1(i + 1, e, [:erlang.element(i, e) | l])
  end

  defp map_2_1(_I, _E, l) do
    l
  end

  defp map_3(i, e, ix, f, d, n, l) when i <= n do
    map_3(i + 1, e, ix + 1, f, d, n, [f.(ix, :erlang.element(i, e)) | l])
  end

  defp map_3(i, e, ix, f, d, n, l) when i <= 10 do
    map_3(i + 1, e, ix + 1, f, d, n, [d | l])
  end

  defp map_3(_I, _E, _Ix, _F, _D, _N, l) do
    l
  end

  defp unfold(s, _D) when s > 10 do
    :erlang.make_tuple(10 + 1, div(s, 10))
  end

  defp unfold(_S, d) do
    :erlang.make_tuple(10, d)
  end

  def sparse_map(
        function,
        array = r_array(size: n, elements: e, default: d)
      )
      when is_function(function, 2) do
    cond do
      n > 0 ->
        a = r_array(array, elements: [])
        r_array(a, elements: sparse_map_1(n - 1, e, 0, function, d))

      true ->
        array
    end
  end

  def sparse_map(_, _) do
    :erlang.error(:badarg)
  end

  defp sparse_map_1(n, e = {_, _, _, _, _, _, _, _, _, _, s}, ix, f, d) do
    :erlang.list_to_tuple(
      :lists.reverse([
        s
        | sparse_map_2(1, e, ix, f, d, [], div(n, s) + 1, rem(n, s), s)
      ])
    )
  end

  defp sparse_map_1(_N, e, _Ix, _F, _D) when is_integer(e) do
    e
  end

  defp sparse_map_1(_N, e, ix, f, d) do
    :erlang.list_to_tuple(:lists.reverse(sparse_map_3(1, e, ix, f, d, [])))
  end

  defp sparse_map_2(i, e, ix, f, d, l, i, r, _S) do
    sparse_map_2_1(i + 1, e, [sparse_map_1(r, :erlang.element(i, e), ix, f, d) | l])
  end

  defp sparse_map_2(i, e, ix, f, d, l, n, r, s) do
    sparse_map_2(
      i + 1,
      e,
      ix + s,
      f,
      d,
      [
        sparse_map_1(s - 1, :erlang.element(i, e), ix, f, d)
        | l
      ],
      n,
      r,
      s
    )
  end

  defp sparse_map_2_1(i, e, l) when i <= 10 do
    sparse_map_2_1(i + 1, e, [:erlang.element(i, e) | l])
  end

  defp sparse_map_2_1(_I, _E, l) do
    l
  end

  defp sparse_map_3(i, t, ix, f, d, l) when i <= 10 do
    case :erlang.element(i, t) do
      ^d ->
        sparse_map_3(i + 1, t, ix + 1, f, d, [d | l])

      e ->
        sparse_map_3(i + 1, t, ix + 1, f, d, [f.(ix, e) | l])
    end
  end

  defp sparse_map_3(_I, _E, _Ix, _F, _D, l) do
    l
  end

  def foldl(function, a, r_array(size: n, elements: e, default: d))
      when is_function(function, 3) do
    cond do
      n > 0 ->
        foldl_1(n - 1, e, a, 0, function, d)

      true ->
        a
    end
  end

  def foldl(_, _, _) do
    :erlang.error(:badarg)
  end

  defp foldl_1(n, e = {_, _, _, _, _, _, _, _, _, _, s}, a, ix, f, d) do
    foldl_2(1, e, a, ix, f, d, div(n, s) + 1, rem(n, s), s)
  end

  defp foldl_1(n, e, a, ix, f, d) when is_integer(e) do
    foldl_1(n, unfold(e, d), a, ix, f, d)
  end

  defp foldl_1(n, e, a, ix, f, _D) do
    foldl_3(1, e, a, ix, f, n + 1)
  end

  defp foldl_2(i, e, a, ix, f, d, i, r, _S) do
    foldl_1(r, :erlang.element(i, e), a, ix, f, d)
  end

  defp foldl_2(i, e, a, ix, f, d, n, r, s) do
    foldl_2(i + 1, e, foldl_1(s - 1, :erlang.element(i, e), a, ix, f, d), ix + s, f, d, n, r, s)
  end

  defp foldl_3(i, e, a, ix, f, n) when i <= n do
    foldl_3(i + 1, e, f.(ix, :erlang.element(i, e), a), ix + 1, f, n)
  end

  defp foldl_3(_I, _E, a, _Ix, _F, _N) do
    a
  end

  def sparse_foldl(function, a, r_array(size: n, elements: e, default: d))
      when is_function(function, 3) do
    cond do
      n > 0 ->
        sparse_foldl_1(n - 1, e, a, 0, function, d)

      true ->
        a
    end
  end

  def sparse_foldl(_, _, _) do
    :erlang.error(:badarg)
  end

  defp sparse_foldl_1(n, e = {_, _, _, _, _, _, _, _, _, _, s}, a, ix, f, d) do
    sparse_foldl_2(1, e, a, ix, f, d, div(n, s) + 1, rem(n, s), s)
  end

  defp sparse_foldl_1(_N, e, a, _Ix, _F, _D) when is_integer(e) do
    a
  end

  defp sparse_foldl_1(n, e, a, ix, f, d) do
    sparse_foldl_3(1, e, a, ix, f, d, n + 1)
  end

  defp sparse_foldl_2(i, e, a, ix, f, d, i, r, _S) do
    sparse_foldl_1(r, :erlang.element(i, e), a, ix, f, d)
  end

  defp sparse_foldl_2(i, e, a, ix, f, d, n, r, s) do
    sparse_foldl_2(
      i + 1,
      e,
      sparse_foldl_1(s - 1, :erlang.element(i, e), a, ix, f, d),
      ix + s,
      f,
      d,
      n,
      r,
      s
    )
  end

  defp sparse_foldl_3(i, t, a, ix, f, d, n) when i <= n do
    case :erlang.element(i, t) do
      ^d ->
        sparse_foldl_3(i + 1, t, a, ix + 1, f, d, n)

      e ->
        sparse_foldl_3(i + 1, t, f.(ix, e, a), ix + 1, f, d, n)
    end
  end

  defp sparse_foldl_3(_I, _T, a, _Ix, _F, _D, _N) do
    a
  end

  def foldr(function, a, r_array(size: n, elements: e, default: d))
      when is_function(function, 3) do
    cond do
      n > 0 ->
        i = n - 1
        foldr_1(i, e, i, a, function, d)

      true ->
        a
    end
  end

  def foldr(_, _, _) do
    :erlang.error(:badarg)
  end

  defp foldr_1(i, e = {_, _, _, _, _, _, _, _, _, _, s}, ix, a, f, d) do
    foldr_2(div(i, s) + 1, e, ix, a, f, d, rem(i, s), s - 1)
  end

  defp foldr_1(i, e, ix, a, f, d) when is_integer(e) do
    foldr_1(i, unfold(e, d), ix, a, f, d)
  end

  defp foldr_1(i, e, ix, a, f, _D) do
    i1 = i + 1
    foldr_3(i1, e, ix - i1, a, f)
  end

  defp foldr_2(0, _E, _Ix, a, _F, _D, _R, _R0) do
    a
  end

  defp foldr_2(i, e, ix, a, f, d, r, r0) do
    foldr_2(i - 1, e, ix - r - 1, foldr_1(r, :erlang.element(i, e), ix, a, f, d), f, d, r0, r0)
  end

  defp foldr_3(0, _E, _Ix, a, _F) do
    a
  end

  defp foldr_3(i, e, ix, a, f) do
    foldr_3(i - 1, e, ix, f.(ix + i, :erlang.element(i, e), a), f)
  end

  def sparse_foldr(function, a, r_array(size: n, elements: e, default: d))
      when is_function(function, 3) do
    cond do
      n > 0 ->
        i = n - 1
        sparse_foldr_1(i, e, i, a, function, d)

      true ->
        a
    end
  end

  def sparse_foldr(_, _, _) do
    :erlang.error(:badarg)
  end

  defp sparse_foldr_1(i, e = {_, _, _, _, _, _, _, _, _, _, s}, ix, a, f, d) do
    sparse_foldr_2(div(i, s) + 1, e, ix, a, f, d, rem(i, s), s - 1)
  end

  defp sparse_foldr_1(_I, e, _Ix, a, _F, _D) when is_integer(e) do
    a
  end

  defp sparse_foldr_1(i, e, ix, a, f, d) do
    i1 = i + 1
    sparse_foldr_3(i1, e, ix - i1, a, f, d)
  end

  defp sparse_foldr_2(0, _E, _Ix, a, _F, _D, _R, _R0) do
    a
  end

  defp sparse_foldr_2(i, e, ix, a, f, d, r, r0) do
    sparse_foldr_2(
      i - 1,
      e,
      ix - r - 1,
      sparse_foldr_1(r, :erlang.element(i, e), ix, a, f, d),
      f,
      d,
      r0,
      r0
    )
  end

  defp sparse_foldr_3(0, _T, _Ix, a, _F, _D) do
    a
  end

  defp sparse_foldr_3(i, t, ix, a, f, d) do
    case :erlang.element(i, t) do
      ^d ->
        sparse_foldr_3(i - 1, t, ix, a, f, d)

      e ->
        sparse_foldr_3(i - 1, t, ix, f.(ix + i, e, a), f, d)
    end
  end

  def sparse_size(a) do
    f = fn i, _V, _A ->
      throw({:value, i})
    end

    try do
      sparse_foldr(f, [], a)
    catch
      {:value, i} ->
        i + 1
    else
      [] ->
        0
    end
  end
end
