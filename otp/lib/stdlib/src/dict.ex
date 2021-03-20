defmodule :m_dict do
  use Bitwise
  require Record

  Record.defrecord(:r_dict, :dict,
    size: 0,
    n: 16,
    maxn: 16,
    bso: div(16, 2),
    exp_size: 16 * 5,
    con_size: 16 * 3,
    empty: :undefined,
    segs: :undefined
  )

  def new() do
    empty = mk_seg(16)
    r_dict(empty: empty, segs: {empty})
  end

  def is_key(key, d) do
    slot = get_slot(d, key)
    bkt = get_bucket(d, slot)
    find_key(key, bkt)
  end

  defp find_key(k, [[k | _Val] | _]) do
    true
  end

  defp find_key(k, [_ | bkt]) do
    find_key(k, bkt)
  end

  defp find_key(_, []) do
    false
  end

  def to_list(d) do
    fold(
      fn key, val, list ->
        [{key, val} | list]
      end,
      [],
      d
    )
  end

  def from_list(l) do
    :lists.foldl(
      fn {k, v}, d ->
        store(k, v, d)
      end,
      new(),
      l
    )
  end

  def size(r_dict(size: n)) when is_integer(n) and n >= 0 do
    n
  end

  def is_empty(r_dict(size: n)) do
    n === 0
  end

  def fetch(key, d) do
    slot = get_slot(d, key)
    bkt = get_bucket(d, slot)

    try do
      fetch_val(key, bkt)
    catch
      :badarg ->
        :erlang.error(:badarg, [key, d])
    end
  end

  defp fetch_val(k, [[k | val] | _]) do
    val
  end

  defp fetch_val(k, [_ | bkt]) do
    fetch_val(k, bkt)
  end

  defp fetch_val(_, []) do
    throw(:badarg)
  end

  def find(key, d) do
    slot = get_slot(d, key)
    bkt = get_bucket(d, slot)
    find_val(key, bkt)
  end

  defp find_val(k, [[k | val] | _]) do
    {:ok, val}
  end

  defp find_val(k, [_ | bkt]) do
    find_val(k, bkt)
  end

  defp find_val(_, []) do
    :error
  end

  def fetch_keys(d) do
    fold(
      fn key, _Val, keys ->
        [key | keys]
      end,
      [],
      d
    )
  end

  def erase(key, d0) do
    slot = get_slot(d0, key)

    {d1, dc} =
      on_bucket(
        fn b0 ->
          erase_key(key, b0)
        end,
        d0,
        slot
      )

    maybe_contract(d1, dc)
  end

  defp erase_key(key, [[key | _Val] | bkt]) do
    {bkt, 1}
  end

  defp erase_key(key, [e | bkt0]) do
    {bkt1, dc} = erase_key(key, bkt0)
    {[e | bkt1], dc}
  end

  defp erase_key(_, []) do
    {[], 0}
  end

  def take(key, d0) do
    slot = get_slot(d0, key)

    case on_bucket(
           fn b0 ->
             take_key(key, b0)
           end,
           d0,
           slot
         ) do
      {d1, {value, dc}} ->
        {value, maybe_contract(d1, dc)}

      {_, :error} ->
        :error
    end
  end

  defp take_key(key, [[key | val] | bkt]) do
    {bkt, {val, 1}}
  end

  defp take_key(key, [e | bkt0]) do
    {bkt1, res} = take_key(key, bkt0)
    {[e | bkt1], res}
  end

  defp take_key(_, []) do
    {[], :error}
  end

  def store(key, val, d0) do
    slot = get_slot(d0, key)

    {d1, ic} =
      on_bucket(
        fn b0 ->
          store_bkt_val(key, val, b0)
        end,
        d0,
        slot
      )

    maybe_expand(d1, ic)
  end

  defp store_bkt_val(key, new, [[key | _Old] | bkt]) do
    {[[key | new] | bkt], 0}
  end

  defp store_bkt_val(key, new, [other | bkt0]) do
    {bkt1, ic} = store_bkt_val(key, new, bkt0)
    {[other | bkt1], ic}
  end

  defp store_bkt_val(key, new, []) do
    {[[key | new]], 1}
  end

  def append(key, val, d0) do
    slot = get_slot(d0, key)

    {d1, ic} =
      on_bucket(
        fn b0 ->
          append_bkt(key, val, b0)
        end,
        d0,
        slot
      )

    maybe_expand(d1, ic)
  end

  defp append_bkt(key, val, [[key | bag] | bkt]) do
    {[[key | bag ++ [val]] | bkt], 0}
  end

  defp append_bkt(key, val, [other | bkt0]) do
    {bkt1, ic} = append_bkt(key, val, bkt0)
    {[other | bkt1], ic}
  end

  defp append_bkt(key, val, []) do
    {[[key, val]], 1}
  end

  def append_list(key, l, d0) do
    slot = get_slot(d0, key)

    {d1, ic} =
      on_bucket(
        fn b0 ->
          app_list_bkt(key, l, b0)
        end,
        d0,
        slot
      )

    maybe_expand(d1, ic)
  end

  defp app_list_bkt(key, l, [[key | bag] | bkt]) do
    {[[key | bag ++ l] | bkt], 0}
  end

  defp app_list_bkt(key, l, [other | bkt0]) do
    {bkt1, ic} = app_list_bkt(key, l, bkt0)
    {[other | bkt1], ic}
  end

  defp app_list_bkt(key, l, []) do
    {[[key | l]], 1}
  end

  def update(key, f, d0) do
    slot = get_slot(d0, key)

    try do
      on_bucket(
        fn b0 ->
          update_bkt(key, f, b0)
        end,
        d0,
        slot
      )
    catch
      :badarg ->
        :erlang.error(:badarg, [key, f, d0])
    else
      {d1, _Uv} ->
        d1
    end
  end

  defp update_bkt(key, f, [[key | val] | bkt]) do
    upd = f.(val)
    {[[key | upd] | bkt], upd}
  end

  defp update_bkt(key, f, [other | bkt0]) do
    {bkt1, upd} = update_bkt(key, f, bkt0)
    {[other | bkt1], upd}
  end

  defp update_bkt(_Key, _F, []) do
    throw(:badarg)
  end

  def update(key, f, init, d0) do
    slot = get_slot(d0, key)

    {d1, ic} =
      on_bucket(
        fn b0 ->
          update_bkt(key, f, init, b0)
        end,
        d0,
        slot
      )

    maybe_expand(d1, ic)
  end

  defp update_bkt(key, f, _, [[key | val] | bkt]) do
    {[[key | f.(val)] | bkt], 0}
  end

  defp update_bkt(key, f, i, [other | bkt0]) do
    {bkt1, ic} = update_bkt(key, f, i, bkt0)
    {[other | bkt1], ic}
  end

  defp update_bkt(key, f, i, []) when is_function(f, 1) do
    {[[key | i]], 1}
  end

  def update_counter(key, incr, d0) when is_number(incr) do
    slot = get_slot(d0, key)

    {d1, ic} =
      on_bucket(
        fn b0 ->
          counter_bkt(key, incr, b0)
        end,
        d0,
        slot
      )

    maybe_expand(d1, ic)
  end

  defp counter_bkt(key, i, [[key | val] | bkt]) do
    {[[key | val + i] | bkt], 0}
  end

  defp counter_bkt(key, i, [other | bkt0]) do
    {bkt1, ic} = counter_bkt(key, i, bkt0)
    {[other | bkt1], ic}
  end

  defp counter_bkt(key, i, []) do
    {[[key | i]], 1}
  end

  def fold(f, acc, d) do
    fold_dict(f, acc, d)
  end

  def map(f, d) do
    map_dict(f, d)
  end

  def filter(f, d) do
    filter_dict(f, d)
  end

  def merge(f, d1, d2) when r_dict(d1, :size) < r_dict(d2, :size) do
    fold_dict(
      fn k, v1, d ->
        update(
          k,
          fn v2 ->
            f.(k, v1, v2)
          end,
          v1,
          d
        )
      end,
      d2,
      d1
    )
  end

  def merge(f, d1, d2) do
    fold_dict(
      fn k, v2, d ->
        update(
          k,
          fn v1 ->
            f.(k, v1, v2)
          end,
          v2,
          d
        )
      end,
      d1,
      d2
    )
  end

  defp get_slot(t, key) do
    h = :erlang.phash(key, r_dict(t, :maxn))

    cond do
      h > r_dict(t, :n) ->
        h - r_dict(t, :bso)

      true ->
        h
    end
  end

  defp get_bucket(t, slot) do
    get_bucket_s(r_dict(t, :segs), slot)
  end

  defp on_bucket(f, t, slot) do
    segI = div(slot - 1, 16) + 1
    bktI = rem(slot - 1, 16) + 1
    segs = r_dict(t, :segs)
    seg = :erlang.element(segI, segs)
    b0 = :erlang.element(bktI, seg)
    {b1, res} = f.(b0)
    {r_dict(t, segs: :erlang.setelement(segI, segs, :erlang.setelement(bktI, seg, b1))), res}
  end

  defp fold_dict(f, acc, r_dict(size: 0)) when is_function(f, 3) do
    acc
  end

  defp fold_dict(f, acc, d) do
    segs = r_dict(d, :segs)
    fold_segs(f, acc, segs, tuple_size(segs))
  end

  defp fold_segs(f, acc, segs, i) when i >= 1 do
    seg = :erlang.element(i, segs)
    fold_segs(f, fold_seg(f, acc, seg, tuple_size(seg)), segs, i - 1)
  end

  defp fold_segs(f, acc, _, 0) when is_function(f, 3) do
    acc
  end

  defp fold_seg(f, acc, seg, i) when i >= 1 do
    fold_seg(f, fold_bucket(f, acc, :erlang.element(i, seg)), seg, i - 1)
  end

  defp fold_seg(f, acc, _, 0) when is_function(f, 3) do
    acc
  end

  defp fold_bucket(f, acc, [[key | val] | bkt]) do
    fold_bucket(f, f.(key, val, acc), bkt)
  end

  defp fold_bucket(f, acc, []) when is_function(f, 3) do
    acc
  end

  defp map_dict(f, r_dict(size: 0) = dict) when is_function(f, 2) do
    dict
  end

  defp map_dict(f, d) do
    segs0 = :erlang.tuple_to_list(r_dict(d, :segs))
    segs1 = map_seg_list(f, segs0)
    r_dict(d, segs: :erlang.list_to_tuple(segs1))
  end

  defp map_seg_list(f, [seg | segs]) do
    bkts0 = :erlang.tuple_to_list(seg)
    bkts1 = map_bkt_list(f, bkts0)
    [:erlang.list_to_tuple(bkts1) | map_seg_list(f, segs)]
  end

  defp map_seg_list(f, []) when is_function(f, 2) do
    []
  end

  defp map_bkt_list(f, [bkt0 | bkts]) do
    [map_bucket(f, bkt0) | map_bkt_list(f, bkts)]
  end

  defp map_bkt_list(f, []) when is_function(f, 2) do
    []
  end

  defp map_bucket(f, [[key | val] | bkt]) do
    [[key | f.(key, val)] | map_bucket(f, bkt)]
  end

  defp map_bucket(f, []) when is_function(f, 2) do
    []
  end

  defp filter_dict(f, r_dict(size: 0) = dict) when is_function(f, 2) do
    dict
  end

  defp filter_dict(f, d) do
    segs0 = :erlang.tuple_to_list(r_dict(d, :segs))
    {segs1, fc} = filter_seg_list(f, segs0, [], 0)

    maybe_contract(
      r_dict(d, segs: :erlang.list_to_tuple(segs1)),
      fc
    )
  end

  defp filter_seg_list(f, [seg | segs], fss, fc0) do
    bkts0 = :erlang.tuple_to_list(seg)
    {bkts1, fc1} = filter_bkt_list(f, bkts0, [], fc0)
    filter_seg_list(f, segs, [:erlang.list_to_tuple(bkts1) | fss], fc1)
  end

  defp filter_seg_list(f, [], fss, fc) when is_function(f, 2) do
    {:lists.reverse(fss, []), fc}
  end

  defp filter_bkt_list(f, [bkt0 | bkts], fbs, fc0) do
    {bkt1, fc1} = filter_bucket(f, bkt0, [], fc0)
    filter_bkt_list(f, bkts, [bkt1 | fbs], fc1)
  end

  defp filter_bkt_list(f, [], fbs, fc) when is_function(f, 2) do
    {:lists.reverse(fbs), fc}
  end

  defp filter_bucket(f, [[key | val] = e | bkt], fb, fc) do
    case f.(key, val) do
      true ->
        filter_bucket(f, bkt, [e | fb], fc)

      false ->
        filter_bucket(f, bkt, fb, fc + 1)
    end
  end

  defp filter_bucket(f, [], fb, fc) when is_function(f, 2) do
    {:lists.reverse(fb), fc}
  end

  defp get_bucket_s(segs, slot) do
    segI = div(slot - 1, 16) + 1
    bktI = rem(slot - 1, 16) + 1
    :erlang.element(bktI, :erlang.element(segI, segs))
  end

  defp put_bucket_s(segs, slot, bkt) do
    segI = div(slot - 1, 16) + 1
    bktI = rem(slot - 1, 16) + 1
    seg = :erlang.setelement(bktI, :erlang.element(segI, segs), bkt)
    :erlang.setelement(segI, segs, seg)
  end

  defp maybe_expand(t, 0) do
    maybe_expand_aux(t, 0)
  end

  defp maybe_expand(t, 1) do
    maybe_expand_aux(t, 1)
  end

  defp maybe_expand_aux(t0, ic)
       when r_dict(t0, :size) + ic > r_dict(t0, :exp_size) do
    t = maybe_expand_segs(t0)
    n = r_dict(t, :n) + 1
    segs0 = r_dict(t, :segs)
    slot1 = n - r_dict(t, :bso)
    b = get_bucket_s(segs0, slot1)
    slot2 = n
    [b1 | b2] = rehash(b, slot1, slot2, r_dict(t, :maxn))
    segs1 = put_bucket_s(segs0, slot1, b1)
    segs2 = put_bucket_s(segs1, slot2, b2)
    r_dict(t, size: r_dict(t, :size) + ic, n: n, exp_size: n * 5, con_size: n * 3, segs: segs2)
  end

  defp maybe_expand_aux(t, ic) do
    r_dict(t, size: r_dict(t, :size) + ic)
  end

  defp maybe_expand_segs(t) when r_dict(t, :n) === r_dict(t, :maxn) do
    r_dict(t,
      maxn: 2 * r_dict(t, :maxn),
      bso: 2 * r_dict(t, :bso),
      segs: expand_segs(r_dict(t, :segs), r_dict(t, :empty))
    )
  end

  defp maybe_expand_segs(t) do
    t
  end

  defp maybe_contract(t, dc)
       when r_dict(t, :size) - dc < r_dict(t, :con_size) and
              r_dict(t, :n) > 16 do
    n = r_dict(t, :n)
    slot1 = n - r_dict(t, :bso)
    segs0 = r_dict(t, :segs)
    b1 = get_bucket_s(segs0, slot1)
    slot2 = n
    b2 = get_bucket_s(segs0, slot2)
    segs1 = put_bucket_s(segs0, slot1, b1 ++ b2)
    segs2 = put_bucket_s(segs1, slot2, [])
    n1 = n - 1

    maybe_contract_segs(
      r_dict(t,
        size: r_dict(t, :size) - dc,
        n: n1,
        exp_size: n1 * 5,
        con_size: n1 * 3,
        segs: segs2
      )
    )
  end

  defp maybe_contract(t, dc) do
    r_dict(t, size: r_dict(t, :size) - dc)
  end

  defp maybe_contract_segs(t) when r_dict(t, :n) === r_dict(t, :bso) do
    r_dict(t,
      maxn: div(r_dict(t, :maxn), 2),
      bso: div(r_dict(t, :bso), 2),
      segs: contract_segs(r_dict(t, :segs))
    )
  end

  defp maybe_contract_segs(t) do
    t
  end

  defp rehash([[key | _Bag] = keyBag | t], slot1, slot2, maxN) do
    [l1 | l2] = rehash(t, slot1, slot2, maxN)

    case :erlang.phash(key, maxN) do
      ^slot1 ->
        [[keyBag | l1] | l2]

      ^slot2 ->
        [l1, keyBag | l2]
    end
  end

  defp rehash([], _Slot1, _Slot2, _MaxN) do
    [[]]
  end

  defp mk_seg(16) do
    {[], [], [], [], [], [], [], [], [], [], [], [], [], [], [], []}
  end

  defp expand_segs({b1}, empty) do
    {b1, empty}
  end

  defp expand_segs({b1, b2}, empty) do
    {b1, b2, empty, empty}
  end

  defp expand_segs({b1, b2, b3, b4}, empty) do
    {b1, b2, b3, b4, empty, empty, empty, empty}
  end

  defp expand_segs({b1, b2, b3, b4, b5, b6, b7, b8}, empty) do
    {b1, b2, b3, b4, b5, b6, b7, b8, empty, empty, empty, empty, empty, empty, empty, empty}
  end

  defp expand_segs(
         {b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15, b16},
         empty
       ) do
    {b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15, b16, empty, empty, empty,
     empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty, empty}
  end

  defp expand_segs(segs, empty) do
    :erlang.list_to_tuple(
      :erlang.tuple_to_list(segs) ++
        :lists.duplicate(
          tuple_size(segs),
          empty
        )
    )
  end

  defp contract_segs({b1, _}) do
    {b1}
  end

  defp contract_segs({b1, b2, _, _}) do
    {b1, b2}
  end

  defp contract_segs({b1, b2, b3, b4, _, _, _, _}) do
    {b1, b2, b3, b4}
  end

  defp contract_segs({b1, b2, b3, b4, b5, b6, b7, b8, _, _, _, _, _, _, _, _}) do
    {b1, b2, b3, b4, b5, b6, b7, b8}
  end

  defp contract_segs(
         {b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15, b16, _, _, _, _, _, _,
          _, _, _, _, _, _, _, _, _, _}
       ) do
    {b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15, b16}
  end

  defp contract_segs(segs) do
    ss = div(tuple_size(segs), 2)
    :erlang.list_to_tuple(:lists.sublist(:erlang.tuple_to_list(segs), 1, ss))
  end
end
