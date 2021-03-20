defmodule :m_beam_types do
  use Bitwise
  import :lists, only: [foldl: 3, reverse: 1]
  require Record
  Record.defrecord(:r_t_atom, :t_atom, elements: :any)
  Record.defrecord(:r_t_bitstring, :t_bitstring, size_unit: 1)
  Record.defrecord(:r_t_bs_context, :t_bs_context, tail_unit: 1, slots: 0, valid: 0)
  Record.defrecord(:r_t_bs_matchable, :t_bs_matchable, tail_unit: 1)
  Record.defrecord(:r_t_float, :t_float, elements: :any)
  Record.defrecord(:r_t_fun, :t_fun, arity: :any, type: :any)
  Record.defrecord(:r_t_integer, :t_integer, elements: :any)

  Record.defrecord(:r_t_map, :t_map,
    super_key: :any,
    super_value: :any
  )

  Record.defrecord(:r_t_cons, :t_cons,
    type: :any,
    terminator: :any
  )

  Record.defrecord(:r_t_list, :t_list,
    type: :any,
    terminator: :any
  )

  Record.defrecord(:r_t_tuple, :t_tuple, size: 0, exact: false, elements: %{})

  Record.defrecord(:r_t_union, :t_union,
    atom: :none,
    list: :none,
    number: :none,
    tuple_set: :none,
    other: :none
  )

  def meet([t1, t2 | ts]) do
    meet([meet(t1, t2) | ts])
  end

  def meet([t]) do
    t
  end

  def meet(t, t) do
    verified_type(t)
  end

  def meet(:any, t) do
    verified_type(t)
  end

  def meet(t, :any) do
    verified_type(t)
  end

  def meet(r_t_union() = a, b) do
    meet_unions(a, b)
  end

  def meet(a, r_t_union() = b) do
    meet_unions(b, a)
  end

  def meet(a, b) do
    glb(a, b)
  end

  defp meet_unions(
         r_t_union(atom: atomA, list: listA, number: numberA, tuple_set: tSetA, other: otherA),
         r_t_union(atom: atomB, list: listB, number: numberB, tuple_set: tSetB, other: otherB)
       ) do
    union =
      r_t_union(
        atom: glb(atomA, atomB),
        list: glb(listA, listB),
        number: glb(numberA, numberB),
        tuple_set: meet_tuple_sets(tSetA, tSetB),
        other: glb(otherA, otherB)
      )

    shrink_union(union)
  end

  defp meet_unions(r_t_union(atom: atomA), r_t_atom() = b) do
    case glb(atomA, b) do
      :none ->
        :none

      atom ->
        atom
    end
  end

  defp meet_unions(r_t_union(number: numberA), b)
       when b === :number or elem(b, 0) === :t_float or elem(b, 0) === :t_integer do
    case glb(numberA, b) do
      :none ->
        :none

      number ->
        number
    end
  end

  defp meet_unions(r_t_union(list: listA), b)
       when elem(b, 0) === :t_list or elem(b, 0) === :t_cons or b === nil do
    case glb(listA, b) do
      :none ->
        :none

      list ->
        list
    end
  end

  defp meet_unions(r_t_union(tuple_set: tuples), r_t_tuple() = b) do
    set = meet_tuple_sets(tuples, new_tuple_set(b))
    shrink_union(r_t_union(tuple_set: set))
  end

  defp meet_unions(r_t_union(other: otherA), otherB) do
    case glb(otherA, otherB) do
      :none ->
        :none

      other ->
        other
    end
  end

  defp meet_tuple_sets(:none, _) do
    :none
  end

  defp meet_tuple_sets(_, :none) do
    :none
  end

  defp meet_tuple_sets(r_t_tuple() = a, r_t_tuple() = b) do
    new_tuple_set(glb(a, b))
  end

  defp meet_tuple_sets(r_t_tuple() = tuple, records) do
    mts_tuple(records, tuple, [])
  end

  defp meet_tuple_sets(records, r_t_tuple() = tuple) do
    meet_tuple_sets(tuple, records)
  end

  defp meet_tuple_sets(recordsA, recordsB) do
    mts_records(recordsA, recordsB)
  end

  defp mts_tuple([{key, type} | records], tuple, acc) do
    case glb(type, tuple) do
      :none ->
        mts_tuple(records, tuple, acc)

      t ->
        mts_tuple(records, tuple, [{key, t} | acc])
    end
  end

  defp mts_tuple([], _Tuple, [_ | _] = acc) do
    reverse(acc)
  end

  defp mts_tuple([], _Tuple, []) do
    :none
  end

  defp mts_records(recordsA, recordsB) do
    mts_records(recordsA, recordsB, [])
  end

  defp mts_records([{key, a} | rsA], [{key, b} | rsB], acc) do
    case glb(a, b) do
      :none ->
        mts_records(rsA, rsB, acc)

      t ->
        mts_records(rsA, rsB, [{key, t} | acc])
    end
  end

  defp mts_records([{keyA, _} | _] = rsA, [{keyB, _} | rsB], acc)
       when keyA > keyB do
    mts_records(rsA, rsB, acc)
  end

  defp mts_records([{keyA, _} | rsA], [{keyB, _} | _] = rsB, acc)
       when keyA < keyB do
    mts_records(rsA, rsB, acc)
  end

  defp mts_records(_RsA, [], [_ | _] = acc) do
    reverse(acc)
  end

  defp mts_records([], _RsB, [_ | _] = acc) do
    reverse(acc)
  end

  defp mts_records(_RsA, _RsB, []) do
    :none
  end

  def join([t1, t2 | ts]) do
    join([join(t1, t2) | ts])
  end

  def join([t]) do
    t
  end

  def join(t, t) do
    t
  end

  def join(_T, :any) do
    :any
  end

  def join(:any, _T) do
    :any
  end

  def join(t, :none) do
    t
  end

  def join(:none, t) do
    t
  end

  def join(r_t_union() = a, b) do
    join_unions(a, b)
  end

  def join(a, r_t_union() = b) do
    join_unions(b, a)
  end

  def join(r_t_atom() = a, r_t_atom() = b) do
    lub(a, b)
  end

  def join(r_t_atom() = a, b)
      when elem(b, 0) === :t_list or elem(b, 0) === :t_cons or b === nil do
    r_t_union(atom: a, list: b)
  end

  def join(r_t_atom() = a, b)
      when b === :number or elem(b, 0) === :t_float or elem(b, 0) === :t_integer do
    r_t_union(atom: a, number: b)
  end

  def join(r_t_atom() = a, r_t_tuple() = b) do
    r_t_union(atom: a, tuple_set: new_tuple_set(b))
  end

  def join(r_t_atom() = a, b) do
    r_t_union(atom: a, other: b)
  end

  def join(a, r_t_atom() = b) do
    join(b, a)
  end

  def join(a, b)
      when elem(a, 0) === :t_list or elem(a, 0) === :t_cons or
             (a === nil and
                elem(b, 0) === :t_list) or elem(b, 0) === :t_cons or b === nil do
    lub(a, b)
  end

  def join(a, b)
      when elem(a, 0) === :t_list or elem(a, 0) === :t_cons or
             (a === nil and
                b === :number) or elem(b, 0) === :t_float or elem(b, 0) === :t_integer do
    r_t_union(list: a, number: b)
  end

  def join(a, r_t_tuple() = b)
      when elem(a, 0) === :t_list or elem(a, 0) === :t_cons or a === nil do
    r_t_union(list: a, tuple_set: new_tuple_set(b))
  end

  def join(a, b)
      when elem(a, 0) === :t_list or elem(a, 0) === :t_cons or a === nil do
    r_t_union(list: a, other: b)
  end

  def join(a, b)
      when elem(b, 0) === :t_list or elem(b, 0) === :t_cons or b === nil do
    join(b, a)
  end

  def join(a, b)
      when a === :number or elem(a, 0) === :t_float or
             (elem(a, 0) === :t_integer and
                b === :number) or elem(b, 0) === :t_float or elem(b, 0) === :t_integer do
    lub(a, b)
  end

  def join(a, r_t_tuple() = b)
      when a === :number or elem(a, 0) === :t_float or elem(a, 0) === :t_integer do
    r_t_union(number: a, tuple_set: new_tuple_set(b))
  end

  def join(a, b)
      when a === :number or elem(a, 0) === :t_float or elem(a, 0) === :t_integer do
    r_t_union(number: a, other: b)
  end

  def join(a, b)
      when b === :number or elem(b, 0) === :t_float or elem(b, 0) === :t_integer do
    join(b, a)
  end

  def join(r_t_tuple() = a, r_t_tuple() = b) do
    case {record_key(a), record_key(b)} do
      {same, same} ->
        lub(a, b)

      {:none, _Key} ->
        lub(a, b)

      {_Key, :none} ->
        lub(a, b)

      {keyA, keyB} when keyA < keyB ->
        r_t_union(tuple_set: [{keyA, a}, {keyB, b}])

      {keyA, keyB} when keyA > keyB ->
        r_t_union(tuple_set: [{keyB, b}, {keyA, a}])
    end
  end

  def join(r_t_tuple() = a, b) do
    r_t_union(tuple_set: new_tuple_set(a), other: b)
  end

  def join(a, r_t_tuple() = b) do
    join(b, a)
  end

  def join(a, b) do
    lub(a, b)
  end

  defp join_unions(
         r_t_union(atom: atomA, list: listA, number: numberA, tuple_set: tSetA, other: otherA),
         r_t_union(atom: atomB, list: listB, number: numberB, tuple_set: tSetB, other: otherB)
       ) do
    union =
      r_t_union(
        atom: lub(atomA, atomB),
        list: lub(listA, listB),
        number: lub(numberA, numberB),
        tuple_set: join_tuple_sets(tSetA, tSetB),
        other: lub(otherA, otherB)
      )

    shrink_union(union)
  end

  defp join_unions(r_t_union(atom: atomA) = a, r_t_atom() = b) do
    r_t_union(a, atom: lub(atomA, b))
  end

  defp join_unions(r_t_union(list: listA) = a, b)
       when elem(b, 0) === :t_list or elem(b, 0) === :t_cons or b === nil do
    r_t_union(a, list: lub(listA, b))
  end

  defp join_unions(r_t_union(number: numberA) = a, b)
       when b === :number or elem(b, 0) === :t_float or elem(b, 0) === :t_integer do
    r_t_union(a, number: lub(numberA, b))
  end

  defp join_unions(r_t_union(tuple_set: tSetA) = a, r_t_tuple() = b) do
    set = join_tuple_sets(tSetA, new_tuple_set(b))
    shrink_union(r_t_union(a, tuple_set: set))
  end

  defp join_unions(r_t_union(other: otherA) = a, b) do
    case lub(otherA, b) do
      :any ->
        :any

      t ->
        r_t_union(a, other: t)
    end
  end

  defp join_tuple_sets(a, :none) do
    a
  end

  defp join_tuple_sets(:none, b) do
    b
  end

  defp join_tuple_sets(r_t_tuple() = a, r_t_tuple() = b) do
    lub(a, b)
  end

  defp join_tuple_sets(r_t_tuple() = tuple, records) do
    jts_tuple(records, tuple)
  end

  defp join_tuple_sets(records, r_t_tuple() = tuple) do
    join_tuple_sets(tuple, records)
  end

  defp join_tuple_sets(recordsA, recordsB) do
    jts_records(recordsA, recordsB)
  end

  defp jts_tuple([{_Key, tuple} | records], acc) do
    jts_tuple(records, lub(tuple, acc))
  end

  defp jts_tuple([], acc) do
    acc
  end

  defp jts_records(rsA, rsB) do
    jts_records(rsA, rsB, 0, [])
  end

  defp jts_records([], [], _N, acc) do
    reverse(acc)
  end

  defp jts_records(rsA, rsB, n, acc) when n > 12 do
    a = normalize_tuple_set(rsA, :none)
    b = normalize_tuple_set(rsB, a)
    r_t_tuple() = normalize_tuple_set(acc, b)
  end

  defp jts_records([{key, a} | rsA], [{key, b} | rsB], n, acc) do
    jts_records(rsA, rsB, n + 1, [{key, lub(a, b)} | acc])
  end

  defp jts_records([{keyA, _} | _] = rsA, [{keyB, b} | rsB], n, acc)
       when keyA > keyB do
    jts_records(rsA, rsB, n + 1, [{keyB, b} | acc])
  end

  defp jts_records([{keyA, a} | rsA], [{keyB, _} | _] = rsB, n, acc)
       when keyA < keyB do
    jts_records(rsA, rsB, n + 1, [{keyA, a} | acc])
  end

  defp jts_records([{keyA, a} | rsA], [], n, acc) do
    jts_records(rsA, [], n + 1, [{keyA, a} | acc])
  end

  defp jts_records([], [{keyB, b} | rsB], n, acc) do
    jts_records([], rsB, n + 1, [{keyB, b} | acc])
  end

  def subtract(
        r_t_atom(elements: [_ | _] = set0),
        r_t_atom(elements: [_ | _] = set1)
      ) do
    case :ordsets.subtract(set0, set1) do
      [] ->
        :none

      [_ | _] = set ->
        r_t_atom(elements: set)
    end
  end

  def subtract(r_t_bitstring(size_unit: unitA) = t, r_t_bs_matchable(tail_unit: unitB)) do
    subtract_matchable(t, unitA, unitB)
  end

  def subtract(r_t_bitstring(size_unit: unitA) = t, r_t_bitstring(size_unit: unitB)) do
    subtract_matchable(t, unitA, unitB)
  end

  def subtract(r_t_bs_context(tail_unit: unitA) = t, r_t_bs_matchable(tail_unit: unitB)) do
    subtract_matchable(t, unitA, unitB)
  end

  def subtract(r_t_bs_context(tail_unit: unitA) = t, r_t_bs_context(tail_unit: unitB)) do
    subtract_matchable(t, unitA, unitB)
  end

  def subtract(r_t_integer(elements: {min, max}), r_t_integer(elements: {n, n})) do
    cond do
      min === n and max === n ->
        :none

      min !== n and max !== n ->
        r_t_integer(elements: {min, max})

      min === n ->
        r_t_integer(elements: {min + 1, max})

      max === n ->
        r_t_integer(elements: {min, max - 1})
    end
  end

  def subtract(:number, r_t_float(elements: :any)) do
    r_t_integer()
  end

  def subtract(:number, r_t_integer(elements: :any)) do
    r_t_float()
  end

  def subtract(
        r_t_list(type: typeA, terminator: termA) = t,
        r_t_cons(type: typeB, terminator: termB)
      ) do
    case {meet(typeA, typeB), meet(termA, termB)} do
      {^typeA, ^termA} ->
        nil

      _ ->
        t
    end
  end

  def subtract(r_t_list(type: type, terminator: term), nil) do
    r_t_cons(type: type, terminator: term)
  end

  def subtract(r_t_union(atom: atom) = a, r_t_atom() = b) do
    shrink_union(r_t_union(a, atom: subtract(atom, b)))
  end

  def subtract(r_t_union(number: number) = a, b)
      when b === :number or elem(b, 0) === :t_float or elem(b, 0) === :t_integer do
    shrink_union(r_t_union(a, number: subtract(number, b)))
  end

  def subtract(r_t_union(list: list) = a, b)
      when elem(b, 0) === :t_list or elem(b, 0) === :t_cons or b === nil do
    shrink_union(r_t_union(a, list: subtract(list, b)))
  end

  def subtract(r_t_union(tuple_set: [_ | _] = records0) = a, r_t_tuple() = b) do
    newSet =
      case (for {key, t} <- records0,
                meet(t, b) !== t do
              {key, t}
            end) do
        [_ | _] = records ->
          records

        [] ->
          :none
      end

    shrink_union(r_t_union(a, tuple_set: newSet))
  end

  def subtract(r_t_union(tuple_set: r_t_tuple() = tuple) = a, r_t_tuple() = b) do
    case meet(tuple, b) do
      ^tuple ->
        shrink_union(r_t_union(a, tuple_set: :none))

      _ ->
        a
    end
  end

  def subtract(r_t_union(other: other) = a, b) do
    shrink_union(r_t_union(a, other: subtract(other, b)))
  end

  def subtract(a, b) do
    case meet(a, b) do
      ^a ->
        :none

      _Other ->
        a
    end
  end

  defp subtract_matchable(t, unitA, unitB) do
    cond do
      rem(unitA, unitB) === 0 ->
        :none

      rem(unitA, unitB) !== 0 ->
        t
    end
  end

  def get_bs_matchable_unit(r_t_bitstring(size_unit: unit)) do
    unit
  end

  def get_bs_matchable_unit(r_t_bs_context(tail_unit: unit)) do
    unit
  end

  def get_bs_matchable_unit(r_t_bs_matchable(tail_unit: unit)) do
    unit
  end

  def get_bs_matchable_unit(_) do
    :error
  end

  def is_bs_matchable_type(type) do
    get_bs_matchable_unit(type) !== :error
  end

  def get_singleton_value(r_t_atom(elements: [atom])) do
    {:ok, atom}
  end

  def get_singleton_value(r_t_float(elements: {float, float})) do
    {:ok, float}
  end

  def get_singleton_value(r_t_integer(elements: {int, int})) do
    {:ok, int}
  end

  def get_singleton_value(r_t_map(super_key: :none, super_value: :none)) do
    {:ok, %{}}
  end

  def get_singleton_value(r_t_tuple(exact: true, size: size, elements: es)) do
    case gsv_elements(size, es, []) do
      values when is_list(values) ->
        {:ok, :erlang.list_to_tuple(values)}

      :error ->
        :error
    end
  end

  def get_singleton_value(nil) do
    {:ok, []}
  end

  def get_singleton_value(_) do
    :error
  end

  defp gsv_elements(0, _Es, acc) do
    acc
  end

  defp gsv_elements(n, es, acc) do
    elementType = get_tuple_element(n, es)

    case get_singleton_value(elementType) do
      {:ok, value} ->
        gsv_elements(n - 1, es, [value | acc])

      :error ->
        :error
    end
  end

  def is_singleton_type(type) do
    get_singleton_value(type) !== :error
  end

  def is_boolean_type(r_t_atom(elements: [f, t])) do
    f === false and t === true
  end

  def is_boolean_type(r_t_atom(elements: [b])) do
    is_boolean(b)
  end

  def is_boolean_type(r_t_union() = t) do
    is_boolean_type(normalize(t))
  end

  def is_boolean_type(_) do
    false
  end

  def set_tuple_element(index, _Type, es) when index > 12 do
    es
  end

  def set_tuple_element(_Index, :none, es) do
    es
  end

  def set_tuple_element(index, :any, es) do
    :maps.remove(index, es)
  end

  def set_tuple_element(index, type, es) do
    Map.put(es, index, type)
  end

  def get_tuple_element(index, es) do
    case es do
      %{^index => t} ->
        t

      %{} ->
        :any
    end
  end

  def normalize(
        r_t_union(atom: atom, list: list, number: number, tuple_set: tuples, other: other)
      ) do
    a = lub(atom, list)
    b = lub(a, number)
    c = lub(b, other)
    normalize_tuple_set(tuples, c)
  end

  def normalize(t) do
    verified_normal_type(t)
  end

  defp normalize_tuple_set([{_, a} | records], b) do
    normalize_tuple_set(records, lub(a, b))
  end

  defp normalize_tuple_set([], b) do
    b
  end

  defp normalize_tuple_set(a, b) do
    lub(a, b)
  end

  def make_type_from_value(value) do
    mtfv_1(value)
  end

  defp mtfv_1(a) when is_atom(a) do
    r_t_atom(elements: [a])
  end

  defp mtfv_1(b) when is_bitstring(b) do
    case bit_size(b) do
      0 ->
        r_t_bitstring(size_unit: 8)

      size ->
        r_t_bitstring(size_unit: size)
    end
  end

  defp mtfv_1(f) when is_float(f) do
    make_float(f)
  end

  defp mtfv_1(f) when is_function(f) do
    {:arity, arity} = :erlang.fun_info(f, :arity)
    r_t_fun(arity: arity)
  end

  defp mtfv_1(i) when is_integer(i) do
    make_integer(i)
  end

  defp mtfv_1(l) when is_list(l) do
    case l do
      [_ | _] ->
        mtfv_cons(l, :none)

      [] ->
        nil
    end
  end

  defp mtfv_1(m) when is_map(m) do
    {sKey, sValue} =
      :maps.fold(
        fn key, value, {sKey0, sValue0} ->
          sKey = join(mtfv_1(key), sKey0)
          sValue = join(mtfv_1(value), sValue0)
          {sKey, sValue}
        end,
        {:none, :none},
        m
      )

    r_t_map(super_key: sKey, super_value: sValue)
  end

  defp mtfv_1(t) when is_tuple(t) do
    {es, _} =
      foldl(
        fn val, {es0, index} ->
          type = mtfv_1(val)
          es = set_tuple_element(index, type, es0)
          {es, index + 1}
        end,
        {%{}, 1},
        :erlang.tuple_to_list(t)
      )

    r_t_tuple(exact: true, size: tuple_size(t), elements: es)
  end

  defp mtfv_1(_Term) do
    :any
  end

  defp mtfv_cons([head | tail], type) do
    mtfv_cons(tail, join(mtfv_1(head), type))
  end

  defp mtfv_cons(terminator, type) do
    r_t_cons(type: type, terminator: mtfv_1(terminator))
  end

  def make_atom(atom) when is_atom(atom) do
    r_t_atom(elements: [atom])
  end

  def make_boolean() do
    r_t_atom(elements: [false, true])
  end

  def make_cons(head0, tail) do
    case meet(tail, r_t_cons()) do
      r_t_cons(type: type0, terminator: term0) ->
        type = join(head0, type0)
        term = join(subtract(tail, r_t_cons()), term0)
        r_t_cons(type: type, terminator: term)

      _ ->
        r_t_cons(type: head0, terminator: tail)
    end
  end

  def make_float(float) when is_float(float) do
    make_float(float, float)
  end

  def make_float(min, max)
      when is_float(min) and
             is_float(max) and min <= max do
    r_t_float(elements: {min, max})
  end

  def make_integer(int) when is_integer(int) do
    make_integer(int, int)
  end

  def make_integer(min, max)
      when is_integer(min) and
             is_integer(max) and min <= max do
    r_t_integer(elements: {min, max})
  end

  def limit_depth(type) do
    limit_depth(type, 4)
  end

  defp limit_depth(r_t_cons() = t, depth) do
    limit_depth_list(t, depth)
  end

  defp limit_depth(r_t_list() = t, depth) do
    limit_depth_list(t, depth)
  end

  defp limit_depth(r_t_tuple() = t, depth) do
    limit_depth_tuple(t, depth)
  end

  defp limit_depth(r_t_fun() = t, depth) do
    limit_depth_fun(t, depth)
  end

  defp limit_depth(r_t_map() = t, depth) do
    limit_depth_map(t, depth)
  end

  defp limit_depth(
         r_t_union(list: list0, tuple_set: tupleSet0, other: other0) = u,
         depth
       ) do
    tupleSet = limit_depth_tuple(tupleSet0, depth)
    list = limit_depth_list(list0, depth)
    other = limit_depth(other0, depth)
    shrink_union(r_t_union(u, list: list, tuple_set: tupleSet, other: other))
  end

  defp limit_depth(type, _Depth) do
    type
  end

  defp limit_depth_fun(r_t_fun(type: type0) = t, depth) do
    type =
      cond do
        depth > 0 ->
          limit_depth(type0, depth - 1)

        depth <= 0 ->
          :any
      end

    r_t_fun(t, type: type)
  end

  defp limit_depth_list(r_t_cons(type: type0, terminator: term0) = t, depth) do
    {type, term} = limit_depth_list_1(type0, term0, depth)
    r_t_cons(t, type: type, terminator: term)
  end

  defp limit_depth_list(r_t_list(type: type0, terminator: term0) = t, depth) do
    {type, term} = limit_depth_list_1(type0, term0, depth)
    r_t_list(t, type: type, terminator: term)
  end

  defp limit_depth_list(nil, _Depth) do
    nil
  end

  defp limit_depth_list(:none, _Depth) do
    :none
  end

  defp limit_depth_list_1(type0, terminator0, depth) when depth > 0 do
    type = limit_depth(type0, depth - 1)
    terminator = limit_depth(terminator0, depth - 1)
    {type, terminator}
  end

  defp limit_depth_list_1(_Type, _Terminator, depth) when depth <= 0 do
    {:any, :any}
  end

  defp limit_depth_map(
         r_t_map(super_key: sKey0, super_value: sValue0),
         depth
       )
       when depth > 0 do
    sKey = limit_depth(sKey0, depth - 1)
    sValue = limit_depth(sValue0, depth - 1)
    r_t_map(super_key: sKey, super_value: sValue)
  end

  defp limit_depth_map(r_t_map(), depth) when depth <= 0 do
    r_t_map()
  end

  defp limit_depth_tuple(r_t_tuple(elements: es0) = t, depth) do
    cond do
      depth > 0 ->
        es =
          :maps.map(
            fn _, e ->
              limit_depth(e, depth - 1)
            end,
            es0
          )

        r_t_tuple(t, elements: es)

      depth <= 0 ->
        r_t_tuple(elements: %{})
    end
  end

  defp limit_depth_tuple([{{minSize, _}, _} | _], depth)
       when depth <= 0 do
    r_t_tuple(exact: false, size: minSize)
  end

  defp limit_depth_tuple([{szTag, tuple} | ts], depth) do
    [
      {szTag, limit_depth_tuple(tuple, depth)}
      | limit_depth_tuple(ts, depth)
    ]
  end

  defp limit_depth_tuple([], _Depth) do
    []
  end

  defp limit_depth_tuple(:none, _Depth) do
    :none
  end

  defp glb(t, t) do
    verified_normal_type(t)
  end

  defp glb(:any, t) do
    verified_normal_type(t)
  end

  defp glb(t, :any) do
    verified_normal_type(t)
  end

  defp glb(
         r_t_atom(elements: [_ | _] = set1),
         r_t_atom(elements: [_ | _] = set2)
       ) do
    case :ordsets.intersection(set1, set2) do
      [] ->
        :none

      [_ | _] = set ->
        r_t_atom(elements: set)
    end
  end

  defp glb(r_t_atom(elements: [_ | _]) = t, r_t_atom(elements: :any)) do
    t
  end

  defp glb(r_t_atom(elements: :any), r_t_atom(elements: [_ | _]) = t) do
    t
  end

  defp glb(r_t_bitstring(size_unit: u1), r_t_bitstring(size_unit: u2)) do
    r_t_bitstring(size_unit: div(u1 * u2, gcd(u1, u2)))
  end

  defp glb(r_t_bitstring(size_unit: unitA) = t, r_t_bs_matchable(tail_unit: unitB)) do
    unit = div(unitA * unitB, gcd(unitA, unitB))
    r_t_bitstring(t, size_unit: unit)
  end

  defp glb(
         r_t_bs_context(tail_unit: unitA, slots: slotCountA, valid: validSlotsA),
         r_t_bs_context(tail_unit: unitB, slots: slotCountB, valid: validSlotsB)
       ) do
    commonSlotMask = 1 <<< (min(slotCountA, slotCountB) - 1)
    commonSlotsA = validSlotsA &&& commonSlotMask
    commonSlotsB = validSlotsB &&& commonSlotMask
    unit = div(unitA * unitB, gcd(unitA, unitB))

    cond do
      commonSlotsA === commonSlotsB ->
        r_t_bs_context(
          tail_unit: unit,
          slots: max(slotCountA, slotCountB),
          valid: validSlotsA ||| validSlotsB
        )

      commonSlotsA !== commonSlotsB ->
        :none
    end
  end

  defp glb(r_t_bs_context(tail_unit: unitA) = t, r_t_bs_matchable(tail_unit: unitB)) do
    unit = div(unitA * unitB, gcd(unitA, unitB))
    r_t_bs_context(t, tail_unit: unit)
  end

  defp glb(r_t_bs_matchable(tail_unit: unitA), r_t_bs_matchable(tail_unit: unitB)) do
    unit = div(unitA * unitB, gcd(unitA, unitB))
    r_t_bs_matchable(tail_unit: unit)
  end

  defp glb(r_t_bs_matchable(tail_unit: unitA), r_t_bitstring(size_unit: unitB) = t) do
    unit = div(unitA * unitB, gcd(unitA, unitB))
    r_t_bitstring(t, size_unit: unit)
  end

  defp glb(r_t_bs_matchable(tail_unit: unitA), r_t_bs_context(tail_unit: unitB) = t) do
    unit = div(unitA * unitB, gcd(unitA, unitB))
    r_t_bs_context(t, tail_unit: unit)
  end

  defp glb(
         r_t_cons(type: typeA, terminator: termA),
         r_t_cons(type: typeB, terminator: termB)
       ) do
    case {meet(typeA, typeB), meet(termA, termB)} do
      {:none, _} ->
        :none

      {_, :none} ->
        :none

      {type, term} ->
        r_t_cons(type: type, terminator: term)
    end
  end

  defp glb(
         r_t_cons(type: typeA, terminator: termA),
         r_t_list(type: typeB, terminator: termB)
       ) do
    case {meet(typeA, typeB), meet(termA, termB)} do
      {:none, _} ->
        :none

      {_, :none} ->
        :none

      {type, term} ->
        r_t_cons(type: type, terminator: term)
    end
  end

  defp glb(r_t_float() = t, r_t_float(elements: :any)) do
    t
  end

  defp glb(r_t_float(elements: :any), r_t_float() = t) do
    t
  end

  defp glb(
         r_t_float(elements: {minA, maxA}),
         r_t_float(elements: {minB, maxB})
       )
       when (minA >= minB and minA <= maxB) or
              (minB >= minA and minB <= maxA) do
    true = minA <= maxA and minB <= maxB
    r_t_float(elements: {max(minA, minB), min(maxA, maxB)})
  end

  defp glb(
         r_t_fun(arity: same, type: typeA),
         r_t_fun(arity: same, type: typeB) = t
       ) do
    r_t_fun(t, type: meet(typeA, typeB))
  end

  defp glb(
         r_t_fun(arity: :any, type: typeA),
         r_t_fun(type: typeB) = t
       ) do
    r_t_fun(t, type: meet(typeA, typeB))
  end

  defp glb(
         r_t_fun(type: typeA) = t,
         r_t_fun(arity: :any, type: typeB)
       ) do
    r_t_fun(t, type: meet(typeA, typeB))
  end

  defp glb(r_t_integer(elements: {_, _}) = t, r_t_integer(elements: :any)) do
    t
  end

  defp glb(r_t_integer(elements: :any), r_t_integer(elements: {_, _}) = t) do
    t
  end

  defp glb(
         r_t_integer(elements: {minA, maxA}),
         r_t_integer(elements: {minB, maxB})
       )
       when (minA >= minB and minA <= maxB) or
              (minB >= minA and minB <= maxA) do
    true = minA <= maxA and minB <= maxB
    r_t_integer(elements: {max(minA, minB), min(maxA, maxB)})
  end

  defp glb(r_t_integer() = t, :number) do
    t
  end

  defp glb(r_t_float() = t, :number) do
    t
  end

  defp glb(
         r_t_list(type: typeA, terminator: termA),
         r_t_list(type: typeB, terminator: termB)
       ) do
    case {meet(typeA, typeB), meet(termA, termB)} do
      {:none, _} ->
        nil

      {_, :none} ->
        nil

      {type, term} ->
        r_t_list(type: type, terminator: term)
    end
  end

  defp glb(r_t_list() = a, r_t_cons() = b) do
    glb(b, a)
  end

  defp glb(r_t_list(), nil) do
    nil
  end

  defp glb(nil, r_t_list()) do
    nil
  end

  defp glb(:number, r_t_integer() = t) do
    t
  end

  defp glb(:number, r_t_float() = t) do
    t
  end

  defp glb(
         r_t_map(super_key: sKeyA, super_value: sValueA),
         r_t_map(super_key: sKeyB, super_value: sValueB)
       ) do
    sKey = meet(sKeyA, sKeyB)
    sValue = meet(sValueA, sValueB)
    r_t_map(super_key: sKey, super_value: sValue)
  end

  defp glb(r_t_tuple() = t1, r_t_tuple() = t2) do
    glb_tuples(t1, t2)
  end

  defp glb(_, _) do
    :none
  end

  defp glb_tuples(
         r_t_tuple(size: sz1, exact: ex1),
         r_t_tuple(size: sz2, exact: ex2)
       )
       when (ex1 and sz1 < sz2) or (ex2 and sz2 < sz1) do
    :none
  end

  defp glb_tuples(
         r_t_tuple(size: sz1, exact: ex1, elements: es1),
         r_t_tuple(size: sz2, exact: ex2, elements: es2)
       ) do
    size = max(sz1, sz2)
    exact = :erlang.or(ex1, ex2)

    case glb_elements(es1, es2) do
      :none ->
        :none

      es ->
        r_t_tuple(size: size, exact: exact, elements: es)
    end
  end

  defp glb_elements(es1, es2) do
    keys = :maps.keys(es1) ++ :maps.keys(es2)
    glb_elements_1(keys, es1, es2, %{})
  end

  defp glb_elements_1([key | keys], es1, es2, acc) do
    case {es1, es2} do
      {%{^key => type1}, %{^key => type2}} ->
        case meet(type1, type2) do
          :none ->
            :none

          type ->
            glb_elements_1(keys, es1, es2, Map.put(acc, key, type))
        end

      {%{^key => type1}, _} ->
        glb_elements_1(keys, es1, es2, Map.put(acc, key, type1))

      {_, %{^key => type2}} ->
        glb_elements_1(keys, es1, es2, Map.put(acc, key, type2))
    end
  end

  defp glb_elements_1([], _Es1, _Es2, acc) do
    acc
  end

  defp lub(t, t) do
    verified_normal_type(t)
  end

  defp lub(:none, t) do
    verified_normal_type(t)
  end

  defp lub(t, :none) do
    verified_normal_type(t)
  end

  defp lub(:any, _) do
    :any
  end

  defp lub(_, :any) do
    :any
  end

  defp lub(
         r_t_atom(elements: [_ | _] = set1),
         r_t_atom(elements: [_ | _] = set2)
       ) do
    set = :ordsets.union(set1, set2)

    case :ordsets.size(set) do
      size when size <= 5 ->
        r_t_atom(elements: set)

      _Size ->
        r_t_atom(elements: :any)
    end
  end

  defp lub(r_t_atom(elements: :any) = t, r_t_atom(elements: [_ | _])) do
    t
  end

  defp lub(r_t_atom(elements: [_ | _]), r_t_atom(elements: :any) = t) do
    t
  end

  defp lub(r_t_bitstring(size_unit: u1), r_t_bitstring(size_unit: u2)) do
    r_t_bitstring(size_unit: gcd(u1, u2))
  end

  defp lub(r_t_bitstring(size_unit: u1), r_t_bs_context(tail_unit: u2)) do
    r_t_bs_matchable(tail_unit: gcd(u1, u2))
  end

  defp lub(r_t_bitstring(size_unit: unitA), r_t_bs_matchable(tail_unit: unitB)) do
    lub_bs_matchable(unitA, unitB)
  end

  defp lub(
         r_t_bs_context(tail_unit: unitA, slots: slotsA, valid: validA),
         r_t_bs_context(tail_unit: unitB, slots: slotsB, valid: validB)
       ) do
    r_t_bs_context(
      tail_unit: gcd(unitA, unitB),
      slots: min(slotsA, slotsB),
      valid: validA &&& validB
    )
  end

  defp lub(r_t_bs_context(tail_unit: u1), r_t_bitstring(size_unit: u2)) do
    r_t_bs_matchable(tail_unit: gcd(u1, u2))
  end

  defp lub(r_t_bs_context(tail_unit: unitA), r_t_bs_matchable(tail_unit: unitB)) do
    lub_bs_matchable(unitA, unitB)
  end

  defp lub(r_t_bs_matchable(tail_unit: unitA), r_t_bs_matchable(tail_unit: unitB)) do
    lub_bs_matchable(unitA, unitB)
  end

  defp lub(r_t_bs_matchable(tail_unit: unitA), r_t_bitstring(size_unit: unitB)) do
    lub_bs_matchable(unitA, unitB)
  end

  defp lub(r_t_bs_matchable(tail_unit: unitA), r_t_bs_context(tail_unit: unitB)) do
    lub_bs_matchable(unitA, unitB)
  end

  defp lub(
         r_t_cons(type: typeA, terminator: termA),
         r_t_cons(type: typeB, terminator: termB)
       ) do
    r_t_cons(
      type: join(typeA, typeB),
      terminator: join(termA, termB)
    )
  end

  defp lub(
         r_t_cons(type: typeA, terminator: termA),
         r_t_list(type: typeB, terminator: termB)
       ) do
    r_t_list(
      type: join(typeA, typeB),
      terminator: join(termA, termB)
    )
  end

  defp lub(r_t_cons(type: type, terminator: term), nil) do
    r_t_list(type: type, terminator: term)
  end

  defp lub(
         r_t_float(elements: {minA, maxA}),
         r_t_float(elements: {minB, maxB})
       ) do
    r_t_float(elements: {min(minA, minB), max(maxA, maxB)})
  end

  defp lub(r_t_float(), r_t_float()) do
    r_t_float()
  end

  defp lub(r_t_float(), r_t_integer()) do
    :number
  end

  defp lub(r_t_float(), :number) do
    :number
  end

  defp lub(
         r_t_fun(arity: same, type: typeA),
         r_t_fun(arity: same, type: typeB)
       ) do
    r_t_fun(arity: same, type: join(typeA, typeB))
  end

  defp lub(r_t_fun(type: typeA), r_t_fun(type: typeB)) do
    r_t_fun(type: join(typeA, typeB))
  end

  defp lub(
         r_t_integer(elements: {minA, maxA}),
         r_t_integer(elements: {minB, maxB})
       ) do
    r_t_integer(elements: {min(minA, minB), max(maxA, maxB)})
  end

  defp lub(r_t_integer(), r_t_integer()) do
    r_t_integer()
  end

  defp lub(r_t_integer(), r_t_float()) do
    :number
  end

  defp lub(r_t_integer(), :number) do
    :number
  end

  defp lub(
         r_t_list(type: typeA, terminator: termA),
         r_t_list(type: typeB, terminator: termB)
       ) do
    r_t_list(
      type: join(typeA, typeB),
      terminator: join(termA, termB)
    )
  end

  defp lub(r_t_list() = a, r_t_cons() = b) do
    lub(b, a)
  end

  defp lub(nil = a, r_t_cons() = b) do
    lub(b, a)
  end

  defp lub(nil, r_t_list() = t) do
    t
  end

  defp lub(r_t_list() = t, nil) do
    t
  end

  defp lub(:number, r_t_integer()) do
    :number
  end

  defp lub(:number, r_t_float()) do
    :number
  end

  defp lub(
         r_t_map(super_key: sKeyA, super_value: sValueA),
         r_t_map(super_key: sKeyB, super_value: sValueB)
       ) do
    sKey = join(sKeyA, sKeyB)
    sValue = join(sValueA, sValueB)
    r_t_map(super_key: sKey, super_value: sValue)
  end

  defp lub(
         r_t_tuple(size: sz, exact: exactA, elements: esA),
         r_t_tuple(size: sz, exact: exactB, elements: esB)
       ) do
    exact = :erlang.and(exactA, exactB)
    es = lub_tuple_elements(sz, esA, esB)
    r_t_tuple(size: sz, exact: exact, elements: es)
  end

  defp lub(
         r_t_tuple(size: szA, elements: esA),
         r_t_tuple(size: szB, elements: esB)
       ) do
    sz = min(szA, szB)
    es = lub_tuple_elements(sz, esA, esB)
    r_t_tuple(size: sz, elements: es)
  end

  defp lub(_T1, _T2) do
    :any
  end

  defp lub_bs_matchable(unitA, unitB) do
    r_t_bs_matchable(tail_unit: gcd(unitA, unitB))
  end

  defp lub_tuple_elements(minSize, esA, esB) do
    es0 = lub_elements(esA, esB)

    :maps.filter(
      fn index, _Type ->
        index <= minSize
      end,
      es0
    )
  end

  defp lub_elements(es1, es2) do
    keys =
      cond do
        map_size(es1) <= map_size(es2) ->
          :maps.keys(es1)

        map_size(es1) > map_size(es2) ->
          :maps.keys(es2)
      end

    lub_elements_1(keys, es1, es2, %{})
  end

  defp lub_elements_1([key | keys], es1, es2, acc0) do
    case {es1, es2} do
      {%{^key => type1}, %{^key => type2}} ->
        acc = set_tuple_element(key, join(type1, type2), acc0)
        lub_elements_1(keys, es1, es2, acc)

      {%{}, %{}} ->
        lub_elements_1(keys, es1, es2, acc0)
    end
  end

  defp lub_elements_1([], _Es1, _Es2, acc) do
    acc
  end

  defp gcd(a, b) do
    case rem(a, b) do
      0 ->
        b

      x ->
        gcd(b, x)
    end
  end

  defp record_key(r_t_tuple(exact: true, size: size, elements: %{1 => tag})) do
    case is_singleton_type(tag) do
      true ->
        {size, tag}

      false ->
        :none
    end
  end

  defp record_key(_) do
    :none
  end

  defp new_tuple_set(t) do
    case record_key(t) do
      :none ->
        t

      key ->
        [{key, t}]
    end
  end

  defp shrink_union(r_t_union(other: :any)) do
    :any
  end

  defp shrink_union(
         r_t_union(atom: atom, list: :none, number: :none, tuple_set: :none, other: :none)
       ) do
    atom
  end

  defp shrink_union(
         r_t_union(atom: :none, list: list, number: :none, tuple_set: :none, other: :none)
       ) do
    list
  end

  defp shrink_union(
         r_t_union(atom: :none, list: :none, number: number, tuple_set: :none, other: :none)
       ) do
    number
  end

  defp shrink_union(
         r_t_union(
           atom: :none,
           list: :none,
           number: :none,
           tuple_set: r_t_tuple() = tuple,
           other: :none
         )
       ) do
    tuple
  end

  defp shrink_union(
         r_t_union(
           atom: :none,
           list: :none,
           number: :none,
           tuple_set: [{_Key, record}],
           other: :none
         )
       ) do
    r_t_tuple() = record
  end

  defp shrink_union(
         r_t_union(atom: :none, list: :none, number: :none, tuple_set: :none, other: other)
       ) do
    other
  end

  defp shrink_union(r_t_union() = t) do
    t
  end

  def verified_type(
        r_t_union(atom: atom, list: list, number: number, tuple_set: tSet, other: other) = t
      ) do
    _ = verified_normal_type(atom)
    _ = verified_normal_type(list)
    _ = verified_normal_type(number)
    _ = verify_tuple_set(tSet)
    _ = verified_normal_type(other)
    t
  end

  def verified_type(t) do
    verified_normal_type(t)
  end

  defp verify_tuple_set([_ | _] = t) do
    _ = verify_tuple_set_1(t, 0)
    t
  end

  defp verify_tuple_set(r_t_tuple() = t) do
    :none = record_key(t)
    t
  end

  defp verify_tuple_set(:none = t) do
    t
  end

  defp verify_tuple_set_1([{_Tag, record} | records], size) do
    true = size <= 12
    _ = verified_normal_type(record)
    verify_tuple_set_1(records, size + 1)
  end

  defp verify_tuple_set_1([], _Size) do
    :ok
  end

  defp verified_normal_type(:any = t) do
    t
  end

  defp verified_normal_type(:none = t) do
    t
  end

  defp verified_normal_type(r_t_atom(elements: :any) = t) do
    t
  end

  defp verified_normal_type(r_t_atom(elements: [_ | _]) = t) do
    t
  end

  defp verified_normal_type(r_t_bitstring(size_unit: u) = t)
       when is_integer(u) and
              u >= 1 do
    t
  end

  defp verified_normal_type(r_t_bs_context(tail_unit: u) = t)
       when is_integer(u) and
              u >= 1 do
    t
  end

  defp verified_normal_type(r_t_bs_matchable(tail_unit: u) = t)
       when is_integer(u) and
              u >= 1 do
    t
  end

  defp verified_normal_type(r_t_cons(type: type, terminator: term) = t) do
    _ = verified_type(type)
    _ = verified_type(term)
    t
  end

  defp verified_normal_type(r_t_fun(arity: arity, type: returnType) = t)
       when arity === :any or is_integer(arity) do
    _ = verified_type(returnType)
    t
  end

  defp verified_normal_type(r_t_float() = t) do
    t
  end

  defp verified_normal_type(r_t_integer(elements: :any) = t) do
    t
  end

  defp verified_normal_type(r_t_integer(elements: {min, max}) = t)
       when is_integer(min) and is_integer(max) and
              min <= max do
    t
  end

  defp verified_normal_type(r_t_list(type: type, terminator: term) = t) do
    _ = verified_type(type)
    _ = verified_type(term)
    t
  end

  defp verified_normal_type(r_t_map() = t) do
    t
  end

  defp verified_normal_type(nil = t) do
    t
  end

  defp verified_normal_type(:number = t) do
    t
  end

  defp verified_normal_type(r_t_tuple(size: size, elements: es) = t) do
    :maps.fold(
      fn index, element, _
         when is_integer(index) and 1 <= index and
                index <= size and index <= 12 and
                element !== :any and element !== :none ->
        verified_type(element)
      end,
      [],
      es
    )

    t
  end
end
