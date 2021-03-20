defmodule :m_sofs do
  use Bitwise

  import :lists,
    only: [
      any: 2,
      append: 1,
      flatten: 1,
      foreach: 2,
      keysort: 2,
      last: 1,
      map: 2,
      mapfoldl: 3,
      member: 2,
      merge: 2,
      reverse: 1,
      reverse: 2,
      sort: 1,
      umerge: 1,
      umerge: 2,
      usort: 1
    ]

  require Record
  Record.defrecord(:r_Set, :Set, data: [], type: :type)

  Record.defrecord(:r_OrdSet, :OrdSet,
    orddata: {},
    ordtype: :type
  )

  def from_term(t) do
    type =
      case t do
        _ when is_list(t) ->
          [:_]

        _ ->
          :_
      end

    try do
      setify(t, type)
    catch
      _, _ ->
        :erlang.error(:badarg)
    end
  end

  def from_term(l, t) do
    case is_type(t) do
      true ->
        try do
          setify(l, t)
        catch
          _, _ ->
            :erlang.error(:badarg)
        end

      false ->
        :erlang.error(:badarg)
    end
  end

  def from_external(l, [type]) do
    r_Set(data: l, type: type)
  end

  def from_external(t, type) do
    r_OrdSet(orddata: t, ordtype: type)
  end

  def empty_set() do
    r_Set(data: [], type: :_)
  end

  def is_type(atom) when is_atom(atom) and atom !== :_ do
    true
  end

  def is_type([t]) do
    is_element_type(t)
  end

  def is_type(t) when tuple_size(t) > 0 do
    is_types(tuple_size(t), t)
  end

  def is_type(_T) do
    false
  end

  def set(l) do
    try do
      usort(l)
    catch
      _, _ ->
        :erlang.error(:badarg)
    else
      sL ->
        r_Set(data: sL, type: :atom)
    end
  end

  def set(l, [type])
      when is_atom(type) and
             type !== :_ do
    try do
      usort(l)
    catch
      _, _ ->
        :erlang.error(:badarg)
    else
      sL ->
        r_Set(data: sL, type: type)
    end
  end

  def set(l, [_] = t) do
    try do
      setify(l, t)
    catch
      _, _ ->
        :erlang.error(:badarg)
    end
  end

  def set(_, _) do
    :erlang.error(:badarg)
  end

  def from_sets(ss) when is_list(ss) do
    case set_of_sets(ss, [], :_) do
      {:error, error} ->
        :erlang.error(error)

      set ->
        set
    end
  end

  def from_sets(tuple) when is_tuple(tuple) do
    case ordset_of_sets(:erlang.tuple_to_list(tuple), [], []) do
      :error ->
        :erlang.error(:badarg)

      set ->
        set
    end
  end

  def from_sets(_) do
    :erlang.error(:badarg)
  end

  def relation([]) do
    r_Set(data: [], type: {:atom, :atom})
  end

  def relation(ts = [t | _]) when is_tuple(t) do
    try do
      rel(ts, tuple_size(t))
    catch
      _, _ ->
        :erlang.error(:badarg)
    end
  end

  def relation(_) do
    :erlang.error(:badarg)
  end

  def relation(ts, tS) do
    try do
      rel(ts, tS)
    catch
      _, _ ->
        :erlang.error(:badarg)
    end
  end

  def a_function(ts) do
    try do
      func(ts, {:atom, :atom})
    catch
      _, _ ->
        :erlang.error(:badarg)
    else
      bad when is_atom(bad) ->
        :erlang.error(bad)

      set ->
        set
    end
  end

  def a_function(ts, t) do
    try do
      a_func(ts, t)
    catch
      _, _ ->
        :erlang.error(:badarg)
    else
      bad when is_atom(bad) ->
        :erlang.error(bad)

      set ->
        set
    end
  end

  def family(ts) do
    try do
      fam2(ts, {:atom, [:atom]})
    catch
      _, _ ->
        :erlang.error(:badarg)
    else
      bad when is_atom(bad) ->
        :erlang.error(bad)

      set ->
        set
    end
  end

  def family(ts, t) do
    try do
      fam(ts, t)
    catch
      _, _ ->
        :erlang.error(:badarg)
    else
      bad when is_atom(bad) ->
        :erlang.error(bad)

      set ->
        set
    end
  end

  def to_external(s) when elem(s, 0) === :Set do
    r_Set(s, :data)
  end

  def to_external(s) when elem(s, 0) === :OrdSet do
    r_OrdSet(s, :orddata)
  end

  def type(s) when elem(s, 0) === :Set do
    [r_Set(s, :type)]
  end

  def type(s) when elem(s, 0) === :OrdSet do
    r_OrdSet(s, :ordtype)
  end

  def to_sets(s) when elem(s, 0) === :Set do
    case r_Set(s, :type) do
      [type] ->
        list_of_sets(r_Set(s, :data), type, [])

      type ->
        list_of_ordsets(r_Set(s, :data), type, [])
    end
  end

  def to_sets(s)
      when elem(s, 0) === :OrdSet and
             is_tuple(r_OrdSet(s, :ordtype)) do
    tuple_of_sets(
      :erlang.tuple_to_list(r_OrdSet(s, :orddata)),
      :erlang.tuple_to_list(r_OrdSet(s, :ordtype)),
      []
    )
  end

  def to_sets(s) when elem(s, 0) === :OrdSet do
    :erlang.error(:badarg)
  end

  def no_elements(s) when elem(s, 0) === :Set do
    length(r_Set(s, :data))
  end

  def no_elements(s)
      when elem(s, 0) === :OrdSet and
             is_tuple(r_OrdSet(s, :ordtype)) do
    tuple_size(r_OrdSet(s, :orddata))
  end

  def no_elements(s) when elem(s, 0) === :OrdSet do
    :erlang.error(:badarg)
  end

  def specification(fun, s) when elem(s, 0) === :Set do
    type = r_Set(s, :type)

    r =
      case external_fun(fun) do
        false ->
          spec(r_Set(s, :data), fun, element_type(type), [])

        xFun ->
          specification(r_Set(s, :data), xFun, [])
      end

    case r do
      sL when is_list(sL) ->
        r_Set(data: sL, type: type)

      bad ->
        :erlang.error(bad)
    end
  end

  def union(s1, s2)
      when elem(s1, 0) === :Set and
             elem(s2, 0) === :Set do
    case unify_types(r_Set(s1, :type), r_Set(s2, :type)) do
      [] ->
        :erlang.error(:type_mismatch)

      type ->
        r_Set(data: umerge(r_Set(s1, :data), r_Set(s2, :data)), type: type)
    end
  end

  def intersection(s1, s2)
      when elem(s1, 0) === :Set and
             elem(s2, 0) === :Set do
    case unify_types(r_Set(s1, :type), r_Set(s2, :type)) do
      [] ->
        :erlang.error(:type_mismatch)

      type ->
        r_Set(
          data: intersection(r_Set(s1, :data), r_Set(s2, :data), []),
          type: type
        )
    end
  end

  def difference(s1, s2)
      when elem(s1, 0) === :Set and
             elem(s2, 0) === :Set do
    case unify_types(r_Set(s1, :type), r_Set(s2, :type)) do
      [] ->
        :erlang.error(:type_mismatch)

      type ->
        r_Set(
          data: difference(r_Set(s1, :data), r_Set(s2, :data), []),
          type: type
        )
    end
  end

  def symdiff(s1, s2)
      when elem(s1, 0) === :Set and
             elem(s2, 0) === :Set do
    case unify_types(r_Set(s1, :type), r_Set(s2, :type)) do
      [] ->
        :erlang.error(:type_mismatch)

      type ->
        r_Set(
          data: symdiff(r_Set(s1, :data), r_Set(s2, :data), []),
          type: type
        )
    end
  end

  def symmetric_partition(s1, s2)
      when elem(s1, 0) === :Set and
             elem(s2, 0) === :Set do
    case unify_types(r_Set(s1, :type), r_Set(s2, :type)) do
      [] ->
        :erlang.error(:type_mismatch)

      type ->
        sympart(r_Set(s1, :data), r_Set(s2, :data), [], [], [], type)
    end
  end

  def product(s1, s2)
      when elem(s1, 0) === :Set and
             elem(s2, 0) === :Set do
    cond do
      r_Set(s1, :type) === :_ ->
        s1

      r_Set(s2, :type) === :_ ->
        s2

      true ->
        f = fn e ->
          {0, e}
        end

        t = {r_Set(s1, :type), r_Set(s2, :type)}

        r_Set(
          data:
            relprod(
              map(f, r_Set(s1, :data)),
              map(f, r_Set(s2, :data))
            ),
          type: t
        )
    end
  end

  def product({s1, s2}) do
    product(s1, s2)
  end

  def product(t) when is_tuple(t) do
    ss = :erlang.tuple_to_list(t)

    try do
      sets_to_list(ss)
    catch
      _, _ ->
        :erlang.error(:badarg)
    else
      [] ->
        :erlang.error(:badarg)

      l ->
        type = types(ss, [])

        case member([], l) do
          true ->
            empty_set()

          false ->
            r_Set(data: reverse(prod(l, [], [])), type: type)
        end
    end
  end

  def constant_function(s, e) when elem(s, 0) === :Set do
    case {r_Set(s, :type), is_sofs_set(e)} do
      {:_, true} ->
        s

      {type, true} ->
        nType = {type, type(e)}

        r_Set(
          data: constant_function(r_Set(s, :data), to_external(e), []),
          type: nType
        )

      _ ->
        :erlang.error(:badarg)
    end
  end

  def constant_function(s, _) when elem(s, 0) === :OrdSet do
    :erlang.error(:badarg)
  end

  def is_equal(s1, s2)
      when elem(s1, 0) === :Set and
             elem(s2, 0) === :Set do
    case match_types(r_Set(s1, :type), r_Set(s2, :type)) do
      true ->
        r_Set(s1, :data) == r_Set(s2, :data)

      false ->
        :erlang.error(:type_mismatch)
    end
  end

  def is_equal(s1, s2)
      when elem(s1, 0) === :OrdSet and
             elem(s2, 0) === :OrdSet do
    case match_types(r_OrdSet(s1, :ordtype), r_OrdSet(s2, :ordtype)) do
      true ->
        r_OrdSet(s1, :orddata) == r_OrdSet(s2, :orddata)

      false ->
        :erlang.error(:type_mismatch)
    end
  end

  def is_equal(s1, s2)
      when elem(s1, 0) === :Set and
             elem(s2, 0) === :OrdSet do
    :erlang.error(:type_mismatch)
  end

  def is_equal(s1, s2)
      when elem(s1, 0) === :OrdSet and
             elem(s2, 0) === :Set do
    :erlang.error(:type_mismatch)
  end

  def is_subset(s1, s2)
      when elem(s1, 0) === :Set and
             elem(s2, 0) === :Set do
    case match_types(r_Set(s1, :type), r_Set(s2, :type)) do
      true ->
        subset(r_Set(s1, :data), r_Set(s2, :data))

      false ->
        :erlang.error(:type_mismatch)
    end
  end

  def is_sofs_set(s) when elem(s, 0) === :Set do
    true
  end

  def is_sofs_set(s) when elem(s, 0) === :OrdSet do
    true
  end

  def is_sofs_set(_S) do
    false
  end

  def is_set(s) when elem(s, 0) === :Set do
    true
  end

  def is_set(s) when elem(s, 0) === :OrdSet do
    false
  end

  def is_empty_set(s) when elem(s, 0) === :Set do
    r_Set(s, :data) === []
  end

  def is_empty_set(s) when elem(s, 0) === :OrdSet do
    false
  end

  def is_disjoint(s1, s2)
      when elem(s1, 0) === :Set and
             elem(s2, 0) === :Set do
    case match_types(r_Set(s1, :type), r_Set(s2, :type)) do
      true ->
        case r_Set(s1, :data) do
          [] ->
            true

          [a | as] ->
            disjoint(r_Set(s2, :data), a, as)
        end

      false ->
        :erlang.error(:type_mismatch)
    end
  end

  def union(sets) when elem(sets, 0) === :Set do
    case r_Set(sets, :type) do
      [type] ->
        r_Set(data: lunion(r_Set(sets, :data)), type: type)

      :_ ->
        sets

      _ ->
        :erlang.error(:badarg)
    end
  end

  def intersection(sets) when elem(sets, 0) === :Set do
    case r_Set(sets, :data) do
      [] ->
        :erlang.error(:badarg)

      [l | ls] ->
        case r_Set(sets, :type) do
          [type] ->
            r_Set(data: lintersection(ls, l), type: type)

          _ ->
            :erlang.error(:badarg)
        end
    end
  end

  def canonical_relation(sets) when elem(sets, 0) === :Set do
    sT = r_Set(sets, :type)

    case sT do
      [:_] ->
        empty_set()

      [type] ->
        r_Set(data: can_rel(r_Set(sets, :data), []), type: {type, sT})

      :_ ->
        sets

      _ ->
        :erlang.error(:badarg)
    end
  end

  def rel2fam(r) do
    relation_to_family(r)
  end

  def relation_to_family(r) when elem(r, 0) === :Set do
    case r_Set(r, :type) do
      {dT, rT} ->
        r_Set(data: rel2family(r_Set(r, :data)), type: {dT, [rT]})

      :_ ->
        r

      _Else ->
        :erlang.error(:badarg)
    end
  end

  def domain(r) when elem(r, 0) === :Set do
    case r_Set(r, :type) do
      {dT, _} ->
        r_Set(data: dom(r_Set(r, :data)), type: dT)

      :_ ->
        r

      _Else ->
        :erlang.error(:badarg)
    end
  end

  def range(r) when elem(r, 0) === :Set do
    case r_Set(r, :type) do
      {_, rT} ->
        r_Set(data: ran(r_Set(r, :data), []), type: rT)

      :_ ->
        r

      _ ->
        :erlang.error(:badarg)
    end
  end

  def field(r) do
    union(domain(r), range(r))
  end

  def relative_product(rT) when is_tuple(rT) do
    relative_product(:erlang.tuple_to_list(rT))
  end

  def relative_product(rL) when is_list(rL) do
    case relprod_n(rL, :foo, false, false) do
      {:error, reason} ->
        :erlang.error(reason)

      reply ->
        reply
    end
  end

  def relative_product(r1, r2)
      when elem(r1, 0) === :Set and
             elem(r2, 0) === :Set do
    relative_product1(converse(r1), r2)
  end

  def relative_product(rT, r)
      when is_tuple(rT) and
             elem(r, 0) === :Set do
    relative_product(:erlang.tuple_to_list(rT), r)
  end

  def relative_product(rL, r)
      when is_list(rL) and
             elem(r, 0) === :Set do
    emptyR =
      case r_Set(r, :type) do
        {_, _} ->
          r_Set(r, :data) === []

        :_ ->
          true

        _ ->
          :erlang.error(:badarg)
      end

    case relprod_n(rL, r, emptyR, true) do
      {:error, reason} ->
        :erlang.error(reason)

      reply ->
        reply
    end
  end

  def relative_product1(r1, r2)
      when elem(r1, 0) === :Set and
             elem(r2, 0) === :Set do
    {dTR1, rTR1} =
      case r_Set(r1, :type) do
        {_, _} = r1T ->
          r1T

        :_ ->
          {:_, :_}

        _ ->
          :erlang.error(:badarg)
      end

    {dTR2, rTR2} =
      case r_Set(r2, :type) do
        {_, _} = r2T ->
          r2T

        :_ ->
          {:_, :_}

        _ ->
          :erlang.error(:badarg)
      end

    case match_types(dTR1, dTR2) do
      true when dTR1 === :_ ->
        r1

      true when dTR2 === :_ ->
        r2

      true ->
        r_Set(
          data: relprod(r_Set(r1, :data), r_Set(r2, :data)),
          type: {rTR1, rTR2}
        )

      false ->
        :erlang.error(:type_mismatch)
    end
  end

  def converse(r) when elem(r, 0) === :Set do
    case r_Set(r, :type) do
      {dT, rT} ->
        r_Set(data: converse(r_Set(r, :data), []), type: {rT, dT})

      :_ ->
        r

      _ ->
        :erlang.error(:badarg)
    end
  end

  def image(r, s)
      when elem(r, 0) === :Set and
             elem(s, 0) === :Set do
    case r_Set(r, :type) do
      {dT, rT} ->
        case match_types(dT, r_Set(s, :type)) do
          true ->
            r_Set(
              data: usort(restrict(r_Set(s, :data), r_Set(r, :data))),
              type: rT
            )

          false ->
            :erlang.error(:type_mismatch)
        end

      :_ ->
        r

      _ ->
        :erlang.error(:badarg)
    end
  end

  def inverse_image(r, s)
      when elem(r, 0) === :Set and
             elem(s, 0) === :Set do
    case r_Set(r, :type) do
      {dT, rT} ->
        case match_types(rT, r_Set(s, :type)) do
          true ->
            nL = restrict(r_Set(s, :data), converse(r_Set(r, :data), []))
            r_Set(data: usort(nL), type: dT)

          false ->
            :erlang.error(:type_mismatch)
        end

      :_ ->
        r

      _ ->
        :erlang.error(:badarg)
    end
  end

  def strict_relation(r) when elem(r, 0) === :Set do
    case r_Set(r, :type) do
      type = {_, _} ->
        r_Set(data: strict(r_Set(r, :data), []), type: type)

      :_ ->
        r

      _ ->
        :erlang.error(:badarg)
    end
  end

  def weak_relation(r) when elem(r, 0) === :Set do
    case r_Set(r, :type) do
      {dT, rT} ->
        case unify_types(dT, rT) do
          [] ->
            :erlang.error(:badarg)

          type ->
            r_Set(data: weak(r_Set(r, :data)), type: {type, type})
        end

      :_ ->
        r

      _ ->
        :erlang.error(:badarg)
    end
  end

  def extension(r, s, e)
      when elem(r, 0) === :Set and
             elem(s, 0) === :Set do
    case {r_Set(r, :type), r_Set(s, :type), is_sofs_set(e)} do
      {t = {dT, rT}, sT, true} ->
        case :erlang.and(
               match_types(dT, sT),
               match_types(rT, type(e))
             ) do
          false ->
            :erlang.error(:type_mismatch)

          true ->
            rL = r_Set(r, :data)

            case extc([], r_Set(s, :data), to_external(e), rL) do
              [] ->
                r

              l ->
                r_Set(data: merge(rL, reverse(l)), type: t)
            end
        end

      {:_, :_, true} ->
        r

      {:_, sT, true} ->
        case type(e) do
          [:_] ->
            r

          eT ->
            r_Set(data: [], type: {sT, eT})
        end

      {_, _, true} ->
        :erlang.error(:badarg)
    end
  end

  def is_a_function(r) when elem(r, 0) === :Set do
    case r_Set(r, :type) do
      {_, _} ->
        case r_Set(r, :data) do
          [] ->
            true

          [{v, _} | es] ->
            is_a_func(es, v)
        end

      :_ ->
        true

      _ ->
        :erlang.error(:badarg)
    end
  end

  def restriction(relation, set) do
    restriction(1, relation, set)
  end

  def drestriction(relation, set) do
    drestriction(1, relation, set)
  end

  def composite(fn1, fn2)
      when elem(fn1, 0) === :Set and
             elem(fn2, 0) === :Set do
    {dTF1, rTF1} =
      case r_Set(fn1, :type) do
        {_, _} = f1T ->
          f1T

        :_ ->
          {:_, :_}

        _ ->
          :erlang.error(:badarg)
      end

    {dTF2, rTF2} =
      case r_Set(fn2, :type) do
        {_, _} = f2T ->
          f2T

        :_ ->
          {:_, :_}

        _ ->
          :erlang.error(:badarg)
      end

    case match_types(rTF1, dTF2) do
      true when dTF1 === :_ ->
        fn1

      true when dTF2 === :_ ->
        fn2

      true ->
        case comp(r_Set(fn1, :data), r_Set(fn2, :data)) do
          sL when is_list(sL) ->
            r_Set(data: sort(sL), type: {dTF1, rTF2})

          bad ->
            :erlang.error(bad)
        end

      false ->
        :erlang.error(:type_mismatch)
    end
  end

  def inverse(fn__) when elem(fn__, 0) === :Set do
    case r_Set(fn__, :type) do
      {dT, rT} ->
        case inverse1(r_Set(fn__, :data)) do
          sL when is_list(sL) ->
            r_Set(data: sL, type: {rT, dT})

          bad ->
            :erlang.error(bad)
        end

      :_ ->
        fn__

      _ ->
        :erlang.error(:badarg)
    end
  end

  def restriction(i, r, s)
      when is_integer(i) and
             elem(r, 0) === :Set and elem(s, 0) === :Set do
    rT = r_Set(r, :type)
    sT = r_Set(s, :type)

    case check_for_sort(rT, i) do
      :empty ->
        r

      :error ->
        :erlang.error(:badarg)

      sort ->
        rL = r_Set(r, :data)

        case {match_types(:erlang.element(i, rT), sT), r_Set(s, :data)} do
          {true, _SL} when rL === [] ->
            r

          {true, []} ->
            r_Set(data: [], type: rT)

          {true, [e | es]} when sort === false ->
            r_Set(data: reverse(restrict_n(i, rL, e, es, [])), type: rT)

          {true, [e | es]} ->
            r_Set(
              data: sort(restrict_n(i, keysort(i, rL), e, es, [])),
              type: rT
            )

          {false, _SL} ->
            :erlang.error(:type_mismatch)
        end
    end
  end

  def restriction(setFun, s1, s2)
      when elem(s1, 0) === :Set and
             elem(s2, 0) === :Set do
    type1 = r_Set(s1, :type)
    type2 = r_Set(s2, :type)
    sL1 = r_Set(s1, :data)

    case external_fun(setFun) do
      false when type2 === :_ ->
        s2

      false ->
        case subst(sL1, setFun, element_type(type1)) do
          {nSL, newType} ->
            case match_types(newType, type2) do
              true ->
                nL = sort(restrict(r_Set(s2, :data), converse(nSL, [])))
                r_Set(data: nL, type: type1)

              false ->
                :erlang.error(:type_mismatch)
            end

          bad ->
            :erlang.error(bad)
        end

      _ when type1 === :_ ->
        s1

      _XFun when is_list(type1) ->
        :erlang.error(:badarg)

      xFun ->
        funT = xFun.(type1)

        try do
          check_fun(type1, xFun, funT)
        catch
          _, _ ->
            :erlang.error(:badarg)
        else
          sort ->
            case match_types(funT, type2) do
              true ->
                r1 = inverse_substitution(sL1, xFun, sort)

                r_Set(
                  data: sort(sort, restrict(r_Set(s2, :data), r1)),
                  type: type1
                )

              false ->
                :erlang.error(:type_mismatch)
            end
        end
    end
  end

  def drestriction(i, r, s)
      when is_integer(i) and
             elem(r, 0) === :Set and elem(s, 0) === :Set do
    rT = r_Set(r, :type)
    sT = r_Set(s, :type)

    case check_for_sort(rT, i) do
      :empty ->
        r

      :error ->
        :erlang.error(:badarg)

      sort ->
        rL = r_Set(r, :data)

        case {match_types(:erlang.element(i, rT), sT), r_Set(s, :data)} do
          {true, []} ->
            r

          {true, _SL} when rL === [] ->
            r

          {true, [e | es]} when sort === false ->
            r_Set(data: diff_restrict_n(i, rL, e, es, []), type: rT)

          {true, [e | es]} ->
            r_Set(
              data: diff_restrict_n(i, keysort(i, rL), e, es, []),
              type: rT
            )

          {false, _SL} ->
            :erlang.error(:type_mismatch)
        end
    end
  end

  def drestriction(setFun, s1, s2)
      when elem(s1, 0) === :Set and
             elem(s2, 0) === :Set do
    type1 = r_Set(s1, :type)
    type2 = r_Set(s2, :type)
    sL1 = r_Set(s1, :data)

    case external_fun(setFun) do
      false when type2 === :_ ->
        s1

      false ->
        case subst(sL1, setFun, element_type(type1)) do
          {nSL, newType} ->
            case match_types(newType, type2) do
              true ->
                sL2 = r_Set(s2, :data)
                nL = sort(diff_restrict(sL2, converse(nSL, [])))
                r_Set(data: nL, type: type1)

              false ->
                :erlang.error(:type_mismatch)
            end

          bad ->
            :erlang.error(bad)
        end

      _ when type1 === :_ ->
        s1

      _XFun when is_list(type1) ->
        :erlang.error(:badarg)

      xFun ->
        funT = xFun.(type1)

        try do
          check_fun(type1, xFun, funT)
        catch
          _, _ ->
            :erlang.error(:badarg)
        else
          sort ->
            case match_types(funT, type2) do
              true ->
                r1 = inverse_substitution(sL1, xFun, sort)
                sL2 = r_Set(s2, :data)
                r_Set(data: sort(sort, diff_restrict(sL2, r1)), type: type1)

              false ->
                :erlang.error(:type_mismatch)
            end
        end
    end
  end

  def projection(i, set)
      when is_integer(i) and
             elem(set, 0) === :Set do
    type = r_Set(set, :type)

    case check_for_sort(type, i) do
      :empty ->
        set

      :error ->
        :erlang.error(:badarg)

      _ when i === 1 ->
        r_Set(
          data: projection1(r_Set(set, :data)),
          type: :erlang.element(i, type)
        )

      _ ->
        r_Set(
          data: projection_n(r_Set(set, :data), i, []),
          type: :erlang.element(i, type)
        )
    end
  end

  def projection(fun, set) do
    range(substitution(fun, set))
  end

  def substitution(i, set)
      when is_integer(i) and
             elem(set, 0) === :Set do
    type = r_Set(set, :type)

    case check_for_sort(type, i) do
      :empty ->
        set

      :error ->
        :erlang.error(:badarg)

      _Sort ->
        nType = :erlang.element(i, type)
        nSL = substitute_element(r_Set(set, :data), i, [])
        r_Set(data: nSL, type: {type, nType})
    end
  end

  def substitution(setFun, set) when elem(set, 0) === :Set do
    type = r_Set(set, :type)
    l = r_Set(set, :data)

    case external_fun(setFun) do
      false when l !== [] ->
        case subst(l, setFun, element_type(type)) do
          {sL, newType} ->
            r_Set(data: reverse(sL), type: {type, newType})

          bad ->
            :erlang.error(bad)
        end

      false ->
        empty_set()

      _ when type === :_ ->
        empty_set()

      _XFun when is_list(type) ->
        :erlang.error(:badarg)

      xFun ->
        funT = xFun.(type)

        try do
          check_fun(type, xFun, funT)
        catch
          _, _ ->
            :erlang.error(:badarg)
        else
          _Sort ->
            sL = substitute(l, xFun, [])
            r_Set(data: sL, type: {type, funT})
        end
    end
  end

  def partition(sets) do
    f1 = relation_to_family(canonical_relation(sets))
    f2 = relation_to_family(converse(f1))
    range(f2)
  end

  def partition(i, set)
      when is_integer(i) and
             elem(set, 0) === :Set do
    type = r_Set(set, :type)

    case check_for_sort(type, i) do
      :empty ->
        set

      :error ->
        :erlang.error(:badarg)

      false ->
        r_Set(data: partition_n(i, r_Set(set, :data)), type: [type])

      true ->
        r_Set(
          data: partition_n(i, keysort(i, r_Set(set, :data))),
          type: [type]
        )
    end
  end

  def partition(fun, set) do
    range(partition_family(fun, set))
  end

  def partition(i, r, s)
      when is_integer(i) and
             elem(r, 0) === :Set and elem(s, 0) === :Set do
    rT = r_Set(r, :type)
    sT = r_Set(s, :type)

    case check_for_sort(rT, i) do
      :empty ->
        {r, r}

      :error ->
        :erlang.error(:badarg)

      sort ->
        rL = r_Set(r, :data)

        case {match_types(:erlang.element(i, rT), sT), r_Set(s, :data)} do
          {true, _SL} when rL === [] ->
            {r, r}

          {true, []} ->
            {r_Set(data: [], type: rT), r}

          {true, [e | es]} when sort === false ->
            [l1 | l2] = partition3_n(i, rL, e, es, [], [])
            {r_Set(data: l1, type: rT), r_Set(data: l2, type: rT)}

          {true, [e | es]} ->
            [l1 | l2] = partition3_n(i, keysort(i, rL), e, es, [], [])
            {r_Set(data: l1, type: rT), r_Set(data: l2, type: rT)}

          {false, _SL} ->
            :erlang.error(:type_mismatch)
        end
    end
  end

  def partition(setFun, s1, s2)
      when elem(s1, 0) === :Set and
             elem(s2, 0) === :Set do
    type1 = r_Set(s1, :type)
    type2 = r_Set(s2, :type)
    sL1 = r_Set(s1, :data)

    case external_fun(setFun) do
      false when type2 === :_ ->
        {s2, s1}

      false ->
        case subst(sL1, setFun, element_type(type1)) do
          {nSL, newType} ->
            case match_types(newType, type2) do
              true ->
                r1 = converse(nSL, [])
                [l1 | l2] = partition3(r_Set(s2, :data), r1)
                {r_Set(data: sort(l1), type: type1), r_Set(data: sort(l2), type: type1)}

              false ->
                :erlang.error(:type_mismatch)
            end

          bad ->
            :erlang.error(bad)
        end

      _ when type1 === :_ ->
        {s1, s1}

      _XFun when is_list(type1) ->
        :erlang.error(:badarg)

      xFun ->
        funT = xFun.(type1)

        try do
          check_fun(type1, xFun, funT)
        catch
          _, _ ->
            :erlang.error(:badarg)
        else
          sort ->
            case match_types(funT, type2) do
              true ->
                r1 = inverse_substitution(sL1, xFun, sort)
                [l1 | l2] = partition3(r_Set(s2, :data), r1)
                {r_Set(data: sort(l1), type: type1), r_Set(data: sort(l2), type: type1)}

              false ->
                :erlang.error(:type_mismatch)
            end
        end
    end
  end

  def multiple_relative_product(t, r)
      when is_tuple(t) and
             elem(r, 0) === :Set do
    case test_rel(r, tuple_size(t), :eq) do
      true when r_Set(r, :type) === :_ ->
        empty_set()

      true ->
        mProd = mul_relprod(:erlang.tuple_to_list(t), 1, r)
        relative_product(mProd)

      false ->
        :erlang.error(:badarg)
    end
  end

  def join(r1, i1, r2, i2)
      when elem(r1, 0) === :Set and
             elem(r2, 0) === :Set and is_integer(i1) and
             is_integer(i2) do
    case :erlang.and(
           test_rel(r1, i1, :lte),
           test_rel(r2, i2, :lte)
         ) do
      false ->
        :erlang.error(:badarg)

      true when r_Set(r1, :type) === :_ ->
        r1

      true when r_Set(r2, :type) === :_ ->
        r2

      true ->
        l1 = r_Set(raise_element(r1, i1), :data)
        l2 = r_Set(raise_element(r2, i2), :data)
        t = relprod1(l1, l2)

        f =
          case :erlang.and(i1 === 1, i2 === 1) do
            true ->
              fn {x, y} ->
                join_element(x, y)
              end

            false ->
              fn {x, y} ->
                :erlang.list_to_tuple(join_element(x, y, i2))
              end
          end

        r_Set(
          data: replace(t, f, []),
          type: f.({r_Set(r1, :type), r_Set(r2, :type)})
        )
    end
  end

  defp test_rel(r, i, c) do
    case r_Set(r, :type) do
      rel
      when is_tuple(rel) and c === :eq and
             i === tuple_size(rel) ->
        true

      rel
      when is_tuple(rel) and c === :lte and i >= 1 and
             i <= tuple_size(rel) ->
        true

      :_ ->
        true

      _ ->
        false
    end
  end

  def fam2rel(f) do
    family_to_relation(f)
  end

  def family_to_relation(f) when elem(f, 0) === :Set do
    case r_Set(f, :type) do
      {dT, [rT]} ->
        r_Set(data: family2rel(r_Set(f, :data), []), type: {dT, rT})

      :_ ->
        f

      _ ->
        :erlang.error(:badarg)
    end
  end

  def family_specification(fun, f) when elem(f, 0) === :Set do
    case r_Set(f, :type) do
      {_DT, [type]} = fType ->
        r =
          case external_fun(fun) do
            false ->
              fam_spec(r_Set(f, :data), fun, type, [])

            xFun ->
              fam_specification(r_Set(f, :data), xFun, [])
          end

        case r do
          sL when is_list(sL) ->
            r_Set(data: sL, type: fType)

          bad ->
            :erlang.error(bad)
        end

      :_ ->
        f

      _ ->
        :erlang.error(:badarg)
    end
  end

  def union_of_family(f) when elem(f, 0) === :Set do
    case r_Set(f, :type) do
      {_DT, [type]} ->
        r_Set(data: un_of_fam(r_Set(f, :data), []), type: type)

      :_ ->
        f

      _ ->
        :erlang.error(:badarg)
    end
  end

  def intersection_of_family(f) when elem(f, 0) === :Set do
    case r_Set(f, :type) do
      {_DT, [type]} ->
        case int_of_fam(r_Set(f, :data)) do
          fU when is_list(fU) ->
            r_Set(data: fU, type: type)

          bad ->
            :erlang.error(bad)
        end

      _ ->
        :erlang.error(:badarg)
    end
  end

  def family_union(f) when elem(f, 0) === :Set do
    case r_Set(f, :type) do
      {dT, [[type]]} ->
        r_Set(data: fam_un(r_Set(f, :data), []), type: {dT, [type]})

      :_ ->
        f

      _ ->
        :erlang.error(:badarg)
    end
  end

  def family_intersection(f) when elem(f, 0) === :Set do
    case r_Set(f, :type) do
      {dT, [[type]]} ->
        case fam_int(r_Set(f, :data), []) do
          fU when is_list(fU) ->
            r_Set(data: fU, type: {dT, [type]})

          bad ->
            :erlang.error(bad)
        end

      :_ ->
        f

      _ ->
        :erlang.error(:badarg)
    end
  end

  def family_domain(f) when elem(f, 0) === :Set do
    case r_Set(f, :type) do
      {fDT, [{dT, _}]} ->
        r_Set(data: fam_dom(r_Set(f, :data), []), type: {fDT, [dT]})

      :_ ->
        f

      {_, [:_]} ->
        f

      _ ->
        :erlang.error(:badarg)
    end
  end

  def family_range(f) when elem(f, 0) === :Set do
    case r_Set(f, :type) do
      {dT, [{_, rT}]} ->
        r_Set(data: fam_ran(r_Set(f, :data), []), type: {dT, [rT]})

      :_ ->
        f

      {_, [:_]} ->
        f

      _ ->
        :erlang.error(:badarg)
    end
  end

  def family_field(f) do
    family_union(family_domain(f), family_range(f))
  end

  def family_union(f1, f2) do
    fam_binop(f1, f2, &fam_union/3)
  end

  def family_intersection(f1, f2) do
    fam_binop(f1, f2, &fam_intersect/3)
  end

  def family_difference(f1, f2) do
    fam_binop(f1, f2, &fam_difference/3)
  end

  defp fam_binop(f1, f2, fF)
       when elem(f1, 0) === :Set and
              elem(f2, 0) === :Set do
    case unify_types(r_Set(f1, :type), r_Set(f2, :type)) do
      [] ->
        :erlang.error(:type_mismatch)

      :_ ->
        f1

      type = {_, [_]} ->
        r_Set(data: fF.(r_Set(f1, :data), r_Set(f2, :data), []), type: type)

      _ ->
        :erlang.error(:badarg)
    end
  end

  def partition_family(i, set)
      when is_integer(i) and
             elem(set, 0) === :Set do
    type = r_Set(set, :type)

    case check_for_sort(type, i) do
      :empty ->
        set

      :error ->
        :erlang.error(:badarg)

      false ->
        r_Set(
          data: fam_partition_n(i, r_Set(set, :data)),
          type: {:erlang.element(i, type), [type]}
        )

      true ->
        r_Set(
          data: fam_partition_n(i, keysort(i, r_Set(set, :data))),
          type: {:erlang.element(i, type), [type]}
        )
    end
  end

  def partition_family(setFun, set) when elem(set, 0) === :Set do
    type = r_Set(set, :type)
    sL = r_Set(set, :data)

    case external_fun(setFun) do
      false when sL !== [] ->
        case subst(sL, setFun, element_type(type)) do
          {nSL, newType} ->
            p = fam_partition(converse(nSL, []), true)
            r_Set(data: reverse(p), type: {newType, [type]})

          bad ->
            :erlang.error(bad)
        end

      false ->
        empty_set()

      _ when type === :_ ->
        empty_set()

      _XFun when is_list(type) ->
        :erlang.error(:badarg)

      xFun ->
        dType = xFun.(type)

        try do
          check_fun(type, xFun, dType)
        catch
          _, _ ->
            :erlang.error(:badarg)
        else
          sort ->
            ts = inverse_substitution(r_Set(set, :data), xFun, sort)
            p = fam_partition(ts, sort)
            r_Set(data: reverse(p), type: {dType, [type]})
        end
    end
  end

  def family_projection(setFun, f) when elem(f, 0) === :Set do
    case r_Set(f, :type) do
      {_, [_]} when [] === r_Set(f, :data) ->
        empty_set()

      {dT, [type]} ->
        case external_fun(setFun) do
          false ->
            case fam_proj(r_Set(f, :data), setFun, type, :_, []) do
              {sL, newType} ->
                r_Set(data: sL, type: {dT, newType})

              bad ->
                :erlang.error(bad)
            end

          _ ->
            :erlang.error(:badarg)
        end

      :_ ->
        f

      _ ->
        :erlang.error(:badarg)
    end
  end

  def family_to_digraph(f) when elem(f, 0) === :Set do
    case r_Set(f, :type) do
      {_, [_]} ->
        fam2digraph(f, :digraph.new())

      :_ ->
        :digraph.new()

      _Else ->
        :erlang.error(:badarg)
    end
  end

  def family_to_digraph(f, type) when elem(f, 0) === :Set do
    case r_Set(f, :type) do
      {_, [_]} ->
        :ok

      :_ ->
        :ok

      _Else ->
        :erlang.error(:badarg)
    end

    try do
      :digraph.new(type)
    catch
      :error, :badarg ->
        :erlang.error(:badarg)
    else
      g ->
        case (try do
                fam2digraph(f, g)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          {:error, reason} ->
            true = :digraph.delete(g)
            :erlang.error(reason)

          _ ->
            g
        end
    end
  end

  def digraph_to_family(g) do
    try do
      digraph_family(g)
    catch
      _, _ ->
        :erlang.error(:badarg)
    else
      l ->
        r_Set(data: l, type: {:atom, [:atom]})
    end
  end

  def digraph_to_family(g, t) do
    case {is_type(t), t} do
      {true, [{_, [_]} = type]} ->
        try do
          digraph_family(g)
        catch
          _, _ ->
            :erlang.error(:badarg)
        else
          l ->
            r_Set(data: l, type: type)
        end

      _ ->
        :erlang.error(:badarg)
    end
  end

  defp is_types(0, _T) do
    true
  end

  defp is_types(i, t) do
    case is_type(:erlang.element(i, t)) do
      true ->
        is_types(i - 1, t)

      false ->
        false
    end
  end

  defp is_element_type(:_) do
    true
  end

  defp is_element_type(t) do
    is_type(t)
  end

  defp set_of_sets([s | ss], l, t0) when elem(s, 0) === :Set do
    case unify_types([r_Set(s, :type)], t0) do
      [] ->
        {:error, :type_mismatch}

      type ->
        set_of_sets(ss, [r_Set(s, :data) | l], type)
    end
  end

  defp set_of_sets([s | ss], l, t0) when elem(s, 0) === :OrdSet do
    case unify_types(r_OrdSet(s, :ordtype), t0) do
      [] ->
        {:error, :type_mismatch}

      type ->
        set_of_sets(ss, [r_OrdSet(s, :orddata) | l], type)
    end
  end

  defp set_of_sets([], l, t) do
    r_Set(data: usort(l), type: t)
  end

  defp set_of_sets(_, _L, _T) do
    {:error, :badarg}
  end

  defp ordset_of_sets([s | ss], l, t) when elem(s, 0) === :Set do
    ordset_of_sets(ss, [r_Set(s, :data) | l], [[r_Set(s, :type)] | t])
  end

  defp ordset_of_sets([s | ss], l, t) when elem(s, 0) === :OrdSet do
    ordset_of_sets(ss, [r_OrdSet(s, :orddata) | l], [r_OrdSet(s, :ordtype) | t])
  end

  defp ordset_of_sets([], l, t) do
    r_OrdSet(
      orddata: :erlang.list_to_tuple(reverse(l)),
      ordtype: :erlang.list_to_tuple(reverse(t))
    )
  end

  defp ordset_of_sets(_, _L, _T) do
    :error
  end

  defp rel(ts, [type]) do
    case :erlang.and(
           is_type(type),
           atoms_only(type, 1)
         ) do
      true ->
        rel(ts, tuple_size(type), type)

      false ->
        rel_type(ts, [], type)
    end
  end

  defp rel(ts, sz) do
    rel(ts, sz, :erlang.make_tuple(sz, :atom))
  end

  defp atoms_only(type, i)
       when is_atom(
              :erlang.element(
                i,
                type
              )
            ) do
    atoms_only(type, i + 1)
  end

  defp atoms_only(type, i)
       when i > tuple_size(type) and
              is_tuple(type) do
    true
  end

  defp atoms_only(_Type, _I) do
    false
  end

  defp rel(ts, sz, type) when sz >= 1 do
    sL = usort(ts)
    rel(sL, sL, sz, type)
  end

  defp rel([t | ts], l, sz, type)
       when tuple_size(t) === sz do
    rel(ts, l, sz, type)
  end

  defp rel([], l, _Sz, type) do
    r_Set(data: l, type: type)
  end

  defp rel_type([e | ts], l, type) do
    {nType, nE} = make_element(e, type, type)
    rel_type(ts, [nE | l], nType)
  end

  defp rel_type([], [], :_) do
    empty_set()
  end

  defp rel_type([], sL, type) when is_tuple(type) do
    r_Set(data: usort(sL), type: type)
  end

  defp a_func(ts, t) do
    case {t, is_type(t)} do
      {[{dT, rT} = type], true}
      when is_atom(dT) and
             is_atom(rT) ->
        func(ts, type)

      {[type], true} ->
        func_type(ts, [], type, fn {_, _} ->
          true
        end)
    end
  end

  defp func(l0, type) do
    l = usort(l0)
    func(l, l, l, type)
  end

  defp func([{x, _} | ts], x0, l, type) when x != x0 do
    func(ts, x, l, type)
  end

  defp func([{x, _} | _Ts], x0, _L, _Type) when x == x0 do
    :bad_function
  end

  defp func([], _X0, l, type) do
    r_Set(data: l, type: type)
  end

  defp fam(ts, t) do
    case {t, is_type(t)} do
      {[{dT, [rT]} = type], true}
      when is_atom(dT) and
             is_atom(rT) ->
        fam2(ts, type)

      {[type], true} ->
        func_type(ts, [], type, fn {_, [_]} ->
          true
        end)
    end
  end

  defp fam2([], type) do
    r_Set(data: [], type: type)
  end

  defp fam2(ts, type) do
    fam2(sort(ts), ts, [], type)
  end

  defp fam2([{i, l} | t], i0, sL, type) when i != i0 do
    fam2(t, i, [{i, usort(l)} | sL], type)
  end

  defp fam2([{i, l} | t], i0, sL, type) when i == i0 do
    case {usort(l), sL} do
      {nL, [{_I, nL1} | _]} when nL == nL1 ->
        fam2(t, i0, sL, type)

      _ ->
        :bad_function
    end
  end

  defp fam2([], _I0, sL, type) do
    r_Set(data: reverse(sL), type: type)
  end

  defp func_type([e | t], sL, type, f) do
    {nType, nE} = make_element(e, type, type)
    func_type(t, [nE | sL], nType, f)
  end

  defp func_type([], [], :_, _F) do
    empty_set()
  end

  defp func_type([], sL, type, f) do
    true = f.(type)
    nL = usort(sL)
    check_function(nL, r_Set(data: nL, type: type))
  end

  defp setify(l, [atom])
       when is_atom(atom) and
              atom !== :_ do
    r_Set(data: usort(l), type: atom)
  end

  defp setify(l, [type0]) do
    try do
      is_no_lists(type0)
    catch
      _, _ ->
        {[type], set} = create(l, type0, type0, [])
        r_Set(data: set, type: type)
    else
      n when is_integer(n) ->
        rel(l, n, type0)

      sizes ->
        make_oset(l, sizes, l, type0)
    end
  end

  defp setify(e, type0) do
    {type, ordSet} = make_element(e, type0, type0)
    r_OrdSet(orddata: ordSet, ordtype: type)
  end

  defp is_no_lists(t) when is_tuple(t) do
    sz = tuple_size(t)
    is_no_lists(t, sz, sz, [])
  end

  defp is_no_lists(_T, 0, sz, []) do
    sz
  end

  defp is_no_lists(_T, 0, sz, l) do
    {sz, l}
  end

  defp is_no_lists(t, i, sz, l)
       when is_atom(
              :erlang.element(
                i,
                t
              )
            ) do
    is_no_lists(t, i - 1, sz, l)
  end

  defp is_no_lists(t, i, sz, l) do
    is_no_lists(t, i - 1, sz, [{i, is_no_lists(:erlang.element(i, t))} | l])
  end

  defp create([e | es], t, t0, l) do
    {nT, s} = make_element(e, t, t0)
    create(es, nT, t0, [s | l])
  end

  defp create([], t, _T0, l) do
    {[t], usort(l)}
  end

  defp make_element(c, :_, _T0) do
    make_element(c)
  end

  defp make_element(c, atom, :_)
       when is_atom(atom) and
              not is_list(c) and not is_tuple(c) do
    {atom, c}
  end

  defp make_element(c, atom, atom) when is_atom(atom) do
    {atom, c}
  end

  defp make_element(t, tT, :_)
       when tuple_size(t) === tuple_size(tT) do
    make_tuple(:erlang.tuple_to_list(t), :erlang.tuple_to_list(tT), [], [], :_)
  end

  defp make_element(t, tT, t0)
       when tuple_size(t) === tuple_size(tT) do
    make_tuple(
      :erlang.tuple_to_list(t),
      :erlang.tuple_to_list(tT),
      [],
      [],
      :erlang.tuple_to_list(t0)
    )
  end

  defp make_element(l, [lT], :_) when is_list(l) do
    create(l, lT, :_, [])
  end

  defp make_element(l, [lT], [t0]) when is_list(l) do
    create(l, lT, t0, [])
  end

  defp make_tuple([e | es], [t | ts], nT, l, t0) when t0 === :_ do
    {eT, eS} = make_element(e, t, t0)
    make_tuple(es, ts, [eT | nT], [eS | l], t0)
  end

  defp make_tuple([e | es], [t | ts], nT, l, [t0 | t0s]) do
    {eT, eS} = make_element(e, t, t0)
    make_tuple(es, ts, [eT | nT], [eS | l], t0s)
  end

  defp make_tuple([], [], nT, l, _T0s) when nT !== [] do
    {:erlang.list_to_tuple(reverse(nT)), :erlang.list_to_tuple(reverse(l))}
  end

  defp make_element(c) when not is_list(c) and not is_tuple(c) do
    {:atom, c}
  end

  defp make_element(t) when is_tuple(t) do
    make_tuple(:erlang.tuple_to_list(t), [], [])
  end

  defp make_element(l) when is_list(l) do
    create(l, :_, :_, [])
  end

  defp make_tuple([e | es], t, l) do
    {eT, eS} = make_element(e)
    make_tuple(es, [eT | t], [eS | l])
  end

  defp make_tuple([], t, l) when t !== [] do
    {:erlang.list_to_tuple(reverse(t)), :erlang.list_to_tuple(reverse(l))}
  end

  defp make_oset([t | ts], szs, l, type) do
    true = test_oset(szs, t, t)
    make_oset(ts, szs, l, type)
  end

  defp make_oset([], _Szs, l, type) do
    r_Set(data: usort(l), type: type)
  end

  defp test_oset({sz, args}, t, t0) when tuple_size(t) === sz do
    test_oset_args(args, t, t0)
  end

  defp test_oset(sz, t, _T0) when tuple_size(t) === sz do
    true
  end

  defp test_oset_args([{arg, szs} | ss], t, t0) do
    true = test_oset(szs, :erlang.element(arg, t), t0)
    test_oset_args(ss, t, t0)
  end

  defp test_oset_args([], _T, _T0) do
    true
  end

  defp list_of_sets([s | ss], type, l) do
    list_of_sets(ss, type, [r_Set(data: s, type: type) | l])
  end

  defp list_of_sets([], _Type, l) do
    reverse(l)
  end

  defp list_of_ordsets([s | ss], type, l) do
    list_of_ordsets(ss, type, [r_OrdSet(orddata: s, ordtype: type) | l])
  end

  defp list_of_ordsets([], _Type, l) do
    reverse(l)
  end

  defp tuple_of_sets([s | ss], [[type] | types], l) do
    tuple_of_sets(ss, types, [r_Set(data: s, type: type) | l])
  end

  defp tuple_of_sets([s | ss], [type | types], l) do
    tuple_of_sets(ss, types, [r_OrdSet(orddata: s, ordtype: type) | l])
  end

  defp tuple_of_sets([], [], l) do
    :erlang.list_to_tuple(reverse(l))
  end

  defp spec([e | es], fun, type, l) do
    case fun.(term2set(e, type)) do
      true ->
        spec(es, fun, type, [e | l])

      false ->
        spec(es, fun, type, l)

      _ ->
        :badarg
    end
  end

  defp spec([], _Fun, _Type, l) do
    reverse(l)
  end

  defp specification([e | es], fun, l) do
    case fun.(e) do
      true ->
        specification(es, fun, [e | l])

      false ->
        specification(es, fun, l)

      _ ->
        :badarg
    end
  end

  defp specification([], _Fun, l) do
    reverse(l)
  end

  defp intersection([h1 | t1], [h2 | t2], l) when h1 < h2 do
    intersection1(t1, t2, l, h2)
  end

  defp intersection([h1 | t1], [h2 | t2], l) when h1 == h2 do
    intersection(t1, t2, [h1 | l])
  end

  defp intersection([h1 | t1], [_H2 | t2], l) do
    intersection2(t1, t2, l, h1)
  end

  defp intersection(_, _, l) do
    reverse(l)
  end

  defp intersection1([h1 | t1], t2, l, h2) when h1 < h2 do
    intersection1(t1, t2, l, h2)
  end

  defp intersection1([h1 | t1], t2, l, h2) when h1 == h2 do
    intersection(t1, t2, [h1 | l])
  end

  defp intersection1([h1 | t1], t2, l, _H2) do
    intersection2(t1, t2, l, h1)
  end

  defp intersection1(_, _, l, _) do
    reverse(l)
  end

  defp intersection2(t1, [h2 | t2], l, h1) when h1 > h2 do
    intersection2(t1, t2, l, h1)
  end

  defp intersection2(t1, [h2 | t2], l, h1) when h1 == h2 do
    intersection(t1, t2, [h1 | l])
  end

  defp intersection2(t1, [h2 | t2], l, _H1) do
    intersection1(t1, t2, l, h2)
  end

  defp intersection2(_, _, l, _) do
    reverse(l)
  end

  defp difference([h1 | t1], [h2 | t2], l) when h1 < h2 do
    diff(t1, t2, [h1 | l], h2)
  end

  defp difference([h1 | t1], [h2 | t2], l) when h1 == h2 do
    difference(t1, t2, l)
  end

  defp difference([h1 | t1], [_H2 | t2], l) do
    diff2(t1, t2, l, h1)
  end

  defp difference(l1, _, l) do
    reverse(l, l1)
  end

  defp diff([h1 | t1], t2, l, h2) when h1 < h2 do
    diff(t1, t2, [h1 | l], h2)
  end

  defp diff([h1 | t1], t2, l, h2) when h1 == h2 do
    difference(t1, t2, l)
  end

  defp diff([h1 | t1], t2, l, _H2) do
    diff2(t1, t2, l, h1)
  end

  defp diff(_, _, l, _) do
    reverse(l)
  end

  defp diff2(t1, [h2 | t2], l, h1) when h1 > h2 do
    diff2(t1, t2, l, h1)
  end

  defp diff2(t1, [h2 | t2], l, h1) when h1 == h2 do
    difference(t1, t2, l)
  end

  defp diff2(t1, [h2 | t2], l, h1) do
    diff(t1, t2, [h1 | l], h2)
  end

  defp diff2(t1, _, l, h1) do
    reverse(l, [h1 | t1])
  end

  defp symdiff([h1 | t1], t2, l) do
    symdiff2(t1, t2, l, h1)
  end

  defp symdiff(_, t2, l) do
    reverse(l, t2)
  end

  defp symdiff1([h1 | t1], t2, l, h2) when h1 < h2 do
    symdiff1(t1, t2, [h1 | l], h2)
  end

  defp symdiff1([h1 | t1], t2, l, h2) when h1 == h2 do
    symdiff(t1, t2, l)
  end

  defp symdiff1([h1 | t1], t2, l, h2) do
    symdiff2(t1, t2, [h2 | l], h1)
  end

  defp symdiff1(_, t2, l, h2) do
    reverse(l, [h2 | t2])
  end

  defp symdiff2(t1, [h2 | t2], l, h1) when h1 > h2 do
    symdiff2(t1, t2, [h2 | l], h1)
  end

  defp symdiff2(t1, [h2 | t2], l, h1) when h1 == h2 do
    symdiff(t1, t2, l)
  end

  defp symdiff2(t1, [h2 | t2], l, h1) do
    symdiff1(t1, t2, [h1 | l], h2)
  end

  defp symdiff2(t1, _, l, h1) do
    reverse(l, [h1 | t1])
  end

  defp sympart([h1 | t1], [h2 | t2], l1, l12, l2, t)
       when h1 < h2 do
    sympart1(t1, t2, [h1 | l1], l12, l2, t, h2)
  end

  defp sympart([h1 | t1], [h2 | t2], l1, l12, l2, t)
       when h1 == h2 do
    sympart(t1, t2, l1, [h1 | l12], l2, t)
  end

  defp sympart([h1 | t1], [h2 | t2], l1, l12, l2, t) do
    sympart2(t1, t2, l1, l12, [h2 | l2], t, h1)
  end

  defp sympart(s1, [], l1, l12, l2, t) do
    {r_Set(data: reverse(l1, s1), type: t), r_Set(data: reverse(l12), type: t),
     r_Set(data: reverse(l2), type: t)}
  end

  defp sympart(_, s2, l1, l12, l2, t) do
    {r_Set(data: reverse(l1), type: t), r_Set(data: reverse(l12), type: t),
     r_Set(data: reverse(l2, s2), type: t)}
  end

  defp sympart1([h1 | t1], t2, l1, l12, l2, t, h2)
       when h1 < h2 do
    sympart1(t1, t2, [h1 | l1], l12, l2, t, h2)
  end

  defp sympart1([h1 | t1], t2, l1, l12, l2, t, h2)
       when h1 == h2 do
    sympart(t1, t2, l1, [h1 | l12], l2, t)
  end

  defp sympart1([h1 | t1], t2, l1, l12, l2, t, h2) do
    sympart2(t1, t2, l1, l12, [h2 | l2], t, h1)
  end

  defp sympart1(_, t2, l1, l12, l2, t, h2) do
    {r_Set(data: reverse(l1), type: t), r_Set(data: reverse(l12), type: t),
     r_Set(data: reverse(l2, [h2 | t2]), type: t)}
  end

  defp sympart2(t1, [h2 | t2], l1, l12, l2, t, h1)
       when h1 > h2 do
    sympart2(t1, t2, l1, l12, [h2 | l2], t, h1)
  end

  defp sympart2(t1, [h2 | t2], l1, l12, l2, t, h1)
       when h1 == h2 do
    sympart(t1, t2, l1, [h1 | l12], l2, t)
  end

  defp sympart2(t1, [h2 | t2], l1, l12, l2, t, h1) do
    sympart1(t1, t2, [h1 | l1], l12, l2, t, h2)
  end

  defp sympart2(t1, _, l1, l12, l2, t, h1) do
    {r_Set(data: reverse(l1, [h1 | t1]), type: t), r_Set(data: reverse(l12), type: t),
     r_Set(data: reverse(l2), type: t)}
  end

  defp prod([[e | es] | xs], t, l) do
    prod(es, xs, t, prod(xs, [e | t], l))
  end

  defp prod([], t, l) do
    [:erlang.list_to_tuple(reverse(t)) | l]
  end

  defp prod([e | es], xs, t, l) do
    prod(es, xs, t, prod(xs, [e | t], l))
  end

  defp prod([], _Xs, _E, l) do
    l
  end

  defp constant_function([e | es], x, l) do
    constant_function(es, x, [{e, x} | l])
  end

  defp constant_function([], _X, l) do
    reverse(l)
  end

  defp subset([h1 | t1], [h2 | t2]) when h1 > h2 do
    subset(t1, t2, h1)
  end

  defp subset([h1 | t1], [h2 | t2]) when h1 == h2 do
    subset(t1, t2)
  end

  defp subset(l1, _) do
    l1 === []
  end

  defp subset(t1, [h2 | t2], h1) when h1 > h2 do
    subset(t1, t2, h1)
  end

  defp subset(t1, [h2 | t2], h1) when h1 == h2 do
    subset(t1, t2)
  end

  defp subset(_, _, _) do
    false
  end

  defp disjoint([b | bs], a, as) when a < b do
    disjoint(as, b, bs)
  end

  defp disjoint([b | _Bs], a, _As) when a == b do
    false
  end

  defp disjoint([_B | bs], a, as) do
    disjoint(bs, a, as)
  end

  defp disjoint(_Bs, _A, _As) do
    true
  end

  defp lunion([[_] = s]) do
    s
  end

  defp lunion([[] | ls]) do
    lunion(ls)
  end

  defp lunion([s | ss]) do
    umerge(lunion(ss, last(s), [s], []))
  end

  defp lunion([]) do
    []
  end

  defp lunion([[e] = s | ss], last, sL, ls) when e > last do
    lunion(ss, e, [s | sL], ls)
  end

  defp lunion([s | ss], last, sL, ls) when hd(s) > last do
    lunion(ss, last(s), [s | sL], ls)
  end

  defp lunion([s | ss], _Last, sL, ls) do
    lunion(ss, last(s), [s], [append(reverse(sL)) | ls])
  end

  defp lunion([], _Last, sL, ls) do
    [append(reverse(sL)) | ls]
  end

  defp lintersection(_, []) do
    []
  end

  defp lintersection([s | ss], s0) do
    lintersection(ss, intersection(s, s0, []))
  end

  defp lintersection([], s) do
    s
  end

  defp can_rel([s | ss], l) do
    can_rel(ss, l, s, s)
  end

  defp can_rel([], l) do
    sort(l)
  end

  defp can_rel(ss, l, [e | es], s) do
    can_rel(ss, [{e, s} | l], es, s)
  end

  defp can_rel(ss, l, _, _S) do
    can_rel(ss, l)
  end

  defp rel2family([{x, y} | s]) do
    rel2fam(s, x, [y], [])
  end

  defp rel2family([]) do
    []
  end

  defp rel2fam([{x, y} | s], x0, yL, l) when x0 == x do
    rel2fam(s, x0, [y | yL], l)
  end

  defp rel2fam([{x, y} | s], x0, [a, b | yL], l) do
    rel2fam(s, x, [y], [{x0, reverse(yL, [b, a])} | l])
  end

  defp rel2fam([{x, y} | s], x0, yL, l) do
    rel2fam(s, x, [y], [{x0, yL} | l])
  end

  defp rel2fam([], x, yL, l) do
    reverse([{x, reverse(yL)} | l])
  end

  defp dom([{x, _} | es]) do
    dom([], x, es)
  end

  defp dom([] = l) do
    l
  end

  defp dom(l, x, [{x1, _} | es]) when x == x1 do
    dom(l, x, es)
  end

  defp dom(l, x, [{y, _} | es]) do
    dom([x | l], y, es)
  end

  defp dom(l, x, []) do
    reverse(l, [x])
  end

  defp ran([{_, y} | es], l) do
    ran(es, [y | l])
  end

  defp ran([], l) do
    usort(l)
  end

  defp relprod(a, b) do
    usort(relprod1(a, b))
  end

  defp relprod1([{ay, ax} | a], b) do
    relprod1(b, ay, ax, a, [])
  end

  defp relprod1(_A, _B) do
    []
  end

  defp relprod1([{bx, _By} | b], ay, ax, a, l) when ay > bx do
    relprod1(b, ay, ax, a, l)
  end

  defp relprod1([{bx, by} | b], ay, ax, a, l) when ay == bx do
    relprod(b, bx, by, a, [{ax, by} | l], ax, b, ay)
  end

  defp relprod1([{bx, by} | b], _Ay, _Ax, a, l) do
    relprod2(b, bx, by, a, l)
  end

  defp relprod1(_B, _Ay, _Ax, _A, l) do
    l
  end

  defp relprod2(b, bx, by, [{ay, _Ax} | a], l) when ay < bx do
    relprod2(b, bx, by, a, l)
  end

  defp relprod2(b, bx, by, [{ay, ax} | a], l) when ay == bx do
    relprod(b, bx, by, a, [{ax, by} | l], ax, b, ay)
  end

  defp relprod2(b, _Bx, _By, [{ay, ax} | a], l) do
    relprod1(b, ay, ax, a, l)
  end

  defp relprod2(_, _, _, _, l) do
    l
  end

  defp relprod(b0, bx0, by0, a0, l, ax, [{bx, by} | b], ay)
       when ay == bx do
    relprod(b0, bx0, by0, a0, [{ax, by} | l], ax, b, ay)
  end

  defp relprod(b0, bx0, by0, a0, l, _Ax, _B, _Ay) do
    relprod2(b0, bx0, by0, a0, l)
  end

  defp relprod_n([], _R, _EmptyG, _IsR) do
    {:error, :badarg}
  end

  defp relprod_n(rL, r, emptyR, isR) do
    case domain_type(rL, :_) do
      error = {:error, _Reason} ->
        error

      dType ->
        empty = :erlang.or(any(&is_empty_set/1, rL), emptyR)
        rType = range_type(rL, [])
        type = {dType, rType}

        prod =
          case empty do
            true when dType === :_ or rType === :_ ->
              empty_set()

            true ->
              r_Set(data: [], type: type)

            false ->
              tL = r_Set(relprod_n(rL), :data)
              sz = length(rL)

              fun = fn {x, a} ->
                {x, flat(sz, a, [])}
              end

              r_Set(data: map(fun, tL), type: type)
          end

        case isR do
          true ->
            relative_product(prod, r)

          false ->
            prod
        end
    end
  end

  defp relprod_n([r | rs]) do
    relprod_n(rs, r)
  end

  defp relprod_n([], r) do
    r
  end

  defp relprod_n([r | rs], r0) do
    t = raise_element(r0, 1)
    r1 = relative_product1(t, r)

    nR =
      projection(
        {:external,
         fn {{x, a}, aS} ->
           {x, {a, aS}}
         end},
        r1
      )

    relprod_n(rs, nR)
  end

  defp flat(1, a, l) do
    :erlang.list_to_tuple([a | l])
  end

  defp flat(n, {t, a}, l) do
    flat(n - 1, t, [a | l])
  end

  defp domain_type([t | ts], t0) when elem(t, 0) === :Set do
    case r_Set(t, :type) do
      {dT, _RT} ->
        case unify_types(dT, t0) do
          [] ->
            {:error, :type_mismatch}

          t1 ->
            domain_type(ts, t1)
        end

      :_ ->
        domain_type(ts, t0)

      _ ->
        {:error, :badarg}
    end
  end

  defp domain_type([], t0) do
    t0
  end

  defp range_type([t | ts], l) do
    case r_Set(t, :type) do
      {_DT, rT} ->
        range_type(ts, [rT | l])

      :_ ->
        :_
    end
  end

  defp range_type([], l) do
    :erlang.list_to_tuple(reverse(l))
  end

  defp converse([{a, b} | x], l) do
    converse(x, [{b, a} | l])
  end

  defp converse([], l) do
    sort(l)
  end

  defp strict([{e1, e2} | es], l) when e1 == e2 do
    strict(es, l)
  end

  defp strict([e | es], l) do
    strict(es, [e | l])
  end

  defp strict([], l) do
    reverse(l)
  end

  defp weak(es) do
    weak(es, ran(es, []), [])
  end

  defp weak(es = [{x, _} | _], [y | ys], l) when x > y do
    weak(es, ys, [{y, y} | l])
  end

  defp weak(es = [{x, _} | _], [y | ys], l) when x == y do
    weak(es, ys, l)
  end

  defp weak([e = {x, y} | es], ys, l) when x > y do
    weak1(es, ys, [e | l], x)
  end

  defp weak([e = {x, y} | es], ys, l) when x == y do
    weak2(es, ys, [e | l], x)
  end

  defp weak([e = {x, _Y} | es], ys, l) do
    weak2(es, ys, [e, {x, x} | l], x)
  end

  defp weak([], [y | ys], l) do
    weak([], ys, [{y, y} | l])
  end

  defp weak([], [], l) do
    reverse(l)
  end

  defp weak1([e = {x, y} | es], ys, l, x0)
       when x > y and
              x == x0 do
    weak1(es, ys, [e | l], x)
  end

  defp weak1([e = {x, y} | es], ys, l, x0)
       when x == y and
              x == x0 do
    weak2(es, ys, [e | l], x)
  end

  defp weak1([e = {x, _Y} | es], ys, l, x0) when x == x0 do
    weak2(es, ys, [e, {x, x} | l], x)
  end

  defp weak1(es, ys, l, x) do
    weak(es, ys, [{x, x} | l])
  end

  defp weak2([e = {x, _Y} | es], ys, l, x0) when x == x0 do
    weak2(es, ys, [e | l], x)
  end

  defp weak2(es, ys, l, _X) do
    weak(es, ys, l)
  end

  defp extc(l, [d | ds], c, ts) do
    extc(l, ds, c, ts, d)
  end

  defp extc(l, [], _C, _Ts) do
    l
  end

  defp extc(l, ds, c, [{x, _Y} | ts], d) when x < d do
    extc(l, ds, c, ts, d)
  end

  defp extc(l, ds, c, [{x, _Y} | ts], d) when x == d do
    extc(l, ds, c, ts)
  end

  defp extc(l, ds, c, [{x, _Y} | ts], d) do
    extc2([{d, c} | l], ds, c, ts, x)
  end

  defp extc(l, ds, c, [], d) do
    extc_tail([{d, c} | l], ds, c)
  end

  defp extc2(l, [d | ds], c, ts, x) when x > d do
    extc2([{d, c} | l], ds, c, ts, x)
  end

  defp extc2(l, [d | ds], c, ts, x) when x == d do
    extc(l, ds, c, ts)
  end

  defp extc2(l, [d | ds], c, ts, _X) do
    extc(l, ds, c, ts, d)
  end

  defp extc2(l, [], _C, _Ts, _X) do
    l
  end

  defp extc_tail(l, [d | ds], c) do
    extc_tail([{d, c} | l], ds, c)
  end

  defp extc_tail(l, [], _C) do
    l
  end

  defp is_a_func([{e, _} | es], e0) when e != e0 do
    is_a_func(es, e)
  end

  defp is_a_func(l, _E) do
    l === []
  end

  defp restrict_n(i, [t | ts], key, keys, l) do
    case :erlang.element(i, t) do
      k when k < key ->
        restrict_n(i, ts, key, keys, l)

      k when k == key ->
        restrict_n(i, ts, key, keys, [t | l])

      k ->
        restrict_n(i, k, ts, keys, l, t)
    end
  end

  defp restrict_n(_I, _Ts, _Key, _Keys, l) do
    l
  end

  defp restrict_n(i, k, ts, [key | keys], l, e) when k > key do
    restrict_n(i, k, ts, keys, l, e)
  end

  defp restrict_n(i, k, ts, [key | keys], l, e) when k == key do
    restrict_n(i, ts, key, keys, [e | l])
  end

  defp restrict_n(i, _K, ts, [key | keys], l, _E) do
    restrict_n(i, ts, key, keys, l)
  end

  defp restrict_n(_I, _K, _Ts, _Keys, l, _E) do
    l
  end

  defp restrict([key | keys], tuples) do
    restrict(tuples, key, keys, [])
  end

  defp restrict(_Keys, _Tuples) do
    []
  end

  defp restrict([{k, _E} | ts], key, keys, l) when k < key do
    restrict(ts, key, keys, l)
  end

  defp restrict([{k, e} | ts], key, keys, l) when k == key do
    restrict(ts, key, keys, [e | l])
  end

  defp restrict([{k, e} | ts], _Key, keys, l) do
    restrict(ts, k, keys, l, e)
  end

  defp restrict(_Ts, _Key, _Keys, l) do
    l
  end

  defp restrict(ts, k, [key | keys], l, e) when k > key do
    restrict(ts, k, keys, l, e)
  end

  defp restrict(ts, k, [key | keys], l, e) when k == key do
    restrict(ts, key, keys, [e | l])
  end

  defp restrict(ts, _K, [key | keys], l, _E) do
    restrict(ts, key, keys, l)
  end

  defp restrict(_Ts, _K, _Keys, l, _E) do
    l
  end

  defp diff_restrict_n(i, [t | ts], key, keys, l) do
    case :erlang.element(i, t) do
      k when k < key ->
        diff_restrict_n(i, ts, key, keys, [t | l])

      k when k == key ->
        diff_restrict_n(i, ts, key, keys, l)

      k ->
        diff_restrict_n(i, k, ts, keys, l, t)
    end
  end

  defp diff_restrict_n(i, _Ts, _Key, _Keys, l) when i === 1 do
    reverse(l)
  end

  defp diff_restrict_n(_I, _Ts, _Key, _Keys, l) do
    sort(l)
  end

  defp diff_restrict_n(i, k, ts, [key | keys], l, t) when k > key do
    diff_restrict_n(i, k, ts, keys, l, t)
  end

  defp diff_restrict_n(i, k, ts, [key | keys], l, _T) when k == key do
    diff_restrict_n(i, ts, key, keys, l)
  end

  defp diff_restrict_n(i, _K, ts, [key | keys], l, t) do
    diff_restrict_n(i, ts, key, keys, [t | l])
  end

  defp diff_restrict_n(i, _K, ts, _Keys, l, t) when i === 1 do
    reverse(l, [t | ts])
  end

  defp diff_restrict_n(_I, _K, ts, _Keys, l, t) do
    sort([t | ts ++ l])
  end

  defp diff_restrict([key | keys], tuples) do
    diff_restrict(tuples, key, keys, [])
  end

  defp diff_restrict(_Keys, tuples) do
    diff_restrict_tail(tuples, [])
  end

  defp diff_restrict([{k, e} | ts], key, keys, l) when k < key do
    diff_restrict(ts, key, keys, [e | l])
  end

  defp diff_restrict([{k, _E} | ts], key, keys, l) when k == key do
    diff_restrict(ts, key, keys, l)
  end

  defp diff_restrict([{k, e} | ts], _Key, keys, l) do
    diff_restrict(ts, k, keys, l, e)
  end

  defp diff_restrict(_Ts, _Key, _Keys, l) do
    l
  end

  defp diff_restrict(ts, k, [key | keys], l, e) when k > key do
    diff_restrict(ts, k, keys, l, e)
  end

  defp diff_restrict(ts, k, [key | keys], l, _E) when k == key do
    diff_restrict(ts, key, keys, l)
  end

  defp diff_restrict(ts, _K, [key | keys], l, e) do
    diff_restrict(ts, key, keys, [e | l])
  end

  defp diff_restrict(ts, _K, _Keys, l, e) do
    diff_restrict_tail(ts, [e | l])
  end

  defp diff_restrict_tail([{_K, e} | ts], l) do
    diff_restrict_tail(ts, [e | l])
  end

  defp diff_restrict_tail(_Ts, l) do
    l
  end

  defp comp([], b) do
    check_function(b, [])
  end

  defp comp(_A, []) do
    :bad_function
  end

  defp comp(a0, [{bx, by} | b]) do
    a = converse(a0, [])
    check_function(a0, comp1(a, b, [], bx, by))
  end

  defp comp1([{ay, ax} | a], b, l, bx, by) when ay == bx do
    comp1(a, b, [{ax, by} | l], bx, by)
  end

  defp comp1([{ay, ax} | a], b, l, bx, _By) when ay > bx do
    comp2(a, b, l, bx, ay, ax)
  end

  defp comp1([{ay, _Ax} | _A], _B, _L, bx, _By)
       when ay < bx do
    :bad_function
  end

  defp comp1([], b, l, bx, _By) do
    check_function(bx, b, l)
  end

  defp comp2(a, [{bx, _By} | b], l, bx0, ay, ax)
       when ay > bx and bx != bx0 do
    comp2(a, b, l, bx, ay, ax)
  end

  defp comp2(a, [{bx, by} | b], l, _Bx0, ay, ax)
       when ay == bx do
    comp1(a, b, [{ax, by} | l], bx, by)
  end

  defp comp2(_A, _B, _L, _Bx0, _Ay, _Ax) do
    :bad_function
  end

  defp inverse1([{a, b} | x]) do
    inverse(x, a, [{b, a}])
  end

  defp inverse1([]) do
    []
  end

  defp inverse([{a, b} | x], a0, l) when a0 != a do
    inverse(x, a, [{b, a} | l])
  end

  defp inverse([{a, _B} | _X], a0, _L) when a0 == a do
    :bad_function
  end

  defp inverse([], _A0, l) do
    sL = [{v, _} | es] = sort(l)

    case is_a_func(es, v) do
      true ->
        sL

      false ->
        :bad_function
    end
  end

  defp external_fun({:external, function}) when is_atom(function) do
    false
  end

  defp external_fun({:external, fun}) do
    fun
  end

  defp external_fun(_) do
    false
  end

  defp element_type([type]) do
    type
  end

  defp element_type(type) do
    type
  end

  defp subst(ts, fun, type) do
    subst(ts, fun, type, :_, [])
  end

  defp subst([t | ts], fun, type, nType, l) do
    case setfun(t, fun, type, nType) do
      {sD, sT} ->
        subst(ts, fun, type, sT, [{t, sD} | l])

      bad ->
        bad
    end
  end

  defp subst([], _Fun, _Type, nType, l) do
    {l, nType}
  end

  defp projection1([e | es]) do
    projection1([], :erlang.element(1, e), es)
  end

  defp projection1([] = l) do
    l
  end

  defp projection1(l, x, [e | es]) do
    case :erlang.element(1, e) do
      x1 when x == x1 ->
        projection1(l, x, es)

      x1 ->
        projection1([x | l], x1, es)
    end
  end

  defp projection1(l, x, []) do
    reverse(l, [x])
  end

  defp projection_n([e | es], i, l) do
    projection_n(es, i, [:erlang.element(i, e) | l])
  end

  defp projection_n([], _I, l) do
    usort(l)
  end

  defp substitute_element([t | ts], i, l) do
    substitute_element(ts, i, [{t, :erlang.element(i, t)} | l])
  end

  defp substitute_element(_, _I, l) do
    reverse(l)
  end

  defp substitute([t | ts], fun, l) do
    substitute(ts, fun, [{t, fun.(t)} | l])
  end

  defp substitute(_, _Fun, l) do
    reverse(l)
  end

  defp partition_n(i, [e | ts]) do
    partition_n(i, ts, :erlang.element(i, e), [e], [])
  end

  defp partition_n(_I, []) do
    []
  end

  defp partition_n(i, [e | ts], k, es, p) do
    case {:erlang.element(i, e), es} do
      {k1, _} when k == k1 ->
        partition_n(i, ts, k, [e | es], p)

      {k1, [_]} ->
        partition_n(i, ts, k1, [e], [es | p])

      {k1, _} ->
        partition_n(i, ts, k1, [e], [reverse(es) | p])
    end
  end

  defp partition_n(i, [], _K, es, p) when i > 1 do
    sort([reverse(es) | p])
  end

  defp partition_n(_I, [], _K, [_] = es, p) do
    reverse(p, [es])
  end

  defp partition_n(_I, [], _K, es, p) do
    reverse(p, [reverse(es)])
  end

  defp partition3_n(i, [t | ts], key, keys, l1, l2) do
    case :erlang.element(i, t) do
      k when k < key ->
        partition3_n(i, ts, key, keys, l1, [t | l2])

      k when k == key ->
        partition3_n(i, ts, key, keys, [t | l1], l2)

      k ->
        partition3_n(i, k, ts, keys, l1, l2, t)
    end
  end

  defp partition3_n(i, _Ts, _Key, _Keys, l1, l2) when i === 1 do
    [reverse(l1) | reverse(l2)]
  end

  defp partition3_n(_I, _Ts, _Key, _Keys, l1, l2) do
    [sort(l1) | sort(l2)]
  end

  defp partition3_n(i, k, ts, [key | keys], l1, l2, t)
       when k > key do
    partition3_n(i, k, ts, keys, l1, l2, t)
  end

  defp partition3_n(i, k, ts, [key | keys], l1, l2, t)
       when k == key do
    partition3_n(i, ts, key, keys, [t | l1], l2)
  end

  defp partition3_n(i, _K, ts, [key | keys], l1, l2, t) do
    partition3_n(i, ts, key, keys, l1, [t | l2])
  end

  defp partition3_n(i, _K, ts, _Keys, l1, l2, t) when i === 1 do
    [reverse(l1) | reverse(l2, [t | ts])]
  end

  defp partition3_n(_I, _K, ts, _Keys, l1, l2, t) do
    [sort(l1) | sort([t | ts ++ l2])]
  end

  defp partition3([key | keys], tuples) do
    partition3(tuples, key, keys, [], [])
  end

  defp partition3(_Keys, tuples) do
    partition3_tail(tuples, [], [])
  end

  defp partition3([{k, e} | ts], key, keys, l1, l2)
       when k < key do
    partition3(ts, key, keys, l1, [e | l2])
  end

  defp partition3([{k, e} | ts], key, keys, l1, l2)
       when k == key do
    partition3(ts, key, keys, [e | l1], l2)
  end

  defp partition3([{k, e} | ts], _Key, keys, l1, l2) do
    partition3(ts, k, keys, l1, l2, e)
  end

  defp partition3(_Ts, _Key, _Keys, l1, l2) do
    [l1 | l2]
  end

  defp partition3(ts, k, [key | keys], l1, l2, e) when k > key do
    partition3(ts, k, keys, l1, l2, e)
  end

  defp partition3(ts, k, [key | keys], l1, l2, e) when k == key do
    partition3(ts, key, keys, [e | l1], l2)
  end

  defp partition3(ts, _K, [key | keys], l1, l2, e) do
    partition3(ts, key, keys, l1, [e | l2])
  end

  defp partition3(ts, _K, _Keys, l1, l2, e) do
    partition3_tail(ts, l1, [e | l2])
  end

  defp partition3_tail([{_K, e} | ts], l1, l2) do
    partition3_tail(ts, l1, [e | l2])
  end

  defp partition3_tail(_Ts, l1, l2) do
    [l1 | l2]
  end

  defp replace([e | es], f, l) do
    replace(es, f, [f.(e) | l])
  end

  defp replace(_, _F, l) do
    sort(l)
  end

  defp mul_relprod([t | ts], i, r) when elem(t, 0) === :Set do
    p = raise_element(r, i)
    f = relative_product1(p, t)
    [f | mul_relprod(ts, i + 1, r)]
  end

  defp mul_relprod([], _I, _R) do
    []
  end

  defp raise_element(r, i) do
    l = sort(i !== 1, rearr(r_Set(r, :data), i, []))
    type = r_Set(r, :type)
    r_Set(data: l, type: {:erlang.element(i, type), type})
  end

  defp rearr([e | es], i, l) do
    rearr(es, i, [{:erlang.element(i, e), e} | l])
  end

  defp rearr([], _I, l) do
    l
  end

  defp join_element(e1, e2) do
    [_ | l2] = :erlang.tuple_to_list(e2)
    :erlang.list_to_tuple(:erlang.tuple_to_list(e1) ++ l2)
  end

  defp join_element(e1, e2, i2) do
    :erlang.tuple_to_list(e1) ++ join_element2(:erlang.tuple_to_list(e2), 1, i2)
  end

  defp join_element2([b | bs], c, i2) when c !== i2 do
    [b | join_element2(bs, c + 1, i2)]
  end

  defp join_element2([_ | bs], _C, _I2) do
    bs
  end

  defp family2rel([{x, s} | f], l) do
    fam2rel(f, l, x, s)
  end

  defp family2rel([], l) do
    reverse(l)
  end

  defp fam2rel(f, l, x, [y | ys]) do
    fam2rel(f, [{x, y} | l], x, ys)
  end

  defp fam2rel(f, l, _X, _) do
    family2rel(f, l)
  end

  defp fam_spec([{_, s} = e | f], fun, type, l) do
    case fun.(r_Set(data: s, type: type)) do
      true ->
        fam_spec(f, fun, type, [e | l])

      false ->
        fam_spec(f, fun, type, l)

      _ ->
        :badarg
    end
  end

  defp fam_spec([], _Fun, _Type, l) do
    reverse(l)
  end

  defp fam_specification([{_, s} = e | f], fun, l) do
    case fun.(s) do
      true ->
        fam_specification(f, fun, [e | l])

      false ->
        fam_specification(f, fun, l)

      _ ->
        :badarg
    end
  end

  defp fam_specification([], _Fun, l) do
    reverse(l)
  end

  defp un_of_fam([{_X, s} | f], l) do
    un_of_fam(f, [s | l])
  end

  defp un_of_fam([], l) do
    lunion(sort(l))
  end

  defp int_of_fam([{_, s} | f]) do
    int_of_fam(f, [s])
  end

  defp int_of_fam([]) do
    :badarg
  end

  defp int_of_fam([{_, s} | f], l) do
    int_of_fam(f, [s | l])
  end

  defp int_of_fam([], [l | ls]) do
    lintersection(ls, l)
  end

  defp fam_un([{x, s} | f], l) do
    fam_un(f, [{x, lunion(s)} | l])
  end

  defp fam_un([], l) do
    reverse(l)
  end

  defp fam_int([{x, [s | ss]} | f], l) do
    fam_int(f, [{x, lintersection(ss, s)} | l])
  end

  defp fam_int([{_X, []} | _F], _L) do
    :badarg
  end

  defp fam_int([], l) do
    reverse(l)
  end

  defp fam_dom([{x, s} | f], l) do
    fam_dom(f, [{x, dom(s)} | l])
  end

  defp fam_dom([], l) do
    reverse(l)
  end

  defp fam_ran([{x, s} | f], l) do
    fam_ran(f, [{x, ran(s, [])} | l])
  end

  defp fam_ran([], l) do
    reverse(l)
  end

  defp fam_union(f1 = [{a, _AS} | _AL], [b1 = {b, _BS} | bL], l)
       when a > b do
    fam_union(f1, bL, [b1 | l])
  end

  defp fam_union([{a, aS} | aL], [{b, bS} | bL], l)
       when a == b do
    fam_union(aL, bL, [{a, umerge(aS, bS)} | l])
  end

  defp fam_union([a1 | aL], f2, l) do
    fam_union(aL, f2, [a1 | l])
  end

  defp fam_union(_, f2, l) do
    reverse(l, f2)
  end

  defp fam_intersect(f1 = [{a, _AS} | _AL], [{b, _BS} | bL], l)
       when a > b do
    fam_intersect(f1, bL, l)
  end

  defp fam_intersect([{a, aS} | aL], [{b, bS} | bL], l)
       when a == b do
    fam_intersect(aL, bL, [{a, intersection(aS, bS, [])} | l])
  end

  defp fam_intersect([_A1 | aL], f2, l) do
    fam_intersect(aL, f2, l)
  end

  defp fam_intersect(_, _, l) do
    reverse(l)
  end

  defp fam_difference(f1 = [{a, _AS} | _AL], [{b, _BS} | bL], l)
       when a > b do
    fam_difference(f1, bL, l)
  end

  defp fam_difference([{a, aS} | aL], [{b, bS} | bL], l)
       when a == b do
    fam_difference(aL, bL, [{a, difference(aS, bS, [])} | l])
  end

  defp fam_difference([a1 | aL], f2, l) do
    fam_difference(aL, f2, [a1 | l])
  end

  defp fam_difference(f1, _, l) do
    reverse(l, f1)
  end

  defp check_function([{x, _} | xL], r) do
    check_function(x, xL, r)
  end

  defp check_function([], r) do
    r
  end

  defp check_function(x0, [{x, _} | xL], r) when x0 != x do
    check_function(x, xL, r)
  end

  defp check_function(x0, [{x, _} | _XL], _R) when x0 == x do
    :bad_function
  end

  defp check_function(_X0, [], r) do
    r
  end

  defp fam_partition_n(i, [e | ts]) do
    fam_partition_n(i, ts, :erlang.element(i, e), [e], [])
  end

  defp fam_partition_n(_I, []) do
    []
  end

  defp fam_partition_n(i, [e | ts], k, es, p) do
    case {:erlang.element(i, e), es} do
      {k1, _} when k == k1 ->
        fam_partition_n(i, ts, k, [e | es], p)

      {k1, [_]} ->
        fam_partition_n(i, ts, k1, [e], [{k, es} | p])

      {k1, _} ->
        fam_partition_n(i, ts, k1, [e], [{k, reverse(es)} | p])
    end
  end

  defp fam_partition_n(_I, [], k, [_] = es, p) do
    reverse(p, [{k, es}])
  end

  defp fam_partition_n(_I, [], k, es, p) do
    reverse(p, [{k, reverse(es)}])
  end

  defp fam_partition([{k, vs} | ts], sort) do
    fam_partition(ts, k, [vs], [], sort)
  end

  defp fam_partition([], _Sort) do
    []
  end

  defp fam_partition([{k1, v} | ts], k, vs, p, s) when k1 == k do
    fam_partition(ts, k, [v | vs], p, s)
  end

  defp fam_partition([{k1, v} | ts], k, [_] = vs, p, s) do
    fam_partition(ts, k1, [v], [{k, vs} | p], s)
  end

  defp fam_partition([{k1, v} | ts], k, vs, p, s) do
    fam_partition(ts, k1, [v], [{k, sort(s, vs)} | p], s)
  end

  defp fam_partition([], k, [_] = vs, p, _S) do
    [{k, vs} | p]
  end

  defp fam_partition([], k, vs, p, s) do
    [{k, sort(s, vs)} | p]
  end

  defp fam_proj([{x, s} | f], fun, type, nType, l) do
    case setfun(s, fun, type, nType) do
      {sD, sT} ->
        fam_proj(f, fun, type, sT, [{x, sD} | l])

      bad ->
        bad
    end
  end

  defp fam_proj([], _Fun, _Type, nType, l) do
    {reverse(l), nType}
  end

  defp setfun(t, fun, type, nType) do
    case fun.(term2set(t, type)) do
      nS when elem(nS, 0) === :Set ->
        case unify_types(nType, [r_Set(nS, :type)]) do
          [] ->
            :type_mismatch

          nT ->
            {r_Set(nS, :data), nT}
        end

      nS when elem(nS, 0) === :OrdSet ->
        case unify_types(nType, nT = r_OrdSet(nS, :ordtype)) do
          [] ->
            :type_mismatch

          ^nT ->
            {r_OrdSet(nS, :orddata), nT}
        end

      _ ->
        :badarg
    end
  end

  defp term2set(l, type) when is_list(l) do
    r_Set(data: l, type: type)
  end

  defp term2set(t, type) do
    r_OrdSet(orddata: t, ordtype: type)
  end

  defp fam2digraph(f, g) do
    fun = fn {from, toL} ->
      :digraph.add_vertex(g, from)

      fun2 = fn to ->
        :digraph.add_vertex(g, to)

        case :digraph.add_edge(g, from, to) do
          {:error, {:bad_edge, _}} ->
            throw({:error, :cyclic})

          _ ->
            true
        end
      end

      foreach(fun2, toL)
    end

    foreach(fun, to_external(f))
    g
  end

  defp digraph_family(g) do
    vs = sort(:digraph.vertices(g))
    digraph_fam(vs, vs, g, [])
  end

  defp digraph_fam([v | vs], v0, g, l) when v != v0 do
    ns = sort(:digraph.out_neighbours(g, v))
    digraph_fam(vs, v, g, [{v, ns} | l])
  end

  defp digraph_fam([], _V0, _G, l) do
    reverse(l)
  end

  defp check_fun(t, f, funT) do
    true = is_type(funT)
    {nT, _MaxI} = number_tuples(t, 1)
    l = flatten(tuple2list(f.(nT)))
    has_hole(l, 1)
  end

  defp number_tuples(t, n) when is_tuple(t) do
    {l, nN} = mapfoldl(&number_tuples/2, n, :erlang.tuple_to_list(t))
    {:erlang.list_to_tuple(l), nN}
  end

  defp number_tuples(_, n) do
    {n, n + 1}
  end

  defp tuple2list(t) when is_tuple(t) do
    map(&tuple2list/1, :erlang.tuple_to_list(t))
  end

  defp tuple2list(c) do
    [c]
  end

  defp has_hole([i | is], i0) when i <= i0 do
    has_hole(is, :erlang.max(i + 1, i0))
  end

  defp has_hole(is, _I) do
    is !== []
  end

  defp check_for_sort(t, _I) when t === :_ do
    :empty
  end

  defp check_for_sort(t, i)
       when is_tuple(t) and
              i <= tuple_size(t) and i >= 1 do
    i > 1
  end

  defp check_for_sort(_T, _I) do
    :error
  end

  defp inverse_substitution(l, fun, sort) do
    sort(sort, fun_rearr(l, fun, []))
  end

  defp fun_rearr([e | es], fun, l) do
    fun_rearr(es, fun, [{fun.(e), e} | l])
  end

  defp fun_rearr([], _Fun, l) do
    l
  end

  defp sets_to_list(ss) do
    map(
      fn s when elem(s, 0) === :Set ->
        r_Set(s, :data)
      end,
      ss
    )
  end

  defp types([], l) do
    :erlang.list_to_tuple(reverse(l))
  end

  defp types([s | _Ss], _L) when r_Set(s, :type) === :_ do
    :_
  end

  defp types([s | ss], l) do
    types(ss, [r_Set(s, :type) | l])
  end

  defp unify_types(t, t) do
    t
  end

  defp unify_types(type1, type2) do
    try do
      unify_types1(type1, type2)
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end
  end

  defp unify_types1(atom, atom) when is_atom(atom) do
    atom
  end

  defp unify_types1(:_, type) do
    type
  end

  defp unify_types1(type, :_) do
    type
  end

  defp unify_types1([type1], [type2]) do
    [unify_types1(type1, type2)]
  end

  defp unify_types1(t1, t2)
       when tuple_size(t1) === tuple_size(t2) do
    unify_typesl(tuple_size(t1), t1, t2, [])
  end

  defp unify_types1(_T1, _T2) do
    throw([])
  end

  defp unify_typesl(0, _T1, _T2, l) do
    :erlang.list_to_tuple(l)
  end

  defp unify_typesl(n, t1, t2, l) do
    t =
      unify_types1(
        :erlang.element(n, t1),
        :erlang.element(n, t2)
      )

    unify_typesl(n - 1, t1, t2, [t | l])
  end

  defp match_types(t, t) do
    true
  end

  defp match_types(type1, type2) do
    match_types1(type1, type2)
  end

  defp match_types1(atom, atom) when is_atom(atom) do
    true
  end

  defp match_types1(:_, _) do
    true
  end

  defp match_types1(_, :_) do
    true
  end

  defp match_types1([type1], [type2]) do
    match_types1(type1, type2)
  end

  defp match_types1(t1, t2)
       when tuple_size(t1) === tuple_size(t2) do
    match_typesl(tuple_size(t1), t1, t2)
  end

  defp match_types1(_T1, _T2) do
    false
  end

  defp match_typesl(0, _T1, _T2) do
    true
  end

  defp match_typesl(n, t1, t2) do
    case match_types1(
           :erlang.element(n, t1),
           :erlang.element(n, t2)
         ) do
      true ->
        match_typesl(n - 1, t1, t2)

      false ->
        false
    end
  end

  defp sort(true, l) do
    sort(l)
  end

  defp sort(false, l) do
    reverse(l)
  end
end
