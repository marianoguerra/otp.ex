defmodule :m_proplists do
  use Bitwise

  def property({key, true}) when is_atom(key) do
    key
  end

  def property(property) do
    property
  end

  def property(key, true) when is_atom(key) do
    key
  end

  def property(key, value) do
    {key, value}
  end

  def unfold([p | ps]) do
    cond do
      is_atom(p) ->
        [{p, true} | unfold(ps)]

      true ->
        [p | unfold(ps)]
    end
  end

  def unfold([]) do
    []
  end

  def compact(listIn) do
    for p <- listIn do
      property(p)
    end
  end

  def lookup(key, [p | ps]) do
    cond do
      is_atom(p) and p === key ->
        {key, true}

      tuple_size(p) >= 1 and
          :erlang.element(1, p) === key ->
        p

      true ->
        lookup(key, ps)
    end
  end

  def lookup(_Key, []) do
    :none
  end

  def lookup_all(key, [p | ps]) do
    cond do
      is_atom(p) and p === key ->
        [{key, true} | lookup_all(key, ps)]

      tuple_size(p) >= 1 and
          :erlang.element(1, p) === key ->
        [p | lookup_all(key, ps)]

      true ->
        lookup_all(key, ps)
    end
  end

  def lookup_all(_Key, []) do
    []
  end

  def is_defined(key, [p | ps]) do
    cond do
      is_atom(p) and p === key ->
        true

      tuple_size(p) >= 1 and
          :erlang.element(1, p) === key ->
        true

      true ->
        is_defined(key, ps)
    end
  end

  def is_defined(_Key, []) do
    false
  end

  def get_value(key, list) do
    get_value(key, list, :undefined)
  end

  def get_value(key, [p | ps], default) do
    cond do
      is_atom(p) and p === key ->
        true

      tuple_size(p) >= 1 and
          :erlang.element(1, p) === key ->
        case p do
          {_, value} ->
            value

          _ ->
            default
        end

      true ->
        get_value(key, ps, default)
    end
  end

  def get_value(_Key, [], default) do
    default
  end

  def get_all_values(key, [p | ps]) do
    cond do
      is_atom(p) and p === key ->
        [true | get_all_values(key, ps)]

      tuple_size(p) >= 1 and
          :erlang.element(1, p) === key ->
        case p do
          {_, value} ->
            [value | get_all_values(key, ps)]

          _ ->
            get_all_values(key, ps)
        end

      true ->
        get_all_values(key, ps)
    end
  end

  def get_all_values(_Key, []) do
    []
  end

  def append_values(key, [p | ps]) do
    cond do
      is_atom(p) and p === key ->
        [true | append_values(key, ps)]

      tuple_size(p) >= 1 and
          :erlang.element(1, p) === key ->
        case p do
          {_, value} when is_list(value) ->
            value ++ append_values(key, ps)

          {_, value} ->
            [value | append_values(key, ps)]

          _ ->
            append_values(key, ps)
        end

      true ->
        append_values(key, ps)
    end
  end

  def append_values(_Key, []) do
    []
  end

  def get_bool(key, [p | ps]) do
    cond do
      is_atom(p) and p === key ->
        true

      tuple_size(p) >= 1 and
          :erlang.element(1, p) === key ->
        case p do
          {_, true} ->
            true

          _ ->
            false
        end

      true ->
        get_bool(key, ps)
    end
  end

  def get_bool(_Key, []) do
    false
  end

  def get_keys(ps) do
    :sets.to_list(get_keys(ps, :sets.new()))
  end

  defp get_keys([p | ps], keys) do
    cond do
      is_atom(p) ->
        get_keys(ps, :sets.add_element(p, keys))

      tuple_size(p) >= 1 ->
        get_keys(
          ps,
          :sets.add_element(:erlang.element(1, p), keys)
        )

      true ->
        get_keys(ps, keys)
    end
  end

  defp get_keys([], keys) do
    keys
  end

  def delete(key, [p | ps]) do
    cond do
      is_atom(p) and p === key ->
        delete(key, ps)

      tuple_size(p) >= 1 and
          :erlang.element(1, p) === key ->
        delete(key, ps)

      true ->
        [p | delete(key, ps)]
    end
  end

  def delete(_, []) do
    []
  end

  def substitute_aliases(as, props) do
    for p <- props do
      substitute_aliases_1(as, p)
    end
  end

  defp substitute_aliases_1([{key, key1} | as], p) do
    cond do
      is_atom(p) and p === key ->
        property(key1, true)

      tuple_size(p) >= 1 and
          :erlang.element(1, p) === key ->
        property(:erlang.setelement(1, p, key1))

      true ->
        substitute_aliases_1(as, p)
    end
  end

  defp substitute_aliases_1([], p) do
    p
  end

  def substitute_negations(as, props) do
    for p <- props do
      substitute_negations_1(as, p)
    end
  end

  defp substitute_negations_1([{key, key1} | as], p) do
    cond do
      is_atom(p) and p === key ->
        property(key1, false)

      tuple_size(p) >= 1 and
          :erlang.element(1, p) === key ->
        case p do
          {_, true} ->
            property(key1, false)

          {_, false} ->
            property(key1, true)

          _ ->
            property(key1, true)
        end

      true ->
        substitute_negations_1(as, p)
    end
  end

  defp substitute_negations_1([], p) do
    p
  end

  def expand(es, ps) when is_list(ps) do
    es1 =
      for {p, v} <- es do
        {property(p), v}
      end

    flatten(expand_0(key_uniq(es1), ps))
  end

  defp expand_0([{p, l} | es], ps) do
    expand_0(es, expand_1(p, l, ps))
  end

  defp expand_0([], ps) do
    ps
  end

  defp expand_1(p, l, ps) do
    cond do
      is_atom(p) ->
        expand_2(p, p, l, ps)

      tuple_size(p) >= 1 ->
        expand_2(:erlang.element(1, p), p, l, ps)

      true ->
        ps
    end
  end

  defp expand_2(key, p1, l, [p | ps]) do
    cond do
      is_atom(p) and p === key ->
        expand_3(key, p1, p, l, ps)

      tuple_size(p) >= 1 and
          :erlang.element(1, p) === key ->
        expand_3(key, p1, property(p), l, ps)

      true ->
        [p | expand_2(key, p1, l, ps)]
    end
  end

  defp expand_2(_, _, _, []) do
    []
  end

  defp expand_3(key, p1, p, l, ps) do
    cond do
      p1 === p ->
        [l | delete(key, ps)]

      true ->
        [p | ps]
    end
  end

  defp key_uniq([{k, v} | ps]) do
    [{k, v} | key_uniq_1(k, ps)]
  end

  defp key_uniq([]) do
    []
  end

  defp key_uniq_1(k, [{k1, v} | ps]) do
    cond do
      k === k1 ->
        key_uniq_1(k, ps)

      true ->
        [{k1, v} | key_uniq_1(k1, ps)]
    end
  end

  defp key_uniq_1(_, []) do
    []
  end

  defp flatten([e | es]) when is_list(e) do
    e ++ flatten(es)
  end

  defp flatten([e | es]) do
    [e | flatten(es)]
  end

  defp flatten([]) do
    []
  end

  def normalize(l, [{:aliases, as} | xs]) do
    normalize(substitute_aliases(as, l), xs)
  end

  def normalize(l, [{:expand, es} | xs]) do
    normalize(expand(es, l), xs)
  end

  def normalize(l, [{:negations, ns} | xs]) do
    normalize(substitute_negations(ns, l), xs)
  end

  def normalize(l, []) do
    compact(l)
  end

  def split(list, keys) do
    {store, rest} =
      split(
        list,
        :maps.from_list(
          for k <- keys do
            {k, []}
          end
        ),
        []
      )

    {for k <- keys do
       :lists.reverse(:erlang.map_get(k, store))
     end, :lists.reverse(rest)}
  end

  defp split([p | ps], store, rest) do
    cond do
      is_atom(p) ->
        case :erlang.is_map_key(p, store) do
          true ->
            split(ps, maps_prepend(p, p, store), rest)

          false ->
            split(ps, store, [p | rest])
        end

      tuple_size(p) >= 1 ->
        key = :erlang.element(1, p)

        case :erlang.is_map_key(key, store) do
          true ->
            split(ps, maps_prepend(key, p, store), rest)

          false ->
            split(ps, store, [p | rest])
        end

      true ->
        split(ps, store, [p | rest])
    end
  end

  defp split([], store, rest) do
    {store, rest}
  end

  defp maps_prepend(key, val, dict) do
    %{dict | key => [val | :erlang.map_get(key, dict)]}
  end
end
