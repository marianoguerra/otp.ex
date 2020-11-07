defmodule :m_orddict do
  use Bitwise

  def new() do
    []
  end

  def is_key(key, [{k, _} | _]) when key < k do
    false
  end

  def is_key(key, [{k, _} | dict]) when key > k do
    is_key(key, dict)
  end

  def is_key(_Key, [{_K, _Val} | _]) do
    true
  end

  def is_key(_, []) do
    false
  end

  def to_list(dict) do
    dict
  end

  def from_list([]) do
    []
  end

  def from_list([{_, _}] = pair) do
    pair
  end

  def from_list(pairs) do
    :lists.ukeysort(1, reverse_pairs(pairs, []))
  end

  def size(d) do
    length(d)
  end

  def is_empty([]) do
    true
  end

  def is_empty([_ | _]) do
    false
  end

  def fetch(key, [{k, _} | d]) when key > k do
    fetch(key, d)
  end

  def fetch(key, [{k, value} | _]) when key == k do
    value
  end

  def find(key, [{k, _} | _]) when key < k do
    :error
  end

  def find(key, [{k, _} | d]) when key > k do
    find(key, d)
  end

  def find(_Key, [{_K, value} | _]) do
    {:ok, value}
  end

  def find(_, []) do
    :error
  end

  def fetch_keys([{key, _} | dict]) do
    [key | fetch_keys(dict)]
  end

  def fetch_keys([]) do
    []
  end

  def erase(key, [{k, _} = e | dict]) when key < k do
    [e | dict]
  end

  def erase(key, [{k, _} = e | dict]) when key > k do
    [e | erase(key, dict)]
  end

  def erase(_Key, [{_K, _Val} | dict]) do
    dict
  end

  def erase(_, []) do
    []
  end

  def take(key, dict) do
    take_1(key, dict, [])
  end

  defp take_1(key, [{k, _} | _], _Acc) when key < k do
    :error
  end

  defp take_1(key, [{k, _} = p | d], acc) when key > k do
    take_1(key, d, [p | acc])
  end

  defp take_1(_Key, [{_K, value} | d], acc) do
    {value, :lists.reverse(acc, d)}
  end

  defp take_1(_, [], _) do
    :error
  end

  def store(key, new, [{k, _} | _] = dict) when key < k do
    [{key, new} | dict]
  end

  def store(key, new, [{k, _} = e | dict]) when key > k do
    [e | store(key, new, dict)]
  end

  def store(key, new, [{_K, _Old} | dict]) do
    [{key, new} | dict]
  end

  def store(key, new, []) do
    [{key, new}]
  end

  def append(key, new, [{k, _} | _] = dict) when key < k do
    [{key, [new]} | dict]
  end

  def append(key, new, [{k, _} = e | dict]) when key > k do
    [e | append(key, new, dict)]
  end

  def append(key, new, [{_K, old} | dict]) do
    [{key, old ++ [new]} | dict]
  end

  def append(key, new, []) do
    [{key, [new]}]
  end

  def append_list(key, newList, [{k, _} | _] = dict)
      when key < k do
    [{key, newList} | dict]
  end

  def append_list(key, newList, [{k, _} = e | dict])
      when key > k do
    [e | append_list(key, newList, dict)]
  end

  def append_list(key, newList, [{_K, old} | dict]) do
    [{key, old ++ newList} | dict]
  end

  def append_list(key, newList, []) do
    [{key, newList}]
  end

  def update(key, fun, [{k, _} = e | dict]) when key > k do
    [e | update(key, fun, dict)]
  end

  def update(key, fun, [{k, val} | dict]) when key == k do
    [{key, fun.(val)} | dict]
  end

  def update(key, _, init, [{k, _} | _] = dict)
      when key < k do
    [{key, init} | dict]
  end

  def update(key, fun, init, [{k, _} = e | dict])
      when key > k do
    [e | update(key, fun, init, dict)]
  end

  def update(key, fun, _Init, [{_K, val} | dict]) do
    [{key, fun.(val)} | dict]
  end

  def update(key, _, init, []) do
    [{key, init}]
  end

  def update_counter(key, incr, [{k, _} | _] = dict) when key < k do
    [{key, incr} | dict]
  end

  def update_counter(key, incr, [{k, _} = e | dict]) when key > k do
    [e | update_counter(key, incr, dict)]
  end

  def update_counter(key, incr, [{_K, val} | dict]) do
    [{key, val + incr} | dict]
  end

  def update_counter(key, incr, []) do
    [{key, incr}]
  end

  def fold(f, acc, [{key, val} | d]) do
    fold(f, f.(key, val, acc), d)
  end

  def fold(f, acc, []) when is_function(f, 3) do
    acc
  end

  def map(f, [{key, val} | d]) do
    [{key, f.(key, val)} | map(f, d)]
  end

  def map(f, []) when is_function(f, 2) do
    []
  end

  def filter(f, [{key, val} = e | d]) do
    case f.(key, val) do
      true ->
        [e | filter(f, d)]

      false ->
        filter(f, d)
    end
  end

  def filter(f, []) when is_function(f, 2) do
    []
  end

  def merge(f, [{k1, _} = e1 | d1], [{k2, _} = e2 | d2])
      when k1 < k2 do
    [e1 | merge(f, d1, [e2 | d2])]
  end

  def merge(f, [{k1, _} = e1 | d1], [{k2, _} = e2 | d2])
      when k1 > k2 do
    [e2 | merge(f, [e1 | d1], d2)]
  end

  def merge(f, [{k1, v1} | d1], [{_K2, v2} | d2]) do
    [{k1, f.(k1, v1, v2)} | merge(f, d1, d2)]
  end

  def merge(f, [], d2) when is_function(f, 3) do
    d2
  end

  def merge(f, d1, []) when is_function(f, 3) do
    d1
  end

  defp reverse_pairs([{_, _} = h | t], acc) do
    reverse_pairs(t, [h | acc])
  end

  defp reverse_pairs([], acc) do
    acc
  end
end
