defmodule :cow_hpack do
  use Bitwise
  require Record
  Record.defrecord(:r_state, :state, size: 0, max_size: 4096,
                                 configured_max_size: 4096, dyn_table: [])
  def init() do
    r_state()
  end

  def init(maxSize) do
    r_state(max_size: maxSize, configured_max_size: maxSize)
  end

  def set_max_size(maxSize, state) do
    r_state(state, configured_max_size: maxSize)
  end

  def decode(data) do
    decode(data, init())
  end

  def decode(<<0 :: size(2), 1 :: size(1), rest :: bits>>,
           state = r_state(configured_max_size: configMaxSize)) do
    {maxSize, rest2} = dec_int5(rest)
    cond do
      maxSize <= configMaxSize ->
        state2 = table_update_size(maxSize, state)
        decode(rest2, state2)
    end
  end

  def decode(data, state) do
    decode(data, state, [])
  end

  defp decode(<<>>, state, acc) do
    {:lists.reverse(acc), state}
  end

  defp decode(<<1 :: size(1), rest :: bits>>, state, acc) do
    dec_indexed(rest, state, acc)
  end

  defp decode(<<0 :: size(1), 1 :: size(1), 0 :: size(6),
              rest :: bits>>,
            state, acc) do
    dec_lit_index_new_name(rest, state, acc)
  end

  defp decode(<<0 :: size(1), 1 :: size(1), rest :: bits>>,
            state, acc) do
    dec_lit_index_indexed_name(rest, state, acc)
  end

  defp decode(<<0 :: size(8), rest :: bits>>, state, acc) do
    dec_lit_no_index_new_name(rest, state, acc)
  end

  defp decode(<<0 :: size(4), rest :: bits>>, state, acc) do
    dec_lit_no_index_indexed_name(rest, state, acc)
  end

  defp decode(<<0 :: size(3), 1 :: size(1), 0 :: size(4),
              rest :: bits>>,
            state, acc) do
    dec_lit_no_index_new_name(rest, state, acc)
  end

  defp decode(<<0 :: size(3), 1 :: size(1), rest :: bits>>,
            state, acc) do
    dec_lit_no_index_indexed_name(rest, state, acc)
  end

  defp dec_indexed(<<127 :: size(7), 0 :: size(1), int :: size(7),
              rest :: bits>>,
            state, acc) do
    {name, value} = table_get(127 + int, state)
    decode(rest, state, [{name, value} | acc])
  end

  defp dec_indexed(<<127 :: size(7), rest0 :: bits>>, state,
            acc) do
    {index, rest} = dec_big_int(rest0, 127, 0)
    {name, value} = table_get(index, state)
    decode(rest, state, [{name, value} | acc])
  end

  defp dec_indexed(<<index :: size(7), rest :: bits>>, state,
            acc) do
    {name, value} = table_get(index, state)
    decode(rest, state, [{name, value} | acc])
  end

  defp dec_lit_index_new_name(rest, state, acc) do
    {name, rest2} = dec_str(rest)
    dec_lit_index(rest2, state, acc, name)
  end

  defp dec_lit_index_indexed_name(<<63 :: size(6), 0 :: size(1), int :: size(7),
              rest :: bits>>,
            state, acc) do
    name = table_get_name(63 + int, state)
    dec_lit_index(rest, state, acc, name)
  end

  defp dec_lit_index_indexed_name(<<63 :: size(6), rest0 :: bits>>, state, acc) do
    {index, rest} = dec_big_int(rest0, 63, 0)
    name = table_get_name(index, state)
    dec_lit_index(rest, state, acc, name)
  end

  defp dec_lit_index_indexed_name(<<index :: size(6), rest :: bits>>, state,
            acc) do
    name = table_get_name(index, state)
    dec_lit_index(rest, state, acc, name)
  end

  defp dec_lit_index(rest, state, acc, name) do
    {value, rest2} = dec_str(rest)
    state2 = table_insert({name, value}, state)
    decode(rest2, state2, [{name, value} | acc])
  end

  defp dec_lit_no_index_new_name(rest, state, acc) do
    {name, rest2} = dec_str(rest)
    dec_lit_no_index(rest2, state, acc, name)
  end

  defp dec_lit_no_index_indexed_name(<<15 :: size(4), 0 :: size(1), int :: size(7),
              rest :: bits>>,
            state, acc) do
    name = table_get_name(15 + int, state)
    dec_lit_no_index(rest, state, acc, name)
  end

  defp dec_lit_no_index_indexed_name(<<15 :: size(4), rest0 :: bits>>, state, acc) do
    {index, rest} = dec_big_int(rest0, 15, 0)
    name = table_get_name(index, state)
    dec_lit_no_index(rest, state, acc, name)
  end

  defp dec_lit_no_index_indexed_name(<<index :: size(4), rest :: bits>>, state,
            acc) do
    name = table_get_name(index, state)
    dec_lit_no_index(rest, state, acc, name)
  end

  defp dec_lit_no_index(rest, state, acc, name) do
    {value, rest2} = dec_str(rest)
    decode(rest2, state, [{name, value} | acc])
  end

  defp dec_int5(<<31 :: size(5), rest :: bits>>) do
    dec_big_int(rest, 31, 0)
  end

  defp dec_int5(<<int :: size(5), rest :: bits>>) do
    {int, rest}
  end

  defp dec_big_int(<<0 :: size(1), value :: size(7),
              rest :: bits>>,
            int, m) do
    {int + (value <<< m), rest}
  end

  defp dec_big_int(<<1 :: size(1), value :: size(7),
              rest :: bits>>,
            int, m) do
    dec_big_int(rest, int + (value <<< m), m + 7)
  end

  defp dec_str(<<0 :: size(1), 127 :: size(7),
              rest0 :: bits>>) do
    {length, rest1} = dec_big_int(rest0, 127, 0)
    <<str :: size(length) - binary, rest :: bits>> = rest1
    {str, rest}
  end

  defp dec_str(<<0 :: size(1), length :: size(7),
              rest0 :: bits>>) do
    <<str :: size(length) - binary, rest :: bits>> = rest0
    {str, rest}
  end

  defp dec_str(<<1 :: size(1), 127 :: size(7),
              rest0 :: bits>>) do
    {length, rest} = dec_big_int(rest0, 127, 0)
    dec_huffman(rest, length, 0, <<>>)
  end

  defp dec_str(<<1 :: size(1), length :: size(7),
              rest :: bits>>) do
    dec_huffman(rest, length, 0, <<>>)
  end

  defp dec_huffman(<<a :: size(4), b :: size(4), r :: bits>>, len,
            huff0, acc)
      when len > 1 do
    {_, charA, huff1} = dec_huffman_lookup(huff0, a)
    {_, charB, huff} = dec_huffman_lookup(huff1, b)
    case ({charA, charB}) do
      {:undefined, :undefined} ->
        dec_huffman(r, len - 1, huff, acc)
      {^charA, :undefined} ->
        dec_huffman(r, len - 1, huff, <<acc :: binary, charA>>)
      {:undefined, ^charB} ->
        dec_huffman(r, len - 1, huff, <<acc :: binary, charB>>)
      {^charA, ^charB} ->
        dec_huffman(r, len - 1, huff,
                      <<acc :: binary, charA, charB>>)
    end
  end

  defp dec_huffman(<<a :: size(4), b :: size(4), rest :: bits>>, 1,
            huff0, acc) do
    {_, charA, huff} = dec_huffman_lookup(huff0, a)
    {:ok, charB, _} = dec_huffman_lookup(huff, b)
    case ({charA, charB}) do
      {^charA, :undefined} ->
        {<<acc :: binary, charA>>, rest}
      {:undefined, ^charB} ->
        {<<acc :: binary, charB>>, rest}
      _ ->
        {<<acc :: binary, charA, charB>>, rest}
    end
  end

  defp dec_huffman(rest, 0, _, <<>>) do
    {<<>>, rest}
  end

  defp dec_huffman_lookup(0, 0) do
    {:more, :undefined, 4}
  end

  defp dec_huffman_lookup(0, 1) do
    {:more, :undefined, 5}
  end

  defp dec_huffman_lookup(0, 2) do
    {:more, :undefined, 7}
  end

  defp dec_huffman_lookup(0, 3) do
    {:more, :undefined, 8}
  end

  defp dec_huffman_lookup(0, 4) do
    {:more, :undefined, 11}
  end

  defp dec_huffman_lookup(0, 5) do
    {:more, :undefined, 12}
  end

  defp dec_huffman_lookup(0, 6) do
    {:more, :undefined, 16}
  end

  defp dec_huffman_lookup(0, 7) do
    {:more, :undefined, 19}
  end

  defp dec_huffman_lookup(0, 8) do
    {:more, :undefined, 25}
  end

  defp dec_huffman_lookup(0, 9) do
    {:more, :undefined, 28}
  end

  defp dec_huffman_lookup(0, 10) do
    {:more, :undefined, 32}
  end

  defp dec_huffman_lookup(0, 11) do
    {:more, :undefined, 35}
  end

  defp dec_huffman_lookup(0, 12) do
    {:more, :undefined, 42}
  end

  defp dec_huffman_lookup(0, 13) do
    {:more, :undefined, 49}
  end

  defp dec_huffman_lookup(0, 14) do
    {:more, :undefined, 57}
  end

  defp dec_huffman_lookup(0, 15) do
    {:ok, :undefined, 64}
  end

  defp dec_huffman_lookup(1, 0) do
    {:ok, 48, 0}
  end

  defp dec_huffman_lookup(1, 1) do
    {:ok, 49, 0}
  end

  defp dec_huffman_lookup(1, 2) do
    {:ok, 50, 0}
  end

  defp dec_huffman_lookup(1, 3) do
    {:ok, 97, 0}
  end

  defp dec_huffman_lookup(1, 4) do
    {:ok, 99, 0}
  end

  defp dec_huffman_lookup(1, 5) do
    {:ok, 101, 0}
  end

  defp dec_huffman_lookup(1, 6) do
    {:ok, 105, 0}
  end

  defp dec_huffman_lookup(1, 7) do
    {:ok, 111, 0}
  end

  defp dec_huffman_lookup(1, 8) do
    {:ok, 115, 0}
  end

  defp dec_huffman_lookup(1, 9) do
    {:ok, 116, 0}
  end

  defp dec_huffman_lookup(1, 10) do
    {:more, :undefined, 13}
  end

  defp dec_huffman_lookup(1, 11) do
    {:more, :undefined, 14}
  end

  defp dec_huffman_lookup(1, 12) do
    {:more, :undefined, 17}
  end

  defp dec_huffman_lookup(1, 13) do
    {:more, :undefined, 18}
  end

  defp dec_huffman_lookup(1, 14) do
    {:more, :undefined, 20}
  end

  defp dec_huffman_lookup(1, 15) do
    {:more, :undefined, 21}
  end

  defp dec_huffman_lookup(2, 0) do
    {:more, 48, 1}
  end

  defp dec_huffman_lookup(2, 1) do
    {:ok, 48, 22}
  end

  defp dec_huffman_lookup(2, 2) do
    {:more, 49, 1}
  end

  defp dec_huffman_lookup(2, 3) do
    {:ok, 49, 22}
  end

  defp dec_huffman_lookup(2, 4) do
    {:more, 50, 1}
  end

  defp dec_huffman_lookup(2, 5) do
    {:ok, 50, 22}
  end

  defp dec_huffman_lookup(2, 6) do
    {:more, 97, 1}
  end

  defp dec_huffman_lookup(2, 7) do
    {:ok, 97, 22}
  end

  defp dec_huffman_lookup(2, 8) do
    {:more, 99, 1}
  end

  defp dec_huffman_lookup(2, 9) do
    {:ok, 99, 22}
  end

  defp dec_huffman_lookup(2, 10) do
    {:more, 101, 1}
  end

  defp dec_huffman_lookup(2, 11) do
    {:ok, 101, 22}
  end

  defp dec_huffman_lookup(2, 12) do
    {:more, 105, 1}
  end

  defp dec_huffman_lookup(2, 13) do
    {:ok, 105, 22}
  end

  defp dec_huffman_lookup(2, 14) do
    {:more, 111, 1}
  end

  defp dec_huffman_lookup(2, 15) do
    {:ok, 111, 22}
  end

  defp dec_huffman_lookup(3, 0) do
    {:more, 48, 2}
  end

  defp dec_huffman_lookup(3, 1) do
    {:more, 48, 9}
  end

  defp dec_huffman_lookup(3, 2) do
    {:more, 48, 23}
  end

  defp dec_huffman_lookup(3, 3) do
    {:ok, 48, 40}
  end

  defp dec_huffman_lookup(3, 4) do
    {:more, 49, 2}
  end

  defp dec_huffman_lookup(3, 5) do
    {:more, 49, 9}
  end

  defp dec_huffman_lookup(3, 6) do
    {:more, 49, 23}
  end

  defp dec_huffman_lookup(3, 7) do
    {:ok, 49, 40}
  end

  defp dec_huffman_lookup(3, 8) do
    {:more, 50, 2}
  end

  defp dec_huffman_lookup(3, 9) do
    {:more, 50, 9}
  end

  defp dec_huffman_lookup(3, 10) do
    {:more, 50, 23}
  end

  defp dec_huffman_lookup(3, 11) do
    {:ok, 50, 40}
  end

  defp dec_huffman_lookup(3, 12) do
    {:more, 97, 2}
  end

  defp dec_huffman_lookup(3, 13) do
    {:more, 97, 9}
  end

  defp dec_huffman_lookup(3, 14) do
    {:more, 97, 23}
  end

  defp dec_huffman_lookup(3, 15) do
    {:ok, 97, 40}
  end

  defp dec_huffman_lookup(4, 0) do
    {:more, 48, 3}
  end

  defp dec_huffman_lookup(4, 1) do
    {:more, 48, 6}
  end

  defp dec_huffman_lookup(4, 2) do
    {:more, 48, 10}
  end

  defp dec_huffman_lookup(4, 3) do
    {:more, 48, 15}
  end

  defp dec_huffman_lookup(4, 4) do
    {:more, 48, 24}
  end

  defp dec_huffman_lookup(4, 5) do
    {:more, 48, 31}
  end

  defp dec_huffman_lookup(4, 6) do
    {:more, 48, 41}
  end

  defp dec_huffman_lookup(4, 7) do
    {:ok, 48, 56}
  end

  defp dec_huffman_lookup(4, 8) do
    {:more, 49, 3}
  end

  defp dec_huffman_lookup(4, 9) do
    {:more, 49, 6}
  end

  defp dec_huffman_lookup(4, 10) do
    {:more, 49, 10}
  end

  defp dec_huffman_lookup(4, 11) do
    {:more, 49, 15}
  end

  defp dec_huffman_lookup(4, 12) do
    {:more, 49, 24}
  end

  defp dec_huffman_lookup(4, 13) do
    {:more, 49, 31}
  end

  defp dec_huffman_lookup(4, 14) do
    {:more, 49, 41}
  end

  defp dec_huffman_lookup(4, 15) do
    {:ok, 49, 56}
  end

  defp dec_huffman_lookup(5, 0) do
    {:more, 50, 3}
  end

  defp dec_huffman_lookup(5, 1) do
    {:more, 50, 6}
  end

  defp dec_huffman_lookup(5, 2) do
    {:more, 50, 10}
  end

  defp dec_huffman_lookup(5, 3) do
    {:more, 50, 15}
  end

  defp dec_huffman_lookup(5, 4) do
    {:more, 50, 24}
  end

  defp dec_huffman_lookup(5, 5) do
    {:more, 50, 31}
  end

  defp dec_huffman_lookup(5, 6) do
    {:more, 50, 41}
  end

  defp dec_huffman_lookup(5, 7) do
    {:ok, 50, 56}
  end

  defp dec_huffman_lookup(5, 8) do
    {:more, 97, 3}
  end

  defp dec_huffman_lookup(5, 9) do
    {:more, 97, 6}
  end

  defp dec_huffman_lookup(5, 10) do
    {:more, 97, 10}
  end

  defp dec_huffman_lookup(5, 11) do
    {:more, 97, 15}
  end

  defp dec_huffman_lookup(5, 12) do
    {:more, 97, 24}
  end

  defp dec_huffman_lookup(5, 13) do
    {:more, 97, 31}
  end

  defp dec_huffman_lookup(5, 14) do
    {:more, 97, 41}
  end

  defp dec_huffman_lookup(5, 15) do
    {:ok, 97, 56}
  end

  defp dec_huffman_lookup(6, 0) do
    {:more, 99, 2}
  end

  defp dec_huffman_lookup(6, 1) do
    {:more, 99, 9}
  end

  defp dec_huffman_lookup(6, 2) do
    {:more, 99, 23}
  end

  defp dec_huffman_lookup(6, 3) do
    {:ok, 99, 40}
  end

  defp dec_huffman_lookup(6, 4) do
    {:more, 101, 2}
  end

  defp dec_huffman_lookup(6, 5) do
    {:more, 101, 9}
  end

  defp dec_huffman_lookup(6, 6) do
    {:more, 101, 23}
  end

  defp dec_huffman_lookup(6, 7) do
    {:ok, 101, 40}
  end

  defp dec_huffman_lookup(6, 8) do
    {:more, 105, 2}
  end

  defp dec_huffman_lookup(6, 9) do
    {:more, 105, 9}
  end

  defp dec_huffman_lookup(6, 10) do
    {:more, 105, 23}
  end

  defp dec_huffman_lookup(6, 11) do
    {:ok, 105, 40}
  end

  defp dec_huffman_lookup(6, 12) do
    {:more, 111, 2}
  end

  defp dec_huffman_lookup(6, 13) do
    {:more, 111, 9}
  end

  defp dec_huffman_lookup(6, 14) do
    {:more, 111, 23}
  end

  defp dec_huffman_lookup(6, 15) do
    {:ok, 111, 40}
  end

  defp dec_huffman_lookup(7, 0) do
    {:more, 99, 3}
  end

  defp dec_huffman_lookup(7, 1) do
    {:more, 99, 6}
  end

  defp dec_huffman_lookup(7, 2) do
    {:more, 99, 10}
  end

  defp dec_huffman_lookup(7, 3) do
    {:more, 99, 15}
  end

  defp dec_huffman_lookup(7, 4) do
    {:more, 99, 24}
  end

  defp dec_huffman_lookup(7, 5) do
    {:more, 99, 31}
  end

  defp dec_huffman_lookup(7, 6) do
    {:more, 99, 41}
  end

  defp dec_huffman_lookup(7, 7) do
    {:ok, 99, 56}
  end

  defp dec_huffman_lookup(7, 8) do
    {:more, 101, 3}
  end

  defp dec_huffman_lookup(7, 9) do
    {:more, 101, 6}
  end

  defp dec_huffman_lookup(7, 10) do
    {:more, 101, 10}
  end

  defp dec_huffman_lookup(7, 11) do
    {:more, 101, 15}
  end

  defp dec_huffman_lookup(7, 12) do
    {:more, 101, 24}
  end

  defp dec_huffman_lookup(7, 13) do
    {:more, 101, 31}
  end

  defp dec_huffman_lookup(7, 14) do
    {:more, 101, 41}
  end

  defp dec_huffman_lookup(7, 15) do
    {:ok, 101, 56}
  end

  defp dec_huffman_lookup(8, 0) do
    {:more, 105, 3}
  end

  defp dec_huffman_lookup(8, 1) do
    {:more, 105, 6}
  end

  defp dec_huffman_lookup(8, 2) do
    {:more, 105, 10}
  end

  defp dec_huffman_lookup(8, 3) do
    {:more, 105, 15}
  end

  defp dec_huffman_lookup(8, 4) do
    {:more, 105, 24}
  end

  defp dec_huffman_lookup(8, 5) do
    {:more, 105, 31}
  end

  defp dec_huffman_lookup(8, 6) do
    {:more, 105, 41}
  end

  defp dec_huffman_lookup(8, 7) do
    {:ok, 105, 56}
  end

  defp dec_huffman_lookup(8, 8) do
    {:more, 111, 3}
  end

  defp dec_huffman_lookup(8, 9) do
    {:more, 111, 6}
  end

  defp dec_huffman_lookup(8, 10) do
    {:more, 111, 10}
  end

  defp dec_huffman_lookup(8, 11) do
    {:more, 111, 15}
  end

  defp dec_huffman_lookup(8, 12) do
    {:more, 111, 24}
  end

  defp dec_huffman_lookup(8, 13) do
    {:more, 111, 31}
  end

  defp dec_huffman_lookup(8, 14) do
    {:more, 111, 41}
  end

  defp dec_huffman_lookup(8, 15) do
    {:ok, 111, 56}
  end

  defp dec_huffman_lookup(9, 0) do
    {:more, 115, 1}
  end

  defp dec_huffman_lookup(9, 1) do
    {:ok, 115, 22}
  end

  defp dec_huffman_lookup(9, 2) do
    {:more, 116, 1}
  end

  defp dec_huffman_lookup(9, 3) do
    {:ok, 116, 22}
  end

  defp dec_huffman_lookup(9, 4) do
    {:ok, 32, 0}
  end

  defp dec_huffman_lookup(9, 5) do
    {:ok, 37, 0}
  end

  defp dec_huffman_lookup(9, 6) do
    {:ok, 45, 0}
  end

  defp dec_huffman_lookup(9, 7) do
    {:ok, 46, 0}
  end

  defp dec_huffman_lookup(9, 8) do
    {:ok, 47, 0}
  end

  defp dec_huffman_lookup(9, 9) do
    {:ok, 51, 0}
  end

  defp dec_huffman_lookup(9, 10) do
    {:ok, 52, 0}
  end

  defp dec_huffman_lookup(9, 11) do
    {:ok, 53, 0}
  end

  defp dec_huffman_lookup(9, 12) do
    {:ok, 54, 0}
  end

  defp dec_huffman_lookup(9, 13) do
    {:ok, 55, 0}
  end

  defp dec_huffman_lookup(9, 14) do
    {:ok, 56, 0}
  end

  defp dec_huffman_lookup(9, 15) do
    {:ok, 57, 0}
  end

  defp dec_huffman_lookup(10, 0) do
    {:more, 115, 2}
  end

  defp dec_huffman_lookup(10, 1) do
    {:more, 115, 9}
  end

  defp dec_huffman_lookup(10, 2) do
    {:more, 115, 23}
  end

  defp dec_huffman_lookup(10, 3) do
    {:ok, 115, 40}
  end

  defp dec_huffman_lookup(10, 4) do
    {:more, 116, 2}
  end

  defp dec_huffman_lookup(10, 5) do
    {:more, 116, 9}
  end

  defp dec_huffman_lookup(10, 6) do
    {:more, 116, 23}
  end

  defp dec_huffman_lookup(10, 7) do
    {:ok, 116, 40}
  end

  defp dec_huffman_lookup(10, 8) do
    {:more, 32, 1}
  end

  defp dec_huffman_lookup(10, 9) do
    {:ok, 32, 22}
  end

  defp dec_huffman_lookup(10, 10) do
    {:more, 37, 1}
  end

  defp dec_huffman_lookup(10, 11) do
    {:ok, 37, 22}
  end

  defp dec_huffman_lookup(10, 12) do
    {:more, 45, 1}
  end

  defp dec_huffman_lookup(10, 13) do
    {:ok, 45, 22}
  end

  defp dec_huffman_lookup(10, 14) do
    {:more, 46, 1}
  end

  defp dec_huffman_lookup(10, 15) do
    {:ok, 46, 22}
  end

  defp dec_huffman_lookup(11, 0) do
    {:more, 115, 3}
  end

  defp dec_huffman_lookup(11, 1) do
    {:more, 115, 6}
  end

  defp dec_huffman_lookup(11, 2) do
    {:more, 115, 10}
  end

  defp dec_huffman_lookup(11, 3) do
    {:more, 115, 15}
  end

  defp dec_huffman_lookup(11, 4) do
    {:more, 115, 24}
  end

  defp dec_huffman_lookup(11, 5) do
    {:more, 115, 31}
  end

  defp dec_huffman_lookup(11, 6) do
    {:more, 115, 41}
  end

  defp dec_huffman_lookup(11, 7) do
    {:ok, 115, 56}
  end

  defp dec_huffman_lookup(11, 8) do
    {:more, 116, 3}
  end

  defp dec_huffman_lookup(11, 9) do
    {:more, 116, 6}
  end

  defp dec_huffman_lookup(11, 10) do
    {:more, 116, 10}
  end

  defp dec_huffman_lookup(11, 11) do
    {:more, 116, 15}
  end

  defp dec_huffman_lookup(11, 12) do
    {:more, 116, 24}
  end

  defp dec_huffman_lookup(11, 13) do
    {:more, 116, 31}
  end

  defp dec_huffman_lookup(11, 14) do
    {:more, 116, 41}
  end

  defp dec_huffman_lookup(11, 15) do
    {:ok, 116, 56}
  end

  defp dec_huffman_lookup(12, 0) do
    {:more, 32, 2}
  end

  defp dec_huffman_lookup(12, 1) do
    {:more, 32, 9}
  end

  defp dec_huffman_lookup(12, 2) do
    {:more, 32, 23}
  end

  defp dec_huffman_lookup(12, 3) do
    {:ok, 32, 40}
  end

  defp dec_huffman_lookup(12, 4) do
    {:more, 37, 2}
  end

  defp dec_huffman_lookup(12, 5) do
    {:more, 37, 9}
  end

  defp dec_huffman_lookup(12, 6) do
    {:more, 37, 23}
  end

  defp dec_huffman_lookup(12, 7) do
    {:ok, 37, 40}
  end

  defp dec_huffman_lookup(12, 8) do
    {:more, 45, 2}
  end

  defp dec_huffman_lookup(12, 9) do
    {:more, 45, 9}
  end

  defp dec_huffman_lookup(12, 10) do
    {:more, 45, 23}
  end

  defp dec_huffman_lookup(12, 11) do
    {:ok, 45, 40}
  end

  defp dec_huffman_lookup(12, 12) do
    {:more, 46, 2}
  end

  defp dec_huffman_lookup(12, 13) do
    {:more, 46, 9}
  end

  defp dec_huffman_lookup(12, 14) do
    {:more, 46, 23}
  end

  defp dec_huffman_lookup(12, 15) do
    {:ok, 46, 40}
  end

  defp dec_huffman_lookup(13, 0) do
    {:more, 32, 3}
  end

  defp dec_huffman_lookup(13, 1) do
    {:more, 32, 6}
  end

  defp dec_huffman_lookup(13, 2) do
    {:more, 32, 10}
  end

  defp dec_huffman_lookup(13, 3) do
    {:more, 32, 15}
  end

  defp dec_huffman_lookup(13, 4) do
    {:more, 32, 24}
  end

  defp dec_huffman_lookup(13, 5) do
    {:more, 32, 31}
  end

  defp dec_huffman_lookup(13, 6) do
    {:more, 32, 41}
  end

  defp dec_huffman_lookup(13, 7) do
    {:ok, 32, 56}
  end

  defp dec_huffman_lookup(13, 8) do
    {:more, 37, 3}
  end

  defp dec_huffman_lookup(13, 9) do
    {:more, 37, 6}
  end

  defp dec_huffman_lookup(13, 10) do
    {:more, 37, 10}
  end

  defp dec_huffman_lookup(13, 11) do
    {:more, 37, 15}
  end

  defp dec_huffman_lookup(13, 12) do
    {:more, 37, 24}
  end

  defp dec_huffman_lookup(13, 13) do
    {:more, 37, 31}
  end

  defp dec_huffman_lookup(13, 14) do
    {:more, 37, 41}
  end

  defp dec_huffman_lookup(13, 15) do
    {:ok, 37, 56}
  end

  defp dec_huffman_lookup(14, 0) do
    {:more, 45, 3}
  end

  defp dec_huffman_lookup(14, 1) do
    {:more, 45, 6}
  end

  defp dec_huffman_lookup(14, 2) do
    {:more, 45, 10}
  end

  defp dec_huffman_lookup(14, 3) do
    {:more, 45, 15}
  end

  defp dec_huffman_lookup(14, 4) do
    {:more, 45, 24}
  end

  defp dec_huffman_lookup(14, 5) do
    {:more, 45, 31}
  end

  defp dec_huffman_lookup(14, 6) do
    {:more, 45, 41}
  end

  defp dec_huffman_lookup(14, 7) do
    {:ok, 45, 56}
  end

  defp dec_huffman_lookup(14, 8) do
    {:more, 46, 3}
  end

  defp dec_huffman_lookup(14, 9) do
    {:more, 46, 6}
  end

  defp dec_huffman_lookup(14, 10) do
    {:more, 46, 10}
  end

  defp dec_huffman_lookup(14, 11) do
    {:more, 46, 15}
  end

  defp dec_huffman_lookup(14, 12) do
    {:more, 46, 24}
  end

  defp dec_huffman_lookup(14, 13) do
    {:more, 46, 31}
  end

  defp dec_huffman_lookup(14, 14) do
    {:more, 46, 41}
  end

  defp dec_huffman_lookup(14, 15) do
    {:ok, 46, 56}
  end

  defp dec_huffman_lookup(15, 0) do
    {:more, 47, 1}
  end

  defp dec_huffman_lookup(15, 1) do
    {:ok, 47, 22}
  end

  defp dec_huffman_lookup(15, 2) do
    {:more, 51, 1}
  end

  defp dec_huffman_lookup(15, 3) do
    {:ok, 51, 22}
  end

  defp dec_huffman_lookup(15, 4) do
    {:more, 52, 1}
  end

  defp dec_huffman_lookup(15, 5) do
    {:ok, 52, 22}
  end

  defp dec_huffman_lookup(15, 6) do
    {:more, 53, 1}
  end

  defp dec_huffman_lookup(15, 7) do
    {:ok, 53, 22}
  end

  defp dec_huffman_lookup(15, 8) do
    {:more, 54, 1}
  end

  defp dec_huffman_lookup(15, 9) do
    {:ok, 54, 22}
  end

  defp dec_huffman_lookup(15, 10) do
    {:more, 55, 1}
  end

  defp dec_huffman_lookup(15, 11) do
    {:ok, 55, 22}
  end

  defp dec_huffman_lookup(15, 12) do
    {:more, 56, 1}
  end

  defp dec_huffman_lookup(15, 13) do
    {:ok, 56, 22}
  end

  defp dec_huffman_lookup(15, 14) do
    {:more, 57, 1}
  end

  defp dec_huffman_lookup(15, 15) do
    {:ok, 57, 22}
  end

  defp dec_huffman_lookup(16, 0) do
    {:more, 47, 2}
  end

  defp dec_huffman_lookup(16, 1) do
    {:more, 47, 9}
  end

  defp dec_huffman_lookup(16, 2) do
    {:more, 47, 23}
  end

  defp dec_huffman_lookup(16, 3) do
    {:ok, 47, 40}
  end

  defp dec_huffman_lookup(16, 4) do
    {:more, 51, 2}
  end

  defp dec_huffman_lookup(16, 5) do
    {:more, 51, 9}
  end

  defp dec_huffman_lookup(16, 6) do
    {:more, 51, 23}
  end

  defp dec_huffman_lookup(16, 7) do
    {:ok, 51, 40}
  end

  defp dec_huffman_lookup(16, 8) do
    {:more, 52, 2}
  end

  defp dec_huffman_lookup(16, 9) do
    {:more, 52, 9}
  end

  defp dec_huffman_lookup(16, 10) do
    {:more, 52, 23}
  end

  defp dec_huffman_lookup(16, 11) do
    {:ok, 52, 40}
  end

  defp dec_huffman_lookup(16, 12) do
    {:more, 53, 2}
  end

  defp dec_huffman_lookup(16, 13) do
    {:more, 53, 9}
  end

  defp dec_huffman_lookup(16, 14) do
    {:more, 53, 23}
  end

  defp dec_huffman_lookup(16, 15) do
    {:ok, 53, 40}
  end

  defp dec_huffman_lookup(17, 0) do
    {:more, 47, 3}
  end

  defp dec_huffman_lookup(17, 1) do
    {:more, 47, 6}
  end

  defp dec_huffman_lookup(17, 2) do
    {:more, 47, 10}
  end

  defp dec_huffman_lookup(17, 3) do
    {:more, 47, 15}
  end

  defp dec_huffman_lookup(17, 4) do
    {:more, 47, 24}
  end

  defp dec_huffman_lookup(17, 5) do
    {:more, 47, 31}
  end

  defp dec_huffman_lookup(17, 6) do
    {:more, 47, 41}
  end

  defp dec_huffman_lookup(17, 7) do
    {:ok, 47, 56}
  end

  defp dec_huffman_lookup(17, 8) do
    {:more, 51, 3}
  end

  defp dec_huffman_lookup(17, 9) do
    {:more, 51, 6}
  end

  defp dec_huffman_lookup(17, 10) do
    {:more, 51, 10}
  end

  defp dec_huffman_lookup(17, 11) do
    {:more, 51, 15}
  end

  defp dec_huffman_lookup(17, 12) do
    {:more, 51, 24}
  end

  defp dec_huffman_lookup(17, 13) do
    {:more, 51, 31}
  end

  defp dec_huffman_lookup(17, 14) do
    {:more, 51, 41}
  end

  defp dec_huffman_lookup(17, 15) do
    {:ok, 51, 56}
  end

  defp dec_huffman_lookup(18, 0) do
    {:more, 52, 3}
  end

  defp dec_huffman_lookup(18, 1) do
    {:more, 52, 6}
  end

  defp dec_huffman_lookup(18, 2) do
    {:more, 52, 10}
  end

  defp dec_huffman_lookup(18, 3) do
    {:more, 52, 15}
  end

  defp dec_huffman_lookup(18, 4) do
    {:more, 52, 24}
  end

  defp dec_huffman_lookup(18, 5) do
    {:more, 52, 31}
  end

  defp dec_huffman_lookup(18, 6) do
    {:more, 52, 41}
  end

  defp dec_huffman_lookup(18, 7) do
    {:ok, 52, 56}
  end

  defp dec_huffman_lookup(18, 8) do
    {:more, 53, 3}
  end

  defp dec_huffman_lookup(18, 9) do
    {:more, 53, 6}
  end

  defp dec_huffman_lookup(18, 10) do
    {:more, 53, 10}
  end

  defp dec_huffman_lookup(18, 11) do
    {:more, 53, 15}
  end

  defp dec_huffman_lookup(18, 12) do
    {:more, 53, 24}
  end

  defp dec_huffman_lookup(18, 13) do
    {:more, 53, 31}
  end

  defp dec_huffman_lookup(18, 14) do
    {:more, 53, 41}
  end

  defp dec_huffman_lookup(18, 15) do
    {:ok, 53, 56}
  end

  defp dec_huffman_lookup(19, 0) do
    {:more, 54, 2}
  end

  defp dec_huffman_lookup(19, 1) do
    {:more, 54, 9}
  end

  defp dec_huffman_lookup(19, 2) do
    {:more, 54, 23}
  end

  defp dec_huffman_lookup(19, 3) do
    {:ok, 54, 40}
  end

  defp dec_huffman_lookup(19, 4) do
    {:more, 55, 2}
  end

  defp dec_huffman_lookup(19, 5) do
    {:more, 55, 9}
  end

  defp dec_huffman_lookup(19, 6) do
    {:more, 55, 23}
  end

  defp dec_huffman_lookup(19, 7) do
    {:ok, 55, 40}
  end

  defp dec_huffman_lookup(19, 8) do
    {:more, 56, 2}
  end

  defp dec_huffman_lookup(19, 9) do
    {:more, 56, 9}
  end

  defp dec_huffman_lookup(19, 10) do
    {:more, 56, 23}
  end

  defp dec_huffman_lookup(19, 11) do
    {:ok, 56, 40}
  end

  defp dec_huffman_lookup(19, 12) do
    {:more, 57, 2}
  end

  defp dec_huffman_lookup(19, 13) do
    {:more, 57, 9}
  end

  defp dec_huffman_lookup(19, 14) do
    {:more, 57, 23}
  end

  defp dec_huffman_lookup(19, 15) do
    {:ok, 57, 40}
  end

  defp dec_huffman_lookup(20, 0) do
    {:more, 54, 3}
  end

  defp dec_huffman_lookup(20, 1) do
    {:more, 54, 6}
  end

  defp dec_huffman_lookup(20, 2) do
    {:more, 54, 10}
  end

  defp dec_huffman_lookup(20, 3) do
    {:more, 54, 15}
  end

  defp dec_huffman_lookup(20, 4) do
    {:more, 54, 24}
  end

  defp dec_huffman_lookup(20, 5) do
    {:more, 54, 31}
  end

  defp dec_huffman_lookup(20, 6) do
    {:more, 54, 41}
  end

  defp dec_huffman_lookup(20, 7) do
    {:ok, 54, 56}
  end

  defp dec_huffman_lookup(20, 8) do
    {:more, 55, 3}
  end

  defp dec_huffman_lookup(20, 9) do
    {:more, 55, 6}
  end

  defp dec_huffman_lookup(20, 10) do
    {:more, 55, 10}
  end

  defp dec_huffman_lookup(20, 11) do
    {:more, 55, 15}
  end

  defp dec_huffman_lookup(20, 12) do
    {:more, 55, 24}
  end

  defp dec_huffman_lookup(20, 13) do
    {:more, 55, 31}
  end

  defp dec_huffman_lookup(20, 14) do
    {:more, 55, 41}
  end

  defp dec_huffman_lookup(20, 15) do
    {:ok, 55, 56}
  end

  defp dec_huffman_lookup(21, 0) do
    {:more, 56, 3}
  end

  defp dec_huffman_lookup(21, 1) do
    {:more, 56, 6}
  end

  defp dec_huffman_lookup(21, 2) do
    {:more, 56, 10}
  end

  defp dec_huffman_lookup(21, 3) do
    {:more, 56, 15}
  end

  defp dec_huffman_lookup(21, 4) do
    {:more, 56, 24}
  end

  defp dec_huffman_lookup(21, 5) do
    {:more, 56, 31}
  end

  defp dec_huffman_lookup(21, 6) do
    {:more, 56, 41}
  end

  defp dec_huffman_lookup(21, 7) do
    {:ok, 56, 56}
  end

  defp dec_huffman_lookup(21, 8) do
    {:more, 57, 3}
  end

  defp dec_huffman_lookup(21, 9) do
    {:more, 57, 6}
  end

  defp dec_huffman_lookup(21, 10) do
    {:more, 57, 10}
  end

  defp dec_huffman_lookup(21, 11) do
    {:more, 57, 15}
  end

  defp dec_huffman_lookup(21, 12) do
    {:more, 57, 24}
  end

  defp dec_huffman_lookup(21, 13) do
    {:more, 57, 31}
  end

  defp dec_huffman_lookup(21, 14) do
    {:more, 57, 41}
  end

  defp dec_huffman_lookup(21, 15) do
    {:ok, 57, 56}
  end

  defp dec_huffman_lookup(22, 0) do
    {:more, :undefined, 26}
  end

  defp dec_huffman_lookup(22, 1) do
    {:more, :undefined, 27}
  end

  defp dec_huffman_lookup(22, 2) do
    {:more, :undefined, 29}
  end

  defp dec_huffman_lookup(22, 3) do
    {:more, :undefined, 30}
  end

  defp dec_huffman_lookup(22, 4) do
    {:more, :undefined, 33}
  end

  defp dec_huffman_lookup(22, 5) do
    {:more, :undefined, 34}
  end

  defp dec_huffman_lookup(22, 6) do
    {:more, :undefined, 36}
  end

  defp dec_huffman_lookup(22, 7) do
    {:more, :undefined, 37}
  end

  defp dec_huffman_lookup(22, 8) do
    {:more, :undefined, 43}
  end

  defp dec_huffman_lookup(22, 9) do
    {:more, :undefined, 46}
  end

  defp dec_huffman_lookup(22, 10) do
    {:more, :undefined, 50}
  end

  defp dec_huffman_lookup(22, 11) do
    {:more, :undefined, 53}
  end

  defp dec_huffman_lookup(22, 12) do
    {:more, :undefined, 58}
  end

  defp dec_huffman_lookup(22, 13) do
    {:more, :undefined, 61}
  end

  defp dec_huffman_lookup(22, 14) do
    {:more, :undefined, 65}
  end

  defp dec_huffman_lookup(22, 15) do
    {:ok, :undefined, 68}
  end

  defp dec_huffman_lookup(23, 0) do
    {:ok, 61, 0}
  end

  defp dec_huffman_lookup(23, 1) do
    {:ok, 65, 0}
  end

  defp dec_huffman_lookup(23, 2) do
    {:ok, 95, 0}
  end

  defp dec_huffman_lookup(23, 3) do
    {:ok, 98, 0}
  end

  defp dec_huffman_lookup(23, 4) do
    {:ok, 100, 0}
  end

  defp dec_huffman_lookup(23, 5) do
    {:ok, 102, 0}
  end

  defp dec_huffman_lookup(23, 6) do
    {:ok, 103, 0}
  end

  defp dec_huffman_lookup(23, 7) do
    {:ok, 104, 0}
  end

  defp dec_huffman_lookup(23, 8) do
    {:ok, 108, 0}
  end

  defp dec_huffman_lookup(23, 9) do
    {:ok, 109, 0}
  end

  defp dec_huffman_lookup(23, 10) do
    {:ok, 110, 0}
  end

  defp dec_huffman_lookup(23, 11) do
    {:ok, 112, 0}
  end

  defp dec_huffman_lookup(23, 12) do
    {:ok, 114, 0}
  end

  defp dec_huffman_lookup(23, 13) do
    {:ok, 117, 0}
  end

  defp dec_huffman_lookup(23, 14) do
    {:more, :undefined, 38}
  end

  defp dec_huffman_lookup(23, 15) do
    {:more, :undefined, 39}
  end

  defp dec_huffman_lookup(24, 0) do
    {:more, 61, 1}
  end

  defp dec_huffman_lookup(24, 1) do
    {:ok, 61, 22}
  end

  defp dec_huffman_lookup(24, 2) do
    {:more, 65, 1}
  end

  defp dec_huffman_lookup(24, 3) do
    {:ok, 65, 22}
  end

  defp dec_huffman_lookup(24, 4) do
    {:more, 95, 1}
  end

  defp dec_huffman_lookup(24, 5) do
    {:ok, 95, 22}
  end

  defp dec_huffman_lookup(24, 6) do
    {:more, 98, 1}
  end

  defp dec_huffman_lookup(24, 7) do
    {:ok, 98, 22}
  end

  defp dec_huffman_lookup(24, 8) do
    {:more, 100, 1}
  end

  defp dec_huffman_lookup(24, 9) do
    {:ok, 100, 22}
  end

  defp dec_huffman_lookup(24, 10) do
    {:more, 102, 1}
  end

  defp dec_huffman_lookup(24, 11) do
    {:ok, 102, 22}
  end

  defp dec_huffman_lookup(24, 12) do
    {:more, 103, 1}
  end

  defp dec_huffman_lookup(24, 13) do
    {:ok, 103, 22}
  end

  defp dec_huffman_lookup(24, 14) do
    {:more, 104, 1}
  end

  defp dec_huffman_lookup(24, 15) do
    {:ok, 104, 22}
  end

  defp dec_huffman_lookup(25, 0) do
    {:more, 61, 2}
  end

  defp dec_huffman_lookup(25, 1) do
    {:more, 61, 9}
  end

  defp dec_huffman_lookup(25, 2) do
    {:more, 61, 23}
  end

  defp dec_huffman_lookup(25, 3) do
    {:ok, 61, 40}
  end

  defp dec_huffman_lookup(25, 4) do
    {:more, 65, 2}
  end

  defp dec_huffman_lookup(25, 5) do
    {:more, 65, 9}
  end

  defp dec_huffman_lookup(25, 6) do
    {:more, 65, 23}
  end

  defp dec_huffman_lookup(25, 7) do
    {:ok, 65, 40}
  end

  defp dec_huffman_lookup(25, 8) do
    {:more, 95, 2}
  end

  defp dec_huffman_lookup(25, 9) do
    {:more, 95, 9}
  end

  defp dec_huffman_lookup(25, 10) do
    {:more, 95, 23}
  end

  defp dec_huffman_lookup(25, 11) do
    {:ok, 95, 40}
  end

  defp dec_huffman_lookup(25, 12) do
    {:more, 98, 2}
  end

  defp dec_huffman_lookup(25, 13) do
    {:more, 98, 9}
  end

  defp dec_huffman_lookup(25, 14) do
    {:more, 98, 23}
  end

  defp dec_huffman_lookup(25, 15) do
    {:ok, 98, 40}
  end

  defp dec_huffman_lookup(26, 0) do
    {:more, 61, 3}
  end

  defp dec_huffman_lookup(26, 1) do
    {:more, 61, 6}
  end

  defp dec_huffman_lookup(26, 2) do
    {:more, 61, 10}
  end

  defp dec_huffman_lookup(26, 3) do
    {:more, 61, 15}
  end

  defp dec_huffman_lookup(26, 4) do
    {:more, 61, 24}
  end

  defp dec_huffman_lookup(26, 5) do
    {:more, 61, 31}
  end

  defp dec_huffman_lookup(26, 6) do
    {:more, 61, 41}
  end

  defp dec_huffman_lookup(26, 7) do
    {:ok, 61, 56}
  end

  defp dec_huffman_lookup(26, 8) do
    {:more, 65, 3}
  end

  defp dec_huffman_lookup(26, 9) do
    {:more, 65, 6}
  end

  defp dec_huffman_lookup(26, 10) do
    {:more, 65, 10}
  end

  defp dec_huffman_lookup(26, 11) do
    {:more, 65, 15}
  end

  defp dec_huffman_lookup(26, 12) do
    {:more, 65, 24}
  end

  defp dec_huffman_lookup(26, 13) do
    {:more, 65, 31}
  end

  defp dec_huffman_lookup(26, 14) do
    {:more, 65, 41}
  end

  defp dec_huffman_lookup(26, 15) do
    {:ok, 65, 56}
  end

  defp dec_huffman_lookup(27, 0) do
    {:more, 95, 3}
  end

  defp dec_huffman_lookup(27, 1) do
    {:more, 95, 6}
  end

  defp dec_huffman_lookup(27, 2) do
    {:more, 95, 10}
  end

  defp dec_huffman_lookup(27, 3) do
    {:more, 95, 15}
  end

  defp dec_huffman_lookup(27, 4) do
    {:more, 95, 24}
  end

  defp dec_huffman_lookup(27, 5) do
    {:more, 95, 31}
  end

  defp dec_huffman_lookup(27, 6) do
    {:more, 95, 41}
  end

  defp dec_huffman_lookup(27, 7) do
    {:ok, 95, 56}
  end

  defp dec_huffman_lookup(27, 8) do
    {:more, 98, 3}
  end

  defp dec_huffman_lookup(27, 9) do
    {:more, 98, 6}
  end

  defp dec_huffman_lookup(27, 10) do
    {:more, 98, 10}
  end

  defp dec_huffman_lookup(27, 11) do
    {:more, 98, 15}
  end

  defp dec_huffman_lookup(27, 12) do
    {:more, 98, 24}
  end

  defp dec_huffman_lookup(27, 13) do
    {:more, 98, 31}
  end

  defp dec_huffman_lookup(27, 14) do
    {:more, 98, 41}
  end

  defp dec_huffman_lookup(27, 15) do
    {:ok, 98, 56}
  end

  defp dec_huffman_lookup(28, 0) do
    {:more, 100, 2}
  end

  defp dec_huffman_lookup(28, 1) do
    {:more, 100, 9}
  end

  defp dec_huffman_lookup(28, 2) do
    {:more, 100, 23}
  end

  defp dec_huffman_lookup(28, 3) do
    {:ok, 100, 40}
  end

  defp dec_huffman_lookup(28, 4) do
    {:more, 102, 2}
  end

  defp dec_huffman_lookup(28, 5) do
    {:more, 102, 9}
  end

  defp dec_huffman_lookup(28, 6) do
    {:more, 102, 23}
  end

  defp dec_huffman_lookup(28, 7) do
    {:ok, 102, 40}
  end

  defp dec_huffman_lookup(28, 8) do
    {:more, 103, 2}
  end

  defp dec_huffman_lookup(28, 9) do
    {:more, 103, 9}
  end

  defp dec_huffman_lookup(28, 10) do
    {:more, 103, 23}
  end

  defp dec_huffman_lookup(28, 11) do
    {:ok, 103, 40}
  end

  defp dec_huffman_lookup(28, 12) do
    {:more, 104, 2}
  end

  defp dec_huffman_lookup(28, 13) do
    {:more, 104, 9}
  end

  defp dec_huffman_lookup(28, 14) do
    {:more, 104, 23}
  end

  defp dec_huffman_lookup(28, 15) do
    {:ok, 104, 40}
  end

  defp dec_huffman_lookup(29, 0) do
    {:more, 100, 3}
  end

  defp dec_huffman_lookup(29, 1) do
    {:more, 100, 6}
  end

  defp dec_huffman_lookup(29, 2) do
    {:more, 100, 10}
  end

  defp dec_huffman_lookup(29, 3) do
    {:more, 100, 15}
  end

  defp dec_huffman_lookup(29, 4) do
    {:more, 100, 24}
  end

  defp dec_huffman_lookup(29, 5) do
    {:more, 100, 31}
  end

  defp dec_huffman_lookup(29, 6) do
    {:more, 100, 41}
  end

  defp dec_huffman_lookup(29, 7) do
    {:ok, 100, 56}
  end

  defp dec_huffman_lookup(29, 8) do
    {:more, 102, 3}
  end

  defp dec_huffman_lookup(29, 9) do
    {:more, 102, 6}
  end

  defp dec_huffman_lookup(29, 10) do
    {:more, 102, 10}
  end

  defp dec_huffman_lookup(29, 11) do
    {:more, 102, 15}
  end

  defp dec_huffman_lookup(29, 12) do
    {:more, 102, 24}
  end

  defp dec_huffman_lookup(29, 13) do
    {:more, 102, 31}
  end

  defp dec_huffman_lookup(29, 14) do
    {:more, 102, 41}
  end

  defp dec_huffman_lookup(29, 15) do
    {:ok, 102, 56}
  end

  defp dec_huffman_lookup(30, 0) do
    {:more, 103, 3}
  end

  defp dec_huffman_lookup(30, 1) do
    {:more, 103, 6}
  end

  defp dec_huffman_lookup(30, 2) do
    {:more, 103, 10}
  end

  defp dec_huffman_lookup(30, 3) do
    {:more, 103, 15}
  end

  defp dec_huffman_lookup(30, 4) do
    {:more, 103, 24}
  end

  defp dec_huffman_lookup(30, 5) do
    {:more, 103, 31}
  end

  defp dec_huffman_lookup(30, 6) do
    {:more, 103, 41}
  end

  defp dec_huffman_lookup(30, 7) do
    {:ok, 103, 56}
  end

  defp dec_huffman_lookup(30, 8) do
    {:more, 104, 3}
  end

  defp dec_huffman_lookup(30, 9) do
    {:more, 104, 6}
  end

  defp dec_huffman_lookup(30, 10) do
    {:more, 104, 10}
  end

  defp dec_huffman_lookup(30, 11) do
    {:more, 104, 15}
  end

  defp dec_huffman_lookup(30, 12) do
    {:more, 104, 24}
  end

  defp dec_huffman_lookup(30, 13) do
    {:more, 104, 31}
  end

  defp dec_huffman_lookup(30, 14) do
    {:more, 104, 41}
  end

  defp dec_huffman_lookup(30, 15) do
    {:ok, 104, 56}
  end

  defp dec_huffman_lookup(31, 0) do
    {:more, 108, 1}
  end

  defp dec_huffman_lookup(31, 1) do
    {:ok, 108, 22}
  end

  defp dec_huffman_lookup(31, 2) do
    {:more, 109, 1}
  end

  defp dec_huffman_lookup(31, 3) do
    {:ok, 109, 22}
  end

  defp dec_huffman_lookup(31, 4) do
    {:more, 110, 1}
  end

  defp dec_huffman_lookup(31, 5) do
    {:ok, 110, 22}
  end

  defp dec_huffman_lookup(31, 6) do
    {:more, 112, 1}
  end

  defp dec_huffman_lookup(31, 7) do
    {:ok, 112, 22}
  end

  defp dec_huffman_lookup(31, 8) do
    {:more, 114, 1}
  end

  defp dec_huffman_lookup(31, 9) do
    {:ok, 114, 22}
  end

  defp dec_huffman_lookup(31, 10) do
    {:more, 117, 1}
  end

  defp dec_huffman_lookup(31, 11) do
    {:ok, 117, 22}
  end

  defp dec_huffman_lookup(31, 12) do
    {:ok, 58, 0}
  end

  defp dec_huffman_lookup(31, 13) do
    {:ok, 66, 0}
  end

  defp dec_huffman_lookup(31, 14) do
    {:ok, 67, 0}
  end

  defp dec_huffman_lookup(31, 15) do
    {:ok, 68, 0}
  end

  defp dec_huffman_lookup(32, 0) do
    {:more, 108, 2}
  end

  defp dec_huffman_lookup(32, 1) do
    {:more, 108, 9}
  end

  defp dec_huffman_lookup(32, 2) do
    {:more, 108, 23}
  end

  defp dec_huffman_lookup(32, 3) do
    {:ok, 108, 40}
  end

  defp dec_huffman_lookup(32, 4) do
    {:more, 109, 2}
  end

  defp dec_huffman_lookup(32, 5) do
    {:more, 109, 9}
  end

  defp dec_huffman_lookup(32, 6) do
    {:more, 109, 23}
  end

  defp dec_huffman_lookup(32, 7) do
    {:ok, 109, 40}
  end

  defp dec_huffman_lookup(32, 8) do
    {:more, 110, 2}
  end

  defp dec_huffman_lookup(32, 9) do
    {:more, 110, 9}
  end

  defp dec_huffman_lookup(32, 10) do
    {:more, 110, 23}
  end

  defp dec_huffman_lookup(32, 11) do
    {:ok, 110, 40}
  end

  defp dec_huffman_lookup(32, 12) do
    {:more, 112, 2}
  end

  defp dec_huffman_lookup(32, 13) do
    {:more, 112, 9}
  end

  defp dec_huffman_lookup(32, 14) do
    {:more, 112, 23}
  end

  defp dec_huffman_lookup(32, 15) do
    {:ok, 112, 40}
  end

  defp dec_huffman_lookup(33, 0) do
    {:more, 108, 3}
  end

  defp dec_huffman_lookup(33, 1) do
    {:more, 108, 6}
  end

  defp dec_huffman_lookup(33, 2) do
    {:more, 108, 10}
  end

  defp dec_huffman_lookup(33, 3) do
    {:more, 108, 15}
  end

  defp dec_huffman_lookup(33, 4) do
    {:more, 108, 24}
  end

  defp dec_huffman_lookup(33, 5) do
    {:more, 108, 31}
  end

  defp dec_huffman_lookup(33, 6) do
    {:more, 108, 41}
  end

  defp dec_huffman_lookup(33, 7) do
    {:ok, 108, 56}
  end

  defp dec_huffman_lookup(33, 8) do
    {:more, 109, 3}
  end

  defp dec_huffman_lookup(33, 9) do
    {:more, 109, 6}
  end

  defp dec_huffman_lookup(33, 10) do
    {:more, 109, 10}
  end

  defp dec_huffman_lookup(33, 11) do
    {:more, 109, 15}
  end

  defp dec_huffman_lookup(33, 12) do
    {:more, 109, 24}
  end

  defp dec_huffman_lookup(33, 13) do
    {:more, 109, 31}
  end

  defp dec_huffman_lookup(33, 14) do
    {:more, 109, 41}
  end

  defp dec_huffman_lookup(33, 15) do
    {:ok, 109, 56}
  end

  defp dec_huffman_lookup(34, 0) do
    {:more, 110, 3}
  end

  defp dec_huffman_lookup(34, 1) do
    {:more, 110, 6}
  end

  defp dec_huffman_lookup(34, 2) do
    {:more, 110, 10}
  end

  defp dec_huffman_lookup(34, 3) do
    {:more, 110, 15}
  end

  defp dec_huffman_lookup(34, 4) do
    {:more, 110, 24}
  end

  defp dec_huffman_lookup(34, 5) do
    {:more, 110, 31}
  end

  defp dec_huffman_lookup(34, 6) do
    {:more, 110, 41}
  end

  defp dec_huffman_lookup(34, 7) do
    {:ok, 110, 56}
  end

  defp dec_huffman_lookup(34, 8) do
    {:more, 112, 3}
  end

  defp dec_huffman_lookup(34, 9) do
    {:more, 112, 6}
  end

  defp dec_huffman_lookup(34, 10) do
    {:more, 112, 10}
  end

  defp dec_huffman_lookup(34, 11) do
    {:more, 112, 15}
  end

  defp dec_huffman_lookup(34, 12) do
    {:more, 112, 24}
  end

  defp dec_huffman_lookup(34, 13) do
    {:more, 112, 31}
  end

  defp dec_huffman_lookup(34, 14) do
    {:more, 112, 41}
  end

  defp dec_huffman_lookup(34, 15) do
    {:ok, 112, 56}
  end

  defp dec_huffman_lookup(35, 0) do
    {:more, 114, 2}
  end

  defp dec_huffman_lookup(35, 1) do
    {:more, 114, 9}
  end

  defp dec_huffman_lookup(35, 2) do
    {:more, 114, 23}
  end

  defp dec_huffman_lookup(35, 3) do
    {:ok, 114, 40}
  end

  defp dec_huffman_lookup(35, 4) do
    {:more, 117, 2}
  end

  defp dec_huffman_lookup(35, 5) do
    {:more, 117, 9}
  end

  defp dec_huffman_lookup(35, 6) do
    {:more, 117, 23}
  end

  defp dec_huffman_lookup(35, 7) do
    {:ok, 117, 40}
  end

  defp dec_huffman_lookup(35, 8) do
    {:more, 58, 1}
  end

  defp dec_huffman_lookup(35, 9) do
    {:ok, 58, 22}
  end

  defp dec_huffman_lookup(35, 10) do
    {:more, 66, 1}
  end

  defp dec_huffman_lookup(35, 11) do
    {:ok, 66, 22}
  end

  defp dec_huffman_lookup(35, 12) do
    {:more, 67, 1}
  end

  defp dec_huffman_lookup(35, 13) do
    {:ok, 67, 22}
  end

  defp dec_huffman_lookup(35, 14) do
    {:more, 68, 1}
  end

  defp dec_huffman_lookup(35, 15) do
    {:ok, 68, 22}
  end

  defp dec_huffman_lookup(36, 0) do
    {:more, 114, 3}
  end

  defp dec_huffman_lookup(36, 1) do
    {:more, 114, 6}
  end

  defp dec_huffman_lookup(36, 2) do
    {:more, 114, 10}
  end

  defp dec_huffman_lookup(36, 3) do
    {:more, 114, 15}
  end

  defp dec_huffman_lookup(36, 4) do
    {:more, 114, 24}
  end

  defp dec_huffman_lookup(36, 5) do
    {:more, 114, 31}
  end

  defp dec_huffman_lookup(36, 6) do
    {:more, 114, 41}
  end

  defp dec_huffman_lookup(36, 7) do
    {:ok, 114, 56}
  end

  defp dec_huffman_lookup(36, 8) do
    {:more, 117, 3}
  end

  defp dec_huffman_lookup(36, 9) do
    {:more, 117, 6}
  end

  defp dec_huffman_lookup(36, 10) do
    {:more, 117, 10}
  end

  defp dec_huffman_lookup(36, 11) do
    {:more, 117, 15}
  end

  defp dec_huffman_lookup(36, 12) do
    {:more, 117, 24}
  end

  defp dec_huffman_lookup(36, 13) do
    {:more, 117, 31}
  end

  defp dec_huffman_lookup(36, 14) do
    {:more, 117, 41}
  end

  defp dec_huffman_lookup(36, 15) do
    {:ok, 117, 56}
  end

  defp dec_huffman_lookup(37, 0) do
    {:more, 58, 2}
  end

  defp dec_huffman_lookup(37, 1) do
    {:more, 58, 9}
  end

  defp dec_huffman_lookup(37, 2) do
    {:more, 58, 23}
  end

  defp dec_huffman_lookup(37, 3) do
    {:ok, 58, 40}
  end

  defp dec_huffman_lookup(37, 4) do
    {:more, 66, 2}
  end

  defp dec_huffman_lookup(37, 5) do
    {:more, 66, 9}
  end

  defp dec_huffman_lookup(37, 6) do
    {:more, 66, 23}
  end

  defp dec_huffman_lookup(37, 7) do
    {:ok, 66, 40}
  end

  defp dec_huffman_lookup(37, 8) do
    {:more, 67, 2}
  end

  defp dec_huffman_lookup(37, 9) do
    {:more, 67, 9}
  end

  defp dec_huffman_lookup(37, 10) do
    {:more, 67, 23}
  end

  defp dec_huffman_lookup(37, 11) do
    {:ok, 67, 40}
  end

  defp dec_huffman_lookup(37, 12) do
    {:more, 68, 2}
  end

  defp dec_huffman_lookup(37, 13) do
    {:more, 68, 9}
  end

  defp dec_huffman_lookup(37, 14) do
    {:more, 68, 23}
  end

  defp dec_huffman_lookup(37, 15) do
    {:ok, 68, 40}
  end

  defp dec_huffman_lookup(38, 0) do
    {:more, 58, 3}
  end

  defp dec_huffman_lookup(38, 1) do
    {:more, 58, 6}
  end

  defp dec_huffman_lookup(38, 2) do
    {:more, 58, 10}
  end

  defp dec_huffman_lookup(38, 3) do
    {:more, 58, 15}
  end

  defp dec_huffman_lookup(38, 4) do
    {:more, 58, 24}
  end

  defp dec_huffman_lookup(38, 5) do
    {:more, 58, 31}
  end

  defp dec_huffman_lookup(38, 6) do
    {:more, 58, 41}
  end

  defp dec_huffman_lookup(38, 7) do
    {:ok, 58, 56}
  end

  defp dec_huffman_lookup(38, 8) do
    {:more, 66, 3}
  end

  defp dec_huffman_lookup(38, 9) do
    {:more, 66, 6}
  end

  defp dec_huffman_lookup(38, 10) do
    {:more, 66, 10}
  end

  defp dec_huffman_lookup(38, 11) do
    {:more, 66, 15}
  end

  defp dec_huffman_lookup(38, 12) do
    {:more, 66, 24}
  end

  defp dec_huffman_lookup(38, 13) do
    {:more, 66, 31}
  end

  defp dec_huffman_lookup(38, 14) do
    {:more, 66, 41}
  end

  defp dec_huffman_lookup(38, 15) do
    {:ok, 66, 56}
  end

  defp dec_huffman_lookup(39, 0) do
    {:more, 67, 3}
  end

  defp dec_huffman_lookup(39, 1) do
    {:more, 67, 6}
  end

  defp dec_huffman_lookup(39, 2) do
    {:more, 67, 10}
  end

  defp dec_huffman_lookup(39, 3) do
    {:more, 67, 15}
  end

  defp dec_huffman_lookup(39, 4) do
    {:more, 67, 24}
  end

  defp dec_huffman_lookup(39, 5) do
    {:more, 67, 31}
  end

  defp dec_huffman_lookup(39, 6) do
    {:more, 67, 41}
  end

  defp dec_huffman_lookup(39, 7) do
    {:ok, 67, 56}
  end

  defp dec_huffman_lookup(39, 8) do
    {:more, 68, 3}
  end

  defp dec_huffman_lookup(39, 9) do
    {:more, 68, 6}
  end

  defp dec_huffman_lookup(39, 10) do
    {:more, 68, 10}
  end

  defp dec_huffman_lookup(39, 11) do
    {:more, 68, 15}
  end

  defp dec_huffman_lookup(39, 12) do
    {:more, 68, 24}
  end

  defp dec_huffman_lookup(39, 13) do
    {:more, 68, 31}
  end

  defp dec_huffman_lookup(39, 14) do
    {:more, 68, 41}
  end

  defp dec_huffman_lookup(39, 15) do
    {:ok, 68, 56}
  end

  defp dec_huffman_lookup(40, 0) do
    {:more, :undefined, 44}
  end

  defp dec_huffman_lookup(40, 1) do
    {:more, :undefined, 45}
  end

  defp dec_huffman_lookup(40, 2) do
    {:more, :undefined, 47}
  end

  defp dec_huffman_lookup(40, 3) do
    {:more, :undefined, 48}
  end

  defp dec_huffman_lookup(40, 4) do
    {:more, :undefined, 51}
  end

  defp dec_huffman_lookup(40, 5) do
    {:more, :undefined, 52}
  end

  defp dec_huffman_lookup(40, 6) do
    {:more, :undefined, 54}
  end

  defp dec_huffman_lookup(40, 7) do
    {:more, :undefined, 55}
  end

  defp dec_huffman_lookup(40, 8) do
    {:more, :undefined, 59}
  end

  defp dec_huffman_lookup(40, 9) do
    {:more, :undefined, 60}
  end

  defp dec_huffman_lookup(40, 10) do
    {:more, :undefined, 62}
  end

  defp dec_huffman_lookup(40, 11) do
    {:more, :undefined, 63}
  end

  defp dec_huffman_lookup(40, 12) do
    {:more, :undefined, 66}
  end

  defp dec_huffman_lookup(40, 13) do
    {:more, :undefined, 67}
  end

  defp dec_huffman_lookup(40, 14) do
    {:more, :undefined, 69}
  end

  defp dec_huffman_lookup(40, 15) do
    {:ok, :undefined, 72}
  end

  defp dec_huffman_lookup(41, 0) do
    {:ok, 69, 0}
  end

  defp dec_huffman_lookup(41, 1) do
    {:ok, 70, 0}
  end

  defp dec_huffman_lookup(41, 2) do
    {:ok, 71, 0}
  end

  defp dec_huffman_lookup(41, 3) do
    {:ok, 72, 0}
  end

  defp dec_huffman_lookup(41, 4) do
    {:ok, 73, 0}
  end

  defp dec_huffman_lookup(41, 5) do
    {:ok, 74, 0}
  end

  defp dec_huffman_lookup(41, 6) do
    {:ok, 75, 0}
  end

  defp dec_huffman_lookup(41, 7) do
    {:ok, 76, 0}
  end

  defp dec_huffman_lookup(41, 8) do
    {:ok, 77, 0}
  end

  defp dec_huffman_lookup(41, 9) do
    {:ok, 78, 0}
  end

  defp dec_huffman_lookup(41, 10) do
    {:ok, 79, 0}
  end

  defp dec_huffman_lookup(41, 11) do
    {:ok, 80, 0}
  end

  defp dec_huffman_lookup(41, 12) do
    {:ok, 81, 0}
  end

  defp dec_huffman_lookup(41, 13) do
    {:ok, 82, 0}
  end

  defp dec_huffman_lookup(41, 14) do
    {:ok, 83, 0}
  end

  defp dec_huffman_lookup(41, 15) do
    {:ok, 84, 0}
  end

  defp dec_huffman_lookup(42, 0) do
    {:more, 69, 1}
  end

  defp dec_huffman_lookup(42, 1) do
    {:ok, 69, 22}
  end

  defp dec_huffman_lookup(42, 2) do
    {:more, 70, 1}
  end

  defp dec_huffman_lookup(42, 3) do
    {:ok, 70, 22}
  end

  defp dec_huffman_lookup(42, 4) do
    {:more, 71, 1}
  end

  defp dec_huffman_lookup(42, 5) do
    {:ok, 71, 22}
  end

  defp dec_huffman_lookup(42, 6) do
    {:more, 72, 1}
  end

  defp dec_huffman_lookup(42, 7) do
    {:ok, 72, 22}
  end

  defp dec_huffman_lookup(42, 8) do
    {:more, 73, 1}
  end

  defp dec_huffman_lookup(42, 9) do
    {:ok, 73, 22}
  end

  defp dec_huffman_lookup(42, 10) do
    {:more, 74, 1}
  end

  defp dec_huffman_lookup(42, 11) do
    {:ok, 74, 22}
  end

  defp dec_huffman_lookup(42, 12) do
    {:more, 75, 1}
  end

  defp dec_huffman_lookup(42, 13) do
    {:ok, 75, 22}
  end

  defp dec_huffman_lookup(42, 14) do
    {:more, 76, 1}
  end

  defp dec_huffman_lookup(42, 15) do
    {:ok, 76, 22}
  end

  defp dec_huffman_lookup(43, 0) do
    {:more, 69, 2}
  end

  defp dec_huffman_lookup(43, 1) do
    {:more, 69, 9}
  end

  defp dec_huffman_lookup(43, 2) do
    {:more, 69, 23}
  end

  defp dec_huffman_lookup(43, 3) do
    {:ok, 69, 40}
  end

  defp dec_huffman_lookup(43, 4) do
    {:more, 70, 2}
  end

  defp dec_huffman_lookup(43, 5) do
    {:more, 70, 9}
  end

  defp dec_huffman_lookup(43, 6) do
    {:more, 70, 23}
  end

  defp dec_huffman_lookup(43, 7) do
    {:ok, 70, 40}
  end

  defp dec_huffman_lookup(43, 8) do
    {:more, 71, 2}
  end

  defp dec_huffman_lookup(43, 9) do
    {:more, 71, 9}
  end

  defp dec_huffman_lookup(43, 10) do
    {:more, 71, 23}
  end

  defp dec_huffman_lookup(43, 11) do
    {:ok, 71, 40}
  end

  defp dec_huffman_lookup(43, 12) do
    {:more, 72, 2}
  end

  defp dec_huffman_lookup(43, 13) do
    {:more, 72, 9}
  end

  defp dec_huffman_lookup(43, 14) do
    {:more, 72, 23}
  end

  defp dec_huffman_lookup(43, 15) do
    {:ok, 72, 40}
  end

  defp dec_huffman_lookup(44, 0) do
    {:more, 69, 3}
  end

  defp dec_huffman_lookup(44, 1) do
    {:more, 69, 6}
  end

  defp dec_huffman_lookup(44, 2) do
    {:more, 69, 10}
  end

  defp dec_huffman_lookup(44, 3) do
    {:more, 69, 15}
  end

  defp dec_huffman_lookup(44, 4) do
    {:more, 69, 24}
  end

  defp dec_huffman_lookup(44, 5) do
    {:more, 69, 31}
  end

  defp dec_huffman_lookup(44, 6) do
    {:more, 69, 41}
  end

  defp dec_huffman_lookup(44, 7) do
    {:ok, 69, 56}
  end

  defp dec_huffman_lookup(44, 8) do
    {:more, 70, 3}
  end

  defp dec_huffman_lookup(44, 9) do
    {:more, 70, 6}
  end

  defp dec_huffman_lookup(44, 10) do
    {:more, 70, 10}
  end

  defp dec_huffman_lookup(44, 11) do
    {:more, 70, 15}
  end

  defp dec_huffman_lookup(44, 12) do
    {:more, 70, 24}
  end

  defp dec_huffman_lookup(44, 13) do
    {:more, 70, 31}
  end

  defp dec_huffman_lookup(44, 14) do
    {:more, 70, 41}
  end

  defp dec_huffman_lookup(44, 15) do
    {:ok, 70, 56}
  end

  defp dec_huffman_lookup(45, 0) do
    {:more, 71, 3}
  end

  defp dec_huffman_lookup(45, 1) do
    {:more, 71, 6}
  end

  defp dec_huffman_lookup(45, 2) do
    {:more, 71, 10}
  end

  defp dec_huffman_lookup(45, 3) do
    {:more, 71, 15}
  end

  defp dec_huffman_lookup(45, 4) do
    {:more, 71, 24}
  end

  defp dec_huffman_lookup(45, 5) do
    {:more, 71, 31}
  end

  defp dec_huffman_lookup(45, 6) do
    {:more, 71, 41}
  end

  defp dec_huffman_lookup(45, 7) do
    {:ok, 71, 56}
  end

  defp dec_huffman_lookup(45, 8) do
    {:more, 72, 3}
  end

  defp dec_huffman_lookup(45, 9) do
    {:more, 72, 6}
  end

  defp dec_huffman_lookup(45, 10) do
    {:more, 72, 10}
  end

  defp dec_huffman_lookup(45, 11) do
    {:more, 72, 15}
  end

  defp dec_huffman_lookup(45, 12) do
    {:more, 72, 24}
  end

  defp dec_huffman_lookup(45, 13) do
    {:more, 72, 31}
  end

  defp dec_huffman_lookup(45, 14) do
    {:more, 72, 41}
  end

  defp dec_huffman_lookup(45, 15) do
    {:ok, 72, 56}
  end

  defp dec_huffman_lookup(46, 0) do
    {:more, 73, 2}
  end

  defp dec_huffman_lookup(46, 1) do
    {:more, 73, 9}
  end

  defp dec_huffman_lookup(46, 2) do
    {:more, 73, 23}
  end

  defp dec_huffman_lookup(46, 3) do
    {:ok, 73, 40}
  end

  defp dec_huffman_lookup(46, 4) do
    {:more, 74, 2}
  end

  defp dec_huffman_lookup(46, 5) do
    {:more, 74, 9}
  end

  defp dec_huffman_lookup(46, 6) do
    {:more, 74, 23}
  end

  defp dec_huffman_lookup(46, 7) do
    {:ok, 74, 40}
  end

  defp dec_huffman_lookup(46, 8) do
    {:more, 75, 2}
  end

  defp dec_huffman_lookup(46, 9) do
    {:more, 75, 9}
  end

  defp dec_huffman_lookup(46, 10) do
    {:more, 75, 23}
  end

  defp dec_huffman_lookup(46, 11) do
    {:ok, 75, 40}
  end

  defp dec_huffman_lookup(46, 12) do
    {:more, 76, 2}
  end

  defp dec_huffman_lookup(46, 13) do
    {:more, 76, 9}
  end

  defp dec_huffman_lookup(46, 14) do
    {:more, 76, 23}
  end

  defp dec_huffman_lookup(46, 15) do
    {:ok, 76, 40}
  end

  defp dec_huffman_lookup(47, 0) do
    {:more, 73, 3}
  end

  defp dec_huffman_lookup(47, 1) do
    {:more, 73, 6}
  end

  defp dec_huffman_lookup(47, 2) do
    {:more, 73, 10}
  end

  defp dec_huffman_lookup(47, 3) do
    {:more, 73, 15}
  end

  defp dec_huffman_lookup(47, 4) do
    {:more, 73, 24}
  end

  defp dec_huffman_lookup(47, 5) do
    {:more, 73, 31}
  end

  defp dec_huffman_lookup(47, 6) do
    {:more, 73, 41}
  end

  defp dec_huffman_lookup(47, 7) do
    {:ok, 73, 56}
  end

  defp dec_huffman_lookup(47, 8) do
    {:more, 74, 3}
  end

  defp dec_huffman_lookup(47, 9) do
    {:more, 74, 6}
  end

  defp dec_huffman_lookup(47, 10) do
    {:more, 74, 10}
  end

  defp dec_huffman_lookup(47, 11) do
    {:more, 74, 15}
  end

  defp dec_huffman_lookup(47, 12) do
    {:more, 74, 24}
  end

  defp dec_huffman_lookup(47, 13) do
    {:more, 74, 31}
  end

  defp dec_huffman_lookup(47, 14) do
    {:more, 74, 41}
  end

  defp dec_huffman_lookup(47, 15) do
    {:ok, 74, 56}
  end

  defp dec_huffman_lookup(48, 0) do
    {:more, 75, 3}
  end

  defp dec_huffman_lookup(48, 1) do
    {:more, 75, 6}
  end

  defp dec_huffman_lookup(48, 2) do
    {:more, 75, 10}
  end

  defp dec_huffman_lookup(48, 3) do
    {:more, 75, 15}
  end

  defp dec_huffman_lookup(48, 4) do
    {:more, 75, 24}
  end

  defp dec_huffman_lookup(48, 5) do
    {:more, 75, 31}
  end

  defp dec_huffman_lookup(48, 6) do
    {:more, 75, 41}
  end

  defp dec_huffman_lookup(48, 7) do
    {:ok, 75, 56}
  end

  defp dec_huffman_lookup(48, 8) do
    {:more, 76, 3}
  end

  defp dec_huffman_lookup(48, 9) do
    {:more, 76, 6}
  end

  defp dec_huffman_lookup(48, 10) do
    {:more, 76, 10}
  end

  defp dec_huffman_lookup(48, 11) do
    {:more, 76, 15}
  end

  defp dec_huffman_lookup(48, 12) do
    {:more, 76, 24}
  end

  defp dec_huffman_lookup(48, 13) do
    {:more, 76, 31}
  end

  defp dec_huffman_lookup(48, 14) do
    {:more, 76, 41}
  end

  defp dec_huffman_lookup(48, 15) do
    {:ok, 76, 56}
  end

  defp dec_huffman_lookup(49, 0) do
    {:more, 77, 1}
  end

  defp dec_huffman_lookup(49, 1) do
    {:ok, 77, 22}
  end

  defp dec_huffman_lookup(49, 2) do
    {:more, 78, 1}
  end

  defp dec_huffman_lookup(49, 3) do
    {:ok, 78, 22}
  end

  defp dec_huffman_lookup(49, 4) do
    {:more, 79, 1}
  end

  defp dec_huffman_lookup(49, 5) do
    {:ok, 79, 22}
  end

  defp dec_huffman_lookup(49, 6) do
    {:more, 80, 1}
  end

  defp dec_huffman_lookup(49, 7) do
    {:ok, 80, 22}
  end

  defp dec_huffman_lookup(49, 8) do
    {:more, 81, 1}
  end

  defp dec_huffman_lookup(49, 9) do
    {:ok, 81, 22}
  end

  defp dec_huffman_lookup(49, 10) do
    {:more, 82, 1}
  end

  defp dec_huffman_lookup(49, 11) do
    {:ok, 82, 22}
  end

  defp dec_huffman_lookup(49, 12) do
    {:more, 83, 1}
  end

  defp dec_huffman_lookup(49, 13) do
    {:ok, 83, 22}
  end

  defp dec_huffman_lookup(49, 14) do
    {:more, 84, 1}
  end

  defp dec_huffman_lookup(49, 15) do
    {:ok, 84, 22}
  end

  defp dec_huffman_lookup(50, 0) do
    {:more, 77, 2}
  end

  defp dec_huffman_lookup(50, 1) do
    {:more, 77, 9}
  end

  defp dec_huffman_lookup(50, 2) do
    {:more, 77, 23}
  end

  defp dec_huffman_lookup(50, 3) do
    {:ok, 77, 40}
  end

  defp dec_huffman_lookup(50, 4) do
    {:more, 78, 2}
  end

  defp dec_huffman_lookup(50, 5) do
    {:more, 78, 9}
  end

  defp dec_huffman_lookup(50, 6) do
    {:more, 78, 23}
  end

  defp dec_huffman_lookup(50, 7) do
    {:ok, 78, 40}
  end

  defp dec_huffman_lookup(50, 8) do
    {:more, 79, 2}
  end

  defp dec_huffman_lookup(50, 9) do
    {:more, 79, 9}
  end

  defp dec_huffman_lookup(50, 10) do
    {:more, 79, 23}
  end

  defp dec_huffman_lookup(50, 11) do
    {:ok, 79, 40}
  end

  defp dec_huffman_lookup(50, 12) do
    {:more, 80, 2}
  end

  defp dec_huffman_lookup(50, 13) do
    {:more, 80, 9}
  end

  defp dec_huffman_lookup(50, 14) do
    {:more, 80, 23}
  end

  defp dec_huffman_lookup(50, 15) do
    {:ok, 80, 40}
  end

  defp dec_huffman_lookup(51, 0) do
    {:more, 77, 3}
  end

  defp dec_huffman_lookup(51, 1) do
    {:more, 77, 6}
  end

  defp dec_huffman_lookup(51, 2) do
    {:more, 77, 10}
  end

  defp dec_huffman_lookup(51, 3) do
    {:more, 77, 15}
  end

  defp dec_huffman_lookup(51, 4) do
    {:more, 77, 24}
  end

  defp dec_huffman_lookup(51, 5) do
    {:more, 77, 31}
  end

  defp dec_huffman_lookup(51, 6) do
    {:more, 77, 41}
  end

  defp dec_huffman_lookup(51, 7) do
    {:ok, 77, 56}
  end

  defp dec_huffman_lookup(51, 8) do
    {:more, 78, 3}
  end

  defp dec_huffman_lookup(51, 9) do
    {:more, 78, 6}
  end

  defp dec_huffman_lookup(51, 10) do
    {:more, 78, 10}
  end

  defp dec_huffman_lookup(51, 11) do
    {:more, 78, 15}
  end

  defp dec_huffman_lookup(51, 12) do
    {:more, 78, 24}
  end

  defp dec_huffman_lookup(51, 13) do
    {:more, 78, 31}
  end

  defp dec_huffman_lookup(51, 14) do
    {:more, 78, 41}
  end

  defp dec_huffman_lookup(51, 15) do
    {:ok, 78, 56}
  end

  defp dec_huffman_lookup(52, 0) do
    {:more, 79, 3}
  end

  defp dec_huffman_lookup(52, 1) do
    {:more, 79, 6}
  end

  defp dec_huffman_lookup(52, 2) do
    {:more, 79, 10}
  end

  defp dec_huffman_lookup(52, 3) do
    {:more, 79, 15}
  end

  defp dec_huffman_lookup(52, 4) do
    {:more, 79, 24}
  end

  defp dec_huffman_lookup(52, 5) do
    {:more, 79, 31}
  end

  defp dec_huffman_lookup(52, 6) do
    {:more, 79, 41}
  end

  defp dec_huffman_lookup(52, 7) do
    {:ok, 79, 56}
  end

  defp dec_huffman_lookup(52, 8) do
    {:more, 80, 3}
  end

  defp dec_huffman_lookup(52, 9) do
    {:more, 80, 6}
  end

  defp dec_huffman_lookup(52, 10) do
    {:more, 80, 10}
  end

  defp dec_huffman_lookup(52, 11) do
    {:more, 80, 15}
  end

  defp dec_huffman_lookup(52, 12) do
    {:more, 80, 24}
  end

  defp dec_huffman_lookup(52, 13) do
    {:more, 80, 31}
  end

  defp dec_huffman_lookup(52, 14) do
    {:more, 80, 41}
  end

  defp dec_huffman_lookup(52, 15) do
    {:ok, 80, 56}
  end

  defp dec_huffman_lookup(53, 0) do
    {:more, 81, 2}
  end

  defp dec_huffman_lookup(53, 1) do
    {:more, 81, 9}
  end

  defp dec_huffman_lookup(53, 2) do
    {:more, 81, 23}
  end

  defp dec_huffman_lookup(53, 3) do
    {:ok, 81, 40}
  end

  defp dec_huffman_lookup(53, 4) do
    {:more, 82, 2}
  end

  defp dec_huffman_lookup(53, 5) do
    {:more, 82, 9}
  end

  defp dec_huffman_lookup(53, 6) do
    {:more, 82, 23}
  end

  defp dec_huffman_lookup(53, 7) do
    {:ok, 82, 40}
  end

  defp dec_huffman_lookup(53, 8) do
    {:more, 83, 2}
  end

  defp dec_huffman_lookup(53, 9) do
    {:more, 83, 9}
  end

  defp dec_huffman_lookup(53, 10) do
    {:more, 83, 23}
  end

  defp dec_huffman_lookup(53, 11) do
    {:ok, 83, 40}
  end

  defp dec_huffman_lookup(53, 12) do
    {:more, 84, 2}
  end

  defp dec_huffman_lookup(53, 13) do
    {:more, 84, 9}
  end

  defp dec_huffman_lookup(53, 14) do
    {:more, 84, 23}
  end

  defp dec_huffman_lookup(53, 15) do
    {:ok, 84, 40}
  end

  defp dec_huffman_lookup(54, 0) do
    {:more, 81, 3}
  end

  defp dec_huffman_lookup(54, 1) do
    {:more, 81, 6}
  end

  defp dec_huffman_lookup(54, 2) do
    {:more, 81, 10}
  end

  defp dec_huffman_lookup(54, 3) do
    {:more, 81, 15}
  end

  defp dec_huffman_lookup(54, 4) do
    {:more, 81, 24}
  end

  defp dec_huffman_lookup(54, 5) do
    {:more, 81, 31}
  end

  defp dec_huffman_lookup(54, 6) do
    {:more, 81, 41}
  end

  defp dec_huffman_lookup(54, 7) do
    {:ok, 81, 56}
  end

  defp dec_huffman_lookup(54, 8) do
    {:more, 82, 3}
  end

  defp dec_huffman_lookup(54, 9) do
    {:more, 82, 6}
  end

  defp dec_huffman_lookup(54, 10) do
    {:more, 82, 10}
  end

  defp dec_huffman_lookup(54, 11) do
    {:more, 82, 15}
  end

  defp dec_huffman_lookup(54, 12) do
    {:more, 82, 24}
  end

  defp dec_huffman_lookup(54, 13) do
    {:more, 82, 31}
  end

  defp dec_huffman_lookup(54, 14) do
    {:more, 82, 41}
  end

  defp dec_huffman_lookup(54, 15) do
    {:ok, 82, 56}
  end

  defp dec_huffman_lookup(55, 0) do
    {:more, 83, 3}
  end

  defp dec_huffman_lookup(55, 1) do
    {:more, 83, 6}
  end

  defp dec_huffman_lookup(55, 2) do
    {:more, 83, 10}
  end

  defp dec_huffman_lookup(55, 3) do
    {:more, 83, 15}
  end

  defp dec_huffman_lookup(55, 4) do
    {:more, 83, 24}
  end

  defp dec_huffman_lookup(55, 5) do
    {:more, 83, 31}
  end

  defp dec_huffman_lookup(55, 6) do
    {:more, 83, 41}
  end

  defp dec_huffman_lookup(55, 7) do
    {:ok, 83, 56}
  end

  defp dec_huffman_lookup(55, 8) do
    {:more, 84, 3}
  end

  defp dec_huffman_lookup(55, 9) do
    {:more, 84, 6}
  end

  defp dec_huffman_lookup(55, 10) do
    {:more, 84, 10}
  end

  defp dec_huffman_lookup(55, 11) do
    {:more, 84, 15}
  end

  defp dec_huffman_lookup(55, 12) do
    {:more, 84, 24}
  end

  defp dec_huffman_lookup(55, 13) do
    {:more, 84, 31}
  end

  defp dec_huffman_lookup(55, 14) do
    {:more, 84, 41}
  end

  defp dec_huffman_lookup(55, 15) do
    {:ok, 84, 56}
  end

  defp dec_huffman_lookup(56, 0) do
    {:ok, 85, 0}
  end

  defp dec_huffman_lookup(56, 1) do
    {:ok, 86, 0}
  end

  defp dec_huffman_lookup(56, 2) do
    {:ok, 87, 0}
  end

  defp dec_huffman_lookup(56, 3) do
    {:ok, 89, 0}
  end

  defp dec_huffman_lookup(56, 4) do
    {:ok, 106, 0}
  end

  defp dec_huffman_lookup(56, 5) do
    {:ok, 107, 0}
  end

  defp dec_huffman_lookup(56, 6) do
    {:ok, 113, 0}
  end

  defp dec_huffman_lookup(56, 7) do
    {:ok, 118, 0}
  end

  defp dec_huffman_lookup(56, 8) do
    {:ok, 119, 0}
  end

  defp dec_huffman_lookup(56, 9) do
    {:ok, 120, 0}
  end

  defp dec_huffman_lookup(56, 10) do
    {:ok, 121, 0}
  end

  defp dec_huffman_lookup(56, 11) do
    {:ok, 122, 0}
  end

  defp dec_huffman_lookup(56, 12) do
    {:more, :undefined, 70}
  end

  defp dec_huffman_lookup(56, 13) do
    {:more, :undefined, 71}
  end

  defp dec_huffman_lookup(56, 14) do
    {:more, :undefined, 73}
  end

  defp dec_huffman_lookup(56, 15) do
    {:ok, :undefined, 74}
  end

  defp dec_huffman_lookup(57, 0) do
    {:more, 85, 1}
  end

  defp dec_huffman_lookup(57, 1) do
    {:ok, 85, 22}
  end

  defp dec_huffman_lookup(57, 2) do
    {:more, 86, 1}
  end

  defp dec_huffman_lookup(57, 3) do
    {:ok, 86, 22}
  end

  defp dec_huffman_lookup(57, 4) do
    {:more, 87, 1}
  end

  defp dec_huffman_lookup(57, 5) do
    {:ok, 87, 22}
  end

  defp dec_huffman_lookup(57, 6) do
    {:more, 89, 1}
  end

  defp dec_huffman_lookup(57, 7) do
    {:ok, 89, 22}
  end

  defp dec_huffman_lookup(57, 8) do
    {:more, 106, 1}
  end

  defp dec_huffman_lookup(57, 9) do
    {:ok, 106, 22}
  end

  defp dec_huffman_lookup(57, 10) do
    {:more, 107, 1}
  end

  defp dec_huffman_lookup(57, 11) do
    {:ok, 107, 22}
  end

  defp dec_huffman_lookup(57, 12) do
    {:more, 113, 1}
  end

  defp dec_huffman_lookup(57, 13) do
    {:ok, 113, 22}
  end

  defp dec_huffman_lookup(57, 14) do
    {:more, 118, 1}
  end

  defp dec_huffman_lookup(57, 15) do
    {:ok, 118, 22}
  end

  defp dec_huffman_lookup(58, 0) do
    {:more, 85, 2}
  end

  defp dec_huffman_lookup(58, 1) do
    {:more, 85, 9}
  end

  defp dec_huffman_lookup(58, 2) do
    {:more, 85, 23}
  end

  defp dec_huffman_lookup(58, 3) do
    {:ok, 85, 40}
  end

  defp dec_huffman_lookup(58, 4) do
    {:more, 86, 2}
  end

  defp dec_huffman_lookup(58, 5) do
    {:more, 86, 9}
  end

  defp dec_huffman_lookup(58, 6) do
    {:more, 86, 23}
  end

  defp dec_huffman_lookup(58, 7) do
    {:ok, 86, 40}
  end

  defp dec_huffman_lookup(58, 8) do
    {:more, 87, 2}
  end

  defp dec_huffman_lookup(58, 9) do
    {:more, 87, 9}
  end

  defp dec_huffman_lookup(58, 10) do
    {:more, 87, 23}
  end

  defp dec_huffman_lookup(58, 11) do
    {:ok, 87, 40}
  end

  defp dec_huffman_lookup(58, 12) do
    {:more, 89, 2}
  end

  defp dec_huffman_lookup(58, 13) do
    {:more, 89, 9}
  end

  defp dec_huffman_lookup(58, 14) do
    {:more, 89, 23}
  end

  defp dec_huffman_lookup(58, 15) do
    {:ok, 89, 40}
  end

  defp dec_huffman_lookup(59, 0) do
    {:more, 85, 3}
  end

  defp dec_huffman_lookup(59, 1) do
    {:more, 85, 6}
  end

  defp dec_huffman_lookup(59, 2) do
    {:more, 85, 10}
  end

  defp dec_huffman_lookup(59, 3) do
    {:more, 85, 15}
  end

  defp dec_huffman_lookup(59, 4) do
    {:more, 85, 24}
  end

  defp dec_huffman_lookup(59, 5) do
    {:more, 85, 31}
  end

  defp dec_huffman_lookup(59, 6) do
    {:more, 85, 41}
  end

  defp dec_huffman_lookup(59, 7) do
    {:ok, 85, 56}
  end

  defp dec_huffman_lookup(59, 8) do
    {:more, 86, 3}
  end

  defp dec_huffman_lookup(59, 9) do
    {:more, 86, 6}
  end

  defp dec_huffman_lookup(59, 10) do
    {:more, 86, 10}
  end

  defp dec_huffman_lookup(59, 11) do
    {:more, 86, 15}
  end

  defp dec_huffman_lookup(59, 12) do
    {:more, 86, 24}
  end

  defp dec_huffman_lookup(59, 13) do
    {:more, 86, 31}
  end

  defp dec_huffman_lookup(59, 14) do
    {:more, 86, 41}
  end

  defp dec_huffman_lookup(59, 15) do
    {:ok, 86, 56}
  end

  defp dec_huffman_lookup(60, 0) do
    {:more, 87, 3}
  end

  defp dec_huffman_lookup(60, 1) do
    {:more, 87, 6}
  end

  defp dec_huffman_lookup(60, 2) do
    {:more, 87, 10}
  end

  defp dec_huffman_lookup(60, 3) do
    {:more, 87, 15}
  end

  defp dec_huffman_lookup(60, 4) do
    {:more, 87, 24}
  end

  defp dec_huffman_lookup(60, 5) do
    {:more, 87, 31}
  end

  defp dec_huffman_lookup(60, 6) do
    {:more, 87, 41}
  end

  defp dec_huffman_lookup(60, 7) do
    {:ok, 87, 56}
  end

  defp dec_huffman_lookup(60, 8) do
    {:more, 89, 3}
  end

  defp dec_huffman_lookup(60, 9) do
    {:more, 89, 6}
  end

  defp dec_huffman_lookup(60, 10) do
    {:more, 89, 10}
  end

  defp dec_huffman_lookup(60, 11) do
    {:more, 89, 15}
  end

  defp dec_huffman_lookup(60, 12) do
    {:more, 89, 24}
  end

  defp dec_huffman_lookup(60, 13) do
    {:more, 89, 31}
  end

  defp dec_huffman_lookup(60, 14) do
    {:more, 89, 41}
  end

  defp dec_huffman_lookup(60, 15) do
    {:ok, 89, 56}
  end

  defp dec_huffman_lookup(61, 0) do
    {:more, 106, 2}
  end

  defp dec_huffman_lookup(61, 1) do
    {:more, 106, 9}
  end

  defp dec_huffman_lookup(61, 2) do
    {:more, 106, 23}
  end

  defp dec_huffman_lookup(61, 3) do
    {:ok, 106, 40}
  end

  defp dec_huffman_lookup(61, 4) do
    {:more, 107, 2}
  end

  defp dec_huffman_lookup(61, 5) do
    {:more, 107, 9}
  end

  defp dec_huffman_lookup(61, 6) do
    {:more, 107, 23}
  end

  defp dec_huffman_lookup(61, 7) do
    {:ok, 107, 40}
  end

  defp dec_huffman_lookup(61, 8) do
    {:more, 113, 2}
  end

  defp dec_huffman_lookup(61, 9) do
    {:more, 113, 9}
  end

  defp dec_huffman_lookup(61, 10) do
    {:more, 113, 23}
  end

  defp dec_huffman_lookup(61, 11) do
    {:ok, 113, 40}
  end

  defp dec_huffman_lookup(61, 12) do
    {:more, 118, 2}
  end

  defp dec_huffman_lookup(61, 13) do
    {:more, 118, 9}
  end

  defp dec_huffman_lookup(61, 14) do
    {:more, 118, 23}
  end

  defp dec_huffman_lookup(61, 15) do
    {:ok, 118, 40}
  end

  defp dec_huffman_lookup(62, 0) do
    {:more, 106, 3}
  end

  defp dec_huffman_lookup(62, 1) do
    {:more, 106, 6}
  end

  defp dec_huffman_lookup(62, 2) do
    {:more, 106, 10}
  end

  defp dec_huffman_lookup(62, 3) do
    {:more, 106, 15}
  end

  defp dec_huffman_lookup(62, 4) do
    {:more, 106, 24}
  end

  defp dec_huffman_lookup(62, 5) do
    {:more, 106, 31}
  end

  defp dec_huffman_lookup(62, 6) do
    {:more, 106, 41}
  end

  defp dec_huffman_lookup(62, 7) do
    {:ok, 106, 56}
  end

  defp dec_huffman_lookup(62, 8) do
    {:more, 107, 3}
  end

  defp dec_huffman_lookup(62, 9) do
    {:more, 107, 6}
  end

  defp dec_huffman_lookup(62, 10) do
    {:more, 107, 10}
  end

  defp dec_huffman_lookup(62, 11) do
    {:more, 107, 15}
  end

  defp dec_huffman_lookup(62, 12) do
    {:more, 107, 24}
  end

  defp dec_huffman_lookup(62, 13) do
    {:more, 107, 31}
  end

  defp dec_huffman_lookup(62, 14) do
    {:more, 107, 41}
  end

  defp dec_huffman_lookup(62, 15) do
    {:ok, 107, 56}
  end

  defp dec_huffman_lookup(63, 0) do
    {:more, 113, 3}
  end

  defp dec_huffman_lookup(63, 1) do
    {:more, 113, 6}
  end

  defp dec_huffman_lookup(63, 2) do
    {:more, 113, 10}
  end

  defp dec_huffman_lookup(63, 3) do
    {:more, 113, 15}
  end

  defp dec_huffman_lookup(63, 4) do
    {:more, 113, 24}
  end

  defp dec_huffman_lookup(63, 5) do
    {:more, 113, 31}
  end

  defp dec_huffman_lookup(63, 6) do
    {:more, 113, 41}
  end

  defp dec_huffman_lookup(63, 7) do
    {:ok, 113, 56}
  end

  defp dec_huffman_lookup(63, 8) do
    {:more, 118, 3}
  end

  defp dec_huffman_lookup(63, 9) do
    {:more, 118, 6}
  end

  defp dec_huffman_lookup(63, 10) do
    {:more, 118, 10}
  end

  defp dec_huffman_lookup(63, 11) do
    {:more, 118, 15}
  end

  defp dec_huffman_lookup(63, 12) do
    {:more, 118, 24}
  end

  defp dec_huffman_lookup(63, 13) do
    {:more, 118, 31}
  end

  defp dec_huffman_lookup(63, 14) do
    {:more, 118, 41}
  end

  defp dec_huffman_lookup(63, 15) do
    {:ok, 118, 56}
  end

  defp dec_huffman_lookup(64, 0) do
    {:more, 119, 1}
  end

  defp dec_huffman_lookup(64, 1) do
    {:ok, 119, 22}
  end

  defp dec_huffman_lookup(64, 2) do
    {:more, 120, 1}
  end

  defp dec_huffman_lookup(64, 3) do
    {:ok, 120, 22}
  end

  defp dec_huffman_lookup(64, 4) do
    {:more, 121, 1}
  end

  defp dec_huffman_lookup(64, 5) do
    {:ok, 121, 22}
  end

  defp dec_huffman_lookup(64, 6) do
    {:more, 122, 1}
  end

  defp dec_huffman_lookup(64, 7) do
    {:ok, 122, 22}
  end

  defp dec_huffman_lookup(64, 8) do
    {:ok, 38, 0}
  end

  defp dec_huffman_lookup(64, 9) do
    {:ok, 42, 0}
  end

  defp dec_huffman_lookup(64, 10) do
    {:ok, 44, 0}
  end

  defp dec_huffman_lookup(64, 11) do
    {:ok, 59, 0}
  end

  defp dec_huffman_lookup(64, 12) do
    {:ok, 88, 0}
  end

  defp dec_huffman_lookup(64, 13) do
    {:ok, 90, 0}
  end

  defp dec_huffman_lookup(64, 14) do
    {:more, :undefined, 75}
  end

  defp dec_huffman_lookup(64, 15) do
    {:ok, :undefined, 78}
  end

  defp dec_huffman_lookup(65, 0) do
    {:more, 119, 2}
  end

  defp dec_huffman_lookup(65, 1) do
    {:more, 119, 9}
  end

  defp dec_huffman_lookup(65, 2) do
    {:more, 119, 23}
  end

  defp dec_huffman_lookup(65, 3) do
    {:ok, 119, 40}
  end

  defp dec_huffman_lookup(65, 4) do
    {:more, 120, 2}
  end

  defp dec_huffman_lookup(65, 5) do
    {:more, 120, 9}
  end

  defp dec_huffman_lookup(65, 6) do
    {:more, 120, 23}
  end

  defp dec_huffman_lookup(65, 7) do
    {:ok, 120, 40}
  end

  defp dec_huffman_lookup(65, 8) do
    {:more, 121, 2}
  end

  defp dec_huffman_lookup(65, 9) do
    {:more, 121, 9}
  end

  defp dec_huffman_lookup(65, 10) do
    {:more, 121, 23}
  end

  defp dec_huffman_lookup(65, 11) do
    {:ok, 121, 40}
  end

  defp dec_huffman_lookup(65, 12) do
    {:more, 122, 2}
  end

  defp dec_huffman_lookup(65, 13) do
    {:more, 122, 9}
  end

  defp dec_huffman_lookup(65, 14) do
    {:more, 122, 23}
  end

  defp dec_huffman_lookup(65, 15) do
    {:ok, 122, 40}
  end

  defp dec_huffman_lookup(66, 0) do
    {:more, 119, 3}
  end

  defp dec_huffman_lookup(66, 1) do
    {:more, 119, 6}
  end

  defp dec_huffman_lookup(66, 2) do
    {:more, 119, 10}
  end

  defp dec_huffman_lookup(66, 3) do
    {:more, 119, 15}
  end

  defp dec_huffman_lookup(66, 4) do
    {:more, 119, 24}
  end

  defp dec_huffman_lookup(66, 5) do
    {:more, 119, 31}
  end

  defp dec_huffman_lookup(66, 6) do
    {:more, 119, 41}
  end

  defp dec_huffman_lookup(66, 7) do
    {:ok, 119, 56}
  end

  defp dec_huffman_lookup(66, 8) do
    {:more, 120, 3}
  end

  defp dec_huffman_lookup(66, 9) do
    {:more, 120, 6}
  end

  defp dec_huffman_lookup(66, 10) do
    {:more, 120, 10}
  end

  defp dec_huffman_lookup(66, 11) do
    {:more, 120, 15}
  end

  defp dec_huffman_lookup(66, 12) do
    {:more, 120, 24}
  end

  defp dec_huffman_lookup(66, 13) do
    {:more, 120, 31}
  end

  defp dec_huffman_lookup(66, 14) do
    {:more, 120, 41}
  end

  defp dec_huffman_lookup(66, 15) do
    {:ok, 120, 56}
  end

  defp dec_huffman_lookup(67, 0) do
    {:more, 121, 3}
  end

  defp dec_huffman_lookup(67, 1) do
    {:more, 121, 6}
  end

  defp dec_huffman_lookup(67, 2) do
    {:more, 121, 10}
  end

  defp dec_huffman_lookup(67, 3) do
    {:more, 121, 15}
  end

  defp dec_huffman_lookup(67, 4) do
    {:more, 121, 24}
  end

  defp dec_huffman_lookup(67, 5) do
    {:more, 121, 31}
  end

  defp dec_huffman_lookup(67, 6) do
    {:more, 121, 41}
  end

  defp dec_huffman_lookup(67, 7) do
    {:ok, 121, 56}
  end

  defp dec_huffman_lookup(67, 8) do
    {:more, 122, 3}
  end

  defp dec_huffman_lookup(67, 9) do
    {:more, 122, 6}
  end

  defp dec_huffman_lookup(67, 10) do
    {:more, 122, 10}
  end

  defp dec_huffman_lookup(67, 11) do
    {:more, 122, 15}
  end

  defp dec_huffman_lookup(67, 12) do
    {:more, 122, 24}
  end

  defp dec_huffman_lookup(67, 13) do
    {:more, 122, 31}
  end

  defp dec_huffman_lookup(67, 14) do
    {:more, 122, 41}
  end

  defp dec_huffman_lookup(67, 15) do
    {:ok, 122, 56}
  end

  defp dec_huffman_lookup(68, 0) do
    {:more, 38, 1}
  end

  defp dec_huffman_lookup(68, 1) do
    {:ok, 38, 22}
  end

  defp dec_huffman_lookup(68, 2) do
    {:more, 42, 1}
  end

  defp dec_huffman_lookup(68, 3) do
    {:ok, 42, 22}
  end

  defp dec_huffman_lookup(68, 4) do
    {:more, 44, 1}
  end

  defp dec_huffman_lookup(68, 5) do
    {:ok, 44, 22}
  end

  defp dec_huffman_lookup(68, 6) do
    {:more, 59, 1}
  end

  defp dec_huffman_lookup(68, 7) do
    {:ok, 59, 22}
  end

  defp dec_huffman_lookup(68, 8) do
    {:more, 88, 1}
  end

  defp dec_huffman_lookup(68, 9) do
    {:ok, 88, 22}
  end

  defp dec_huffman_lookup(68, 10) do
    {:more, 90, 1}
  end

  defp dec_huffman_lookup(68, 11) do
    {:ok, 90, 22}
  end

  defp dec_huffman_lookup(68, 12) do
    {:more, :undefined, 76}
  end

  defp dec_huffman_lookup(68, 13) do
    {:more, :undefined, 77}
  end

  defp dec_huffman_lookup(68, 14) do
    {:more, :undefined, 79}
  end

  defp dec_huffman_lookup(68, 15) do
    {:ok, :undefined, 81}
  end

  defp dec_huffman_lookup(69, 0) do
    {:more, 38, 2}
  end

  defp dec_huffman_lookup(69, 1) do
    {:more, 38, 9}
  end

  defp dec_huffman_lookup(69, 2) do
    {:more, 38, 23}
  end

  defp dec_huffman_lookup(69, 3) do
    {:ok, 38, 40}
  end

  defp dec_huffman_lookup(69, 4) do
    {:more, 42, 2}
  end

  defp dec_huffman_lookup(69, 5) do
    {:more, 42, 9}
  end

  defp dec_huffman_lookup(69, 6) do
    {:more, 42, 23}
  end

  defp dec_huffman_lookup(69, 7) do
    {:ok, 42, 40}
  end

  defp dec_huffman_lookup(69, 8) do
    {:more, 44, 2}
  end

  defp dec_huffman_lookup(69, 9) do
    {:more, 44, 9}
  end

  defp dec_huffman_lookup(69, 10) do
    {:more, 44, 23}
  end

  defp dec_huffman_lookup(69, 11) do
    {:ok, 44, 40}
  end

  defp dec_huffman_lookup(69, 12) do
    {:more, 59, 2}
  end

  defp dec_huffman_lookup(69, 13) do
    {:more, 59, 9}
  end

  defp dec_huffman_lookup(69, 14) do
    {:more, 59, 23}
  end

  defp dec_huffman_lookup(69, 15) do
    {:ok, 59, 40}
  end

  defp dec_huffman_lookup(70, 0) do
    {:more, 38, 3}
  end

  defp dec_huffman_lookup(70, 1) do
    {:more, 38, 6}
  end

  defp dec_huffman_lookup(70, 2) do
    {:more, 38, 10}
  end

  defp dec_huffman_lookup(70, 3) do
    {:more, 38, 15}
  end

  defp dec_huffman_lookup(70, 4) do
    {:more, 38, 24}
  end

  defp dec_huffman_lookup(70, 5) do
    {:more, 38, 31}
  end

  defp dec_huffman_lookup(70, 6) do
    {:more, 38, 41}
  end

  defp dec_huffman_lookup(70, 7) do
    {:ok, 38, 56}
  end

  defp dec_huffman_lookup(70, 8) do
    {:more, 42, 3}
  end

  defp dec_huffman_lookup(70, 9) do
    {:more, 42, 6}
  end

  defp dec_huffman_lookup(70, 10) do
    {:more, 42, 10}
  end

  defp dec_huffman_lookup(70, 11) do
    {:more, 42, 15}
  end

  defp dec_huffman_lookup(70, 12) do
    {:more, 42, 24}
  end

  defp dec_huffman_lookup(70, 13) do
    {:more, 42, 31}
  end

  defp dec_huffman_lookup(70, 14) do
    {:more, 42, 41}
  end

  defp dec_huffman_lookup(70, 15) do
    {:ok, 42, 56}
  end

  defp dec_huffman_lookup(71, 0) do
    {:more, 44, 3}
  end

  defp dec_huffman_lookup(71, 1) do
    {:more, 44, 6}
  end

  defp dec_huffman_lookup(71, 2) do
    {:more, 44, 10}
  end

  defp dec_huffman_lookup(71, 3) do
    {:more, 44, 15}
  end

  defp dec_huffman_lookup(71, 4) do
    {:more, 44, 24}
  end

  defp dec_huffman_lookup(71, 5) do
    {:more, 44, 31}
  end

  defp dec_huffman_lookup(71, 6) do
    {:more, 44, 41}
  end

  defp dec_huffman_lookup(71, 7) do
    {:ok, 44, 56}
  end

  defp dec_huffman_lookup(71, 8) do
    {:more, 59, 3}
  end

  defp dec_huffman_lookup(71, 9) do
    {:more, 59, 6}
  end

  defp dec_huffman_lookup(71, 10) do
    {:more, 59, 10}
  end

  defp dec_huffman_lookup(71, 11) do
    {:more, 59, 15}
  end

  defp dec_huffman_lookup(71, 12) do
    {:more, 59, 24}
  end

  defp dec_huffman_lookup(71, 13) do
    {:more, 59, 31}
  end

  defp dec_huffman_lookup(71, 14) do
    {:more, 59, 41}
  end

  defp dec_huffman_lookup(71, 15) do
    {:ok, 59, 56}
  end

  defp dec_huffman_lookup(72, 0) do
    {:more, 88, 2}
  end

  defp dec_huffman_lookup(72, 1) do
    {:more, 88, 9}
  end

  defp dec_huffman_lookup(72, 2) do
    {:more, 88, 23}
  end

  defp dec_huffman_lookup(72, 3) do
    {:ok, 88, 40}
  end

  defp dec_huffman_lookup(72, 4) do
    {:more, 90, 2}
  end

  defp dec_huffman_lookup(72, 5) do
    {:more, 90, 9}
  end

  defp dec_huffman_lookup(72, 6) do
    {:more, 90, 23}
  end

  defp dec_huffman_lookup(72, 7) do
    {:ok, 90, 40}
  end

  defp dec_huffman_lookup(72, 8) do
    {:ok, 33, 0}
  end

  defp dec_huffman_lookup(72, 9) do
    {:ok, 34, 0}
  end

  defp dec_huffman_lookup(72, 10) do
    {:ok, 40, 0}
  end

  defp dec_huffman_lookup(72, 11) do
    {:ok, 41, 0}
  end

  defp dec_huffman_lookup(72, 12) do
    {:ok, 63, 0}
  end

  defp dec_huffman_lookup(72, 13) do
    {:more, :undefined, 80}
  end

  defp dec_huffman_lookup(72, 14) do
    {:more, :undefined, 82}
  end

  defp dec_huffman_lookup(72, 15) do
    {:ok, :undefined, 84}
  end

  defp dec_huffman_lookup(73, 0) do
    {:more, 88, 3}
  end

  defp dec_huffman_lookup(73, 1) do
    {:more, 88, 6}
  end

  defp dec_huffman_lookup(73, 2) do
    {:more, 88, 10}
  end

  defp dec_huffman_lookup(73, 3) do
    {:more, 88, 15}
  end

  defp dec_huffman_lookup(73, 4) do
    {:more, 88, 24}
  end

  defp dec_huffman_lookup(73, 5) do
    {:more, 88, 31}
  end

  defp dec_huffman_lookup(73, 6) do
    {:more, 88, 41}
  end

  defp dec_huffman_lookup(73, 7) do
    {:ok, 88, 56}
  end

  defp dec_huffman_lookup(73, 8) do
    {:more, 90, 3}
  end

  defp dec_huffman_lookup(73, 9) do
    {:more, 90, 6}
  end

  defp dec_huffman_lookup(73, 10) do
    {:more, 90, 10}
  end

  defp dec_huffman_lookup(73, 11) do
    {:more, 90, 15}
  end

  defp dec_huffman_lookup(73, 12) do
    {:more, 90, 24}
  end

  defp dec_huffman_lookup(73, 13) do
    {:more, 90, 31}
  end

  defp dec_huffman_lookup(73, 14) do
    {:more, 90, 41}
  end

  defp dec_huffman_lookup(73, 15) do
    {:ok, 90, 56}
  end

  defp dec_huffman_lookup(74, 0) do
    {:more, 33, 1}
  end

  defp dec_huffman_lookup(74, 1) do
    {:ok, 33, 22}
  end

  defp dec_huffman_lookup(74, 2) do
    {:more, 34, 1}
  end

  defp dec_huffman_lookup(74, 3) do
    {:ok, 34, 22}
  end

  defp dec_huffman_lookup(74, 4) do
    {:more, 40, 1}
  end

  defp dec_huffman_lookup(74, 5) do
    {:ok, 40, 22}
  end

  defp dec_huffman_lookup(74, 6) do
    {:more, 41, 1}
  end

  defp dec_huffman_lookup(74, 7) do
    {:ok, 41, 22}
  end

  defp dec_huffman_lookup(74, 8) do
    {:more, 63, 1}
  end

  defp dec_huffman_lookup(74, 9) do
    {:ok, 63, 22}
  end

  defp dec_huffman_lookup(74, 10) do
    {:ok, 39, 0}
  end

  defp dec_huffman_lookup(74, 11) do
    {:ok, 43, 0}
  end

  defp dec_huffman_lookup(74, 12) do
    {:ok, 124, 0}
  end

  defp dec_huffman_lookup(74, 13) do
    {:more, :undefined, 83}
  end

  defp dec_huffman_lookup(74, 14) do
    {:more, :undefined, 85}
  end

  defp dec_huffman_lookup(74, 15) do
    {:ok, :undefined, 88}
  end

  defp dec_huffman_lookup(75, 0) do
    {:more, 33, 2}
  end

  defp dec_huffman_lookup(75, 1) do
    {:more, 33, 9}
  end

  defp dec_huffman_lookup(75, 2) do
    {:more, 33, 23}
  end

  defp dec_huffman_lookup(75, 3) do
    {:ok, 33, 40}
  end

  defp dec_huffman_lookup(75, 4) do
    {:more, 34, 2}
  end

  defp dec_huffman_lookup(75, 5) do
    {:more, 34, 9}
  end

  defp dec_huffman_lookup(75, 6) do
    {:more, 34, 23}
  end

  defp dec_huffman_lookup(75, 7) do
    {:ok, 34, 40}
  end

  defp dec_huffman_lookup(75, 8) do
    {:more, 40, 2}
  end

  defp dec_huffman_lookup(75, 9) do
    {:more, 40, 9}
  end

  defp dec_huffman_lookup(75, 10) do
    {:more, 40, 23}
  end

  defp dec_huffman_lookup(75, 11) do
    {:ok, 40, 40}
  end

  defp dec_huffman_lookup(75, 12) do
    {:more, 41, 2}
  end

  defp dec_huffman_lookup(75, 13) do
    {:more, 41, 9}
  end

  defp dec_huffman_lookup(75, 14) do
    {:more, 41, 23}
  end

  defp dec_huffman_lookup(75, 15) do
    {:ok, 41, 40}
  end

  defp dec_huffman_lookup(76, 0) do
    {:more, 33, 3}
  end

  defp dec_huffman_lookup(76, 1) do
    {:more, 33, 6}
  end

  defp dec_huffman_lookup(76, 2) do
    {:more, 33, 10}
  end

  defp dec_huffman_lookup(76, 3) do
    {:more, 33, 15}
  end

  defp dec_huffman_lookup(76, 4) do
    {:more, 33, 24}
  end

  defp dec_huffman_lookup(76, 5) do
    {:more, 33, 31}
  end

  defp dec_huffman_lookup(76, 6) do
    {:more, 33, 41}
  end

  defp dec_huffman_lookup(76, 7) do
    {:ok, 33, 56}
  end

  defp dec_huffman_lookup(76, 8) do
    {:more, 34, 3}
  end

  defp dec_huffman_lookup(76, 9) do
    {:more, 34, 6}
  end

  defp dec_huffman_lookup(76, 10) do
    {:more, 34, 10}
  end

  defp dec_huffman_lookup(76, 11) do
    {:more, 34, 15}
  end

  defp dec_huffman_lookup(76, 12) do
    {:more, 34, 24}
  end

  defp dec_huffman_lookup(76, 13) do
    {:more, 34, 31}
  end

  defp dec_huffman_lookup(76, 14) do
    {:more, 34, 41}
  end

  defp dec_huffman_lookup(76, 15) do
    {:ok, 34, 56}
  end

  defp dec_huffman_lookup(77, 0) do
    {:more, 40, 3}
  end

  defp dec_huffman_lookup(77, 1) do
    {:more, 40, 6}
  end

  defp dec_huffman_lookup(77, 2) do
    {:more, 40, 10}
  end

  defp dec_huffman_lookup(77, 3) do
    {:more, 40, 15}
  end

  defp dec_huffman_lookup(77, 4) do
    {:more, 40, 24}
  end

  defp dec_huffman_lookup(77, 5) do
    {:more, 40, 31}
  end

  defp dec_huffman_lookup(77, 6) do
    {:more, 40, 41}
  end

  defp dec_huffman_lookup(77, 7) do
    {:ok, 40, 56}
  end

  defp dec_huffman_lookup(77, 8) do
    {:more, 41, 3}
  end

  defp dec_huffman_lookup(77, 9) do
    {:more, 41, 6}
  end

  defp dec_huffman_lookup(77, 10) do
    {:more, 41, 10}
  end

  defp dec_huffman_lookup(77, 11) do
    {:more, 41, 15}
  end

  defp dec_huffman_lookup(77, 12) do
    {:more, 41, 24}
  end

  defp dec_huffman_lookup(77, 13) do
    {:more, 41, 31}
  end

  defp dec_huffman_lookup(77, 14) do
    {:more, 41, 41}
  end

  defp dec_huffman_lookup(77, 15) do
    {:ok, 41, 56}
  end

  defp dec_huffman_lookup(78, 0) do
    {:more, 63, 2}
  end

  defp dec_huffman_lookup(78, 1) do
    {:more, 63, 9}
  end

  defp dec_huffman_lookup(78, 2) do
    {:more, 63, 23}
  end

  defp dec_huffman_lookup(78, 3) do
    {:ok, 63, 40}
  end

  defp dec_huffman_lookup(78, 4) do
    {:more, 39, 1}
  end

  defp dec_huffman_lookup(78, 5) do
    {:ok, 39, 22}
  end

  defp dec_huffman_lookup(78, 6) do
    {:more, 43, 1}
  end

  defp dec_huffman_lookup(78, 7) do
    {:ok, 43, 22}
  end

  defp dec_huffman_lookup(78, 8) do
    {:more, 124, 1}
  end

  defp dec_huffman_lookup(78, 9) do
    {:ok, 124, 22}
  end

  defp dec_huffman_lookup(78, 10) do
    {:ok, 35, 0}
  end

  defp dec_huffman_lookup(78, 11) do
    {:ok, 62, 0}
  end

  defp dec_huffman_lookup(78, 12) do
    {:more, :undefined, 86}
  end

  defp dec_huffman_lookup(78, 13) do
    {:more, :undefined, 87}
  end

  defp dec_huffman_lookup(78, 14) do
    {:more, :undefined, 89}
  end

  defp dec_huffman_lookup(78, 15) do
    {:ok, :undefined, 90}
  end

  defp dec_huffman_lookup(79, 0) do
    {:more, 63, 3}
  end

  defp dec_huffman_lookup(79, 1) do
    {:more, 63, 6}
  end

  defp dec_huffman_lookup(79, 2) do
    {:more, 63, 10}
  end

  defp dec_huffman_lookup(79, 3) do
    {:more, 63, 15}
  end

  defp dec_huffman_lookup(79, 4) do
    {:more, 63, 24}
  end

  defp dec_huffman_lookup(79, 5) do
    {:more, 63, 31}
  end

  defp dec_huffman_lookup(79, 6) do
    {:more, 63, 41}
  end

  defp dec_huffman_lookup(79, 7) do
    {:ok, 63, 56}
  end

  defp dec_huffman_lookup(79, 8) do
    {:more, 39, 2}
  end

  defp dec_huffman_lookup(79, 9) do
    {:more, 39, 9}
  end

  defp dec_huffman_lookup(79, 10) do
    {:more, 39, 23}
  end

  defp dec_huffman_lookup(79, 11) do
    {:ok, 39, 40}
  end

  defp dec_huffman_lookup(79, 12) do
    {:more, 43, 2}
  end

  defp dec_huffman_lookup(79, 13) do
    {:more, 43, 9}
  end

  defp dec_huffman_lookup(79, 14) do
    {:more, 43, 23}
  end

  defp dec_huffman_lookup(79, 15) do
    {:ok, 43, 40}
  end

  defp dec_huffman_lookup(80, 0) do
    {:more, 39, 3}
  end

  defp dec_huffman_lookup(80, 1) do
    {:more, 39, 6}
  end

  defp dec_huffman_lookup(80, 2) do
    {:more, 39, 10}
  end

  defp dec_huffman_lookup(80, 3) do
    {:more, 39, 15}
  end

  defp dec_huffman_lookup(80, 4) do
    {:more, 39, 24}
  end

  defp dec_huffman_lookup(80, 5) do
    {:more, 39, 31}
  end

  defp dec_huffman_lookup(80, 6) do
    {:more, 39, 41}
  end

  defp dec_huffman_lookup(80, 7) do
    {:ok, 39, 56}
  end

  defp dec_huffman_lookup(80, 8) do
    {:more, 43, 3}
  end

  defp dec_huffman_lookup(80, 9) do
    {:more, 43, 6}
  end

  defp dec_huffman_lookup(80, 10) do
    {:more, 43, 10}
  end

  defp dec_huffman_lookup(80, 11) do
    {:more, 43, 15}
  end

  defp dec_huffman_lookup(80, 12) do
    {:more, 43, 24}
  end

  defp dec_huffman_lookup(80, 13) do
    {:more, 43, 31}
  end

  defp dec_huffman_lookup(80, 14) do
    {:more, 43, 41}
  end

  defp dec_huffman_lookup(80, 15) do
    {:ok, 43, 56}
  end

  defp dec_huffman_lookup(81, 0) do
    {:more, 124, 2}
  end

  defp dec_huffman_lookup(81, 1) do
    {:more, 124, 9}
  end

  defp dec_huffman_lookup(81, 2) do
    {:more, 124, 23}
  end

  defp dec_huffman_lookup(81, 3) do
    {:ok, 124, 40}
  end

  defp dec_huffman_lookup(81, 4) do
    {:more, 35, 1}
  end

  defp dec_huffman_lookup(81, 5) do
    {:ok, 35, 22}
  end

  defp dec_huffman_lookup(81, 6) do
    {:more, 62, 1}
  end

  defp dec_huffman_lookup(81, 7) do
    {:ok, 62, 22}
  end

  defp dec_huffman_lookup(81, 8) do
    {:ok, 0, 0}
  end

  defp dec_huffman_lookup(81, 9) do
    {:ok, 36, 0}
  end

  defp dec_huffman_lookup(81, 10) do
    {:ok, 64, 0}
  end

  defp dec_huffman_lookup(81, 11) do
    {:ok, 91, 0}
  end

  defp dec_huffman_lookup(81, 12) do
    {:ok, 93, 0}
  end

  defp dec_huffman_lookup(81, 13) do
    {:ok, 126, 0}
  end

  defp dec_huffman_lookup(81, 14) do
    {:more, :undefined, 91}
  end

  defp dec_huffman_lookup(81, 15) do
    {:ok, :undefined, 92}
  end

  defp dec_huffman_lookup(82, 0) do
    {:more, 124, 3}
  end

  defp dec_huffman_lookup(82, 1) do
    {:more, 124, 6}
  end

  defp dec_huffman_lookup(82, 2) do
    {:more, 124, 10}
  end

  defp dec_huffman_lookup(82, 3) do
    {:more, 124, 15}
  end

  defp dec_huffman_lookup(82, 4) do
    {:more, 124, 24}
  end

  defp dec_huffman_lookup(82, 5) do
    {:more, 124, 31}
  end

  defp dec_huffman_lookup(82, 6) do
    {:more, 124, 41}
  end

  defp dec_huffman_lookup(82, 7) do
    {:ok, 124, 56}
  end

  defp dec_huffman_lookup(82, 8) do
    {:more, 35, 2}
  end

  defp dec_huffman_lookup(82, 9) do
    {:more, 35, 9}
  end

  defp dec_huffman_lookup(82, 10) do
    {:more, 35, 23}
  end

  defp dec_huffman_lookup(82, 11) do
    {:ok, 35, 40}
  end

  defp dec_huffman_lookup(82, 12) do
    {:more, 62, 2}
  end

  defp dec_huffman_lookup(82, 13) do
    {:more, 62, 9}
  end

  defp dec_huffman_lookup(82, 14) do
    {:more, 62, 23}
  end

  defp dec_huffman_lookup(82, 15) do
    {:ok, 62, 40}
  end

  defp dec_huffman_lookup(83, 0) do
    {:more, 35, 3}
  end

  defp dec_huffman_lookup(83, 1) do
    {:more, 35, 6}
  end

  defp dec_huffman_lookup(83, 2) do
    {:more, 35, 10}
  end

  defp dec_huffman_lookup(83, 3) do
    {:more, 35, 15}
  end

  defp dec_huffman_lookup(83, 4) do
    {:more, 35, 24}
  end

  defp dec_huffman_lookup(83, 5) do
    {:more, 35, 31}
  end

  defp dec_huffman_lookup(83, 6) do
    {:more, 35, 41}
  end

  defp dec_huffman_lookup(83, 7) do
    {:ok, 35, 56}
  end

  defp dec_huffman_lookup(83, 8) do
    {:more, 62, 3}
  end

  defp dec_huffman_lookup(83, 9) do
    {:more, 62, 6}
  end

  defp dec_huffman_lookup(83, 10) do
    {:more, 62, 10}
  end

  defp dec_huffman_lookup(83, 11) do
    {:more, 62, 15}
  end

  defp dec_huffman_lookup(83, 12) do
    {:more, 62, 24}
  end

  defp dec_huffman_lookup(83, 13) do
    {:more, 62, 31}
  end

  defp dec_huffman_lookup(83, 14) do
    {:more, 62, 41}
  end

  defp dec_huffman_lookup(83, 15) do
    {:ok, 62, 56}
  end

  defp dec_huffman_lookup(84, 0) do
    {:more, 0, 1}
  end

  defp dec_huffman_lookup(84, 1) do
    {:ok, 0, 22}
  end

  defp dec_huffman_lookup(84, 2) do
    {:more, 36, 1}
  end

  defp dec_huffman_lookup(84, 3) do
    {:ok, 36, 22}
  end

  defp dec_huffman_lookup(84, 4) do
    {:more, 64, 1}
  end

  defp dec_huffman_lookup(84, 5) do
    {:ok, 64, 22}
  end

  defp dec_huffman_lookup(84, 6) do
    {:more, 91, 1}
  end

  defp dec_huffman_lookup(84, 7) do
    {:ok, 91, 22}
  end

  defp dec_huffman_lookup(84, 8) do
    {:more, 93, 1}
  end

  defp dec_huffman_lookup(84, 9) do
    {:ok, 93, 22}
  end

  defp dec_huffman_lookup(84, 10) do
    {:more, 126, 1}
  end

  defp dec_huffman_lookup(84, 11) do
    {:ok, 126, 22}
  end

  defp dec_huffman_lookup(84, 12) do
    {:ok, 94, 0}
  end

  defp dec_huffman_lookup(84, 13) do
    {:ok, 125, 0}
  end

  defp dec_huffman_lookup(84, 14) do
    {:more, :undefined, 93}
  end

  defp dec_huffman_lookup(84, 15) do
    {:ok, :undefined, 94}
  end

  defp dec_huffman_lookup(85, 0) do
    {:more, 0, 2}
  end

  defp dec_huffman_lookup(85, 1) do
    {:more, 0, 9}
  end

  defp dec_huffman_lookup(85, 2) do
    {:more, 0, 23}
  end

  defp dec_huffman_lookup(85, 3) do
    {:ok, 0, 40}
  end

  defp dec_huffman_lookup(85, 4) do
    {:more, 36, 2}
  end

  defp dec_huffman_lookup(85, 5) do
    {:more, 36, 9}
  end

  defp dec_huffman_lookup(85, 6) do
    {:more, 36, 23}
  end

  defp dec_huffman_lookup(85, 7) do
    {:ok, 36, 40}
  end

  defp dec_huffman_lookup(85, 8) do
    {:more, 64, 2}
  end

  defp dec_huffman_lookup(85, 9) do
    {:more, 64, 9}
  end

  defp dec_huffman_lookup(85, 10) do
    {:more, 64, 23}
  end

  defp dec_huffman_lookup(85, 11) do
    {:ok, 64, 40}
  end

  defp dec_huffman_lookup(85, 12) do
    {:more, 91, 2}
  end

  defp dec_huffman_lookup(85, 13) do
    {:more, 91, 9}
  end

  defp dec_huffman_lookup(85, 14) do
    {:more, 91, 23}
  end

  defp dec_huffman_lookup(85, 15) do
    {:ok, 91, 40}
  end

  defp dec_huffman_lookup(86, 0) do
    {:more, 0, 3}
  end

  defp dec_huffman_lookup(86, 1) do
    {:more, 0, 6}
  end

  defp dec_huffman_lookup(86, 2) do
    {:more, 0, 10}
  end

  defp dec_huffman_lookup(86, 3) do
    {:more, 0, 15}
  end

  defp dec_huffman_lookup(86, 4) do
    {:more, 0, 24}
  end

  defp dec_huffman_lookup(86, 5) do
    {:more, 0, 31}
  end

  defp dec_huffman_lookup(86, 6) do
    {:more, 0, 41}
  end

  defp dec_huffman_lookup(86, 7) do
    {:ok, 0, 56}
  end

  defp dec_huffman_lookup(86, 8) do
    {:more, 36, 3}
  end

  defp dec_huffman_lookup(86, 9) do
    {:more, 36, 6}
  end

  defp dec_huffman_lookup(86, 10) do
    {:more, 36, 10}
  end

  defp dec_huffman_lookup(86, 11) do
    {:more, 36, 15}
  end

  defp dec_huffman_lookup(86, 12) do
    {:more, 36, 24}
  end

  defp dec_huffman_lookup(86, 13) do
    {:more, 36, 31}
  end

  defp dec_huffman_lookup(86, 14) do
    {:more, 36, 41}
  end

  defp dec_huffman_lookup(86, 15) do
    {:ok, 36, 56}
  end

  defp dec_huffman_lookup(87, 0) do
    {:more, 64, 3}
  end

  defp dec_huffman_lookup(87, 1) do
    {:more, 64, 6}
  end

  defp dec_huffman_lookup(87, 2) do
    {:more, 64, 10}
  end

  defp dec_huffman_lookup(87, 3) do
    {:more, 64, 15}
  end

  defp dec_huffman_lookup(87, 4) do
    {:more, 64, 24}
  end

  defp dec_huffman_lookup(87, 5) do
    {:more, 64, 31}
  end

  defp dec_huffman_lookup(87, 6) do
    {:more, 64, 41}
  end

  defp dec_huffman_lookup(87, 7) do
    {:ok, 64, 56}
  end

  defp dec_huffman_lookup(87, 8) do
    {:more, 91, 3}
  end

  defp dec_huffman_lookup(87, 9) do
    {:more, 91, 6}
  end

  defp dec_huffman_lookup(87, 10) do
    {:more, 91, 10}
  end

  defp dec_huffman_lookup(87, 11) do
    {:more, 91, 15}
  end

  defp dec_huffman_lookup(87, 12) do
    {:more, 91, 24}
  end

  defp dec_huffman_lookup(87, 13) do
    {:more, 91, 31}
  end

  defp dec_huffman_lookup(87, 14) do
    {:more, 91, 41}
  end

  defp dec_huffman_lookup(87, 15) do
    {:ok, 91, 56}
  end

  defp dec_huffman_lookup(88, 0) do
    {:more, 93, 2}
  end

  defp dec_huffman_lookup(88, 1) do
    {:more, 93, 9}
  end

  defp dec_huffman_lookup(88, 2) do
    {:more, 93, 23}
  end

  defp dec_huffman_lookup(88, 3) do
    {:ok, 93, 40}
  end

  defp dec_huffman_lookup(88, 4) do
    {:more, 126, 2}
  end

  defp dec_huffman_lookup(88, 5) do
    {:more, 126, 9}
  end

  defp dec_huffman_lookup(88, 6) do
    {:more, 126, 23}
  end

  defp dec_huffman_lookup(88, 7) do
    {:ok, 126, 40}
  end

  defp dec_huffman_lookup(88, 8) do
    {:more, 94, 1}
  end

  defp dec_huffman_lookup(88, 9) do
    {:ok, 94, 22}
  end

  defp dec_huffman_lookup(88, 10) do
    {:more, 125, 1}
  end

  defp dec_huffman_lookup(88, 11) do
    {:ok, 125, 22}
  end

  defp dec_huffman_lookup(88, 12) do
    {:ok, 60, 0}
  end

  defp dec_huffman_lookup(88, 13) do
    {:ok, 96, 0}
  end

  defp dec_huffman_lookup(88, 14) do
    {:ok, 123, 0}
  end

  defp dec_huffman_lookup(88, 15) do
    {:ok, :undefined, 95}
  end

  defp dec_huffman_lookup(89, 0) do
    {:more, 93, 3}
  end

  defp dec_huffman_lookup(89, 1) do
    {:more, 93, 6}
  end

  defp dec_huffman_lookup(89, 2) do
    {:more, 93, 10}
  end

  defp dec_huffman_lookup(89, 3) do
    {:more, 93, 15}
  end

  defp dec_huffman_lookup(89, 4) do
    {:more, 93, 24}
  end

  defp dec_huffman_lookup(89, 5) do
    {:more, 93, 31}
  end

  defp dec_huffman_lookup(89, 6) do
    {:more, 93, 41}
  end

  defp dec_huffman_lookup(89, 7) do
    {:ok, 93, 56}
  end

  defp dec_huffman_lookup(89, 8) do
    {:more, 126, 3}
  end

  defp dec_huffman_lookup(89, 9) do
    {:more, 126, 6}
  end

  defp dec_huffman_lookup(89, 10) do
    {:more, 126, 10}
  end

  defp dec_huffman_lookup(89, 11) do
    {:more, 126, 15}
  end

  defp dec_huffman_lookup(89, 12) do
    {:more, 126, 24}
  end

  defp dec_huffman_lookup(89, 13) do
    {:more, 126, 31}
  end

  defp dec_huffman_lookup(89, 14) do
    {:more, 126, 41}
  end

  defp dec_huffman_lookup(89, 15) do
    {:ok, 126, 56}
  end

  defp dec_huffman_lookup(90, 0) do
    {:more, 94, 2}
  end

  defp dec_huffman_lookup(90, 1) do
    {:more, 94, 9}
  end

  defp dec_huffman_lookup(90, 2) do
    {:more, 94, 23}
  end

  defp dec_huffman_lookup(90, 3) do
    {:ok, 94, 40}
  end

  defp dec_huffman_lookup(90, 4) do
    {:more, 125, 2}
  end

  defp dec_huffman_lookup(90, 5) do
    {:more, 125, 9}
  end

  defp dec_huffman_lookup(90, 6) do
    {:more, 125, 23}
  end

  defp dec_huffman_lookup(90, 7) do
    {:ok, 125, 40}
  end

  defp dec_huffman_lookup(90, 8) do
    {:more, 60, 1}
  end

  defp dec_huffman_lookup(90, 9) do
    {:ok, 60, 22}
  end

  defp dec_huffman_lookup(90, 10) do
    {:more, 96, 1}
  end

  defp dec_huffman_lookup(90, 11) do
    {:ok, 96, 22}
  end

  defp dec_huffman_lookup(90, 12) do
    {:more, 123, 1}
  end

  defp dec_huffman_lookup(90, 13) do
    {:ok, 123, 22}
  end

  defp dec_huffman_lookup(90, 14) do
    {:more, :undefined, 96}
  end

  defp dec_huffman_lookup(90, 15) do
    {:ok, :undefined, 110}
  end

  defp dec_huffman_lookup(91, 0) do
    {:more, 94, 3}
  end

  defp dec_huffman_lookup(91, 1) do
    {:more, 94, 6}
  end

  defp dec_huffman_lookup(91, 2) do
    {:more, 94, 10}
  end

  defp dec_huffman_lookup(91, 3) do
    {:more, 94, 15}
  end

  defp dec_huffman_lookup(91, 4) do
    {:more, 94, 24}
  end

  defp dec_huffman_lookup(91, 5) do
    {:more, 94, 31}
  end

  defp dec_huffman_lookup(91, 6) do
    {:more, 94, 41}
  end

  defp dec_huffman_lookup(91, 7) do
    {:ok, 94, 56}
  end

  defp dec_huffman_lookup(91, 8) do
    {:more, 125, 3}
  end

  defp dec_huffman_lookup(91, 9) do
    {:more, 125, 6}
  end

  defp dec_huffman_lookup(91, 10) do
    {:more, 125, 10}
  end

  defp dec_huffman_lookup(91, 11) do
    {:more, 125, 15}
  end

  defp dec_huffman_lookup(91, 12) do
    {:more, 125, 24}
  end

  defp dec_huffman_lookup(91, 13) do
    {:more, 125, 31}
  end

  defp dec_huffman_lookup(91, 14) do
    {:more, 125, 41}
  end

  defp dec_huffman_lookup(91, 15) do
    {:ok, 125, 56}
  end

  defp dec_huffman_lookup(92, 0) do
    {:more, 60, 2}
  end

  defp dec_huffman_lookup(92, 1) do
    {:more, 60, 9}
  end

  defp dec_huffman_lookup(92, 2) do
    {:more, 60, 23}
  end

  defp dec_huffman_lookup(92, 3) do
    {:ok, 60, 40}
  end

  defp dec_huffman_lookup(92, 4) do
    {:more, 96, 2}
  end

  defp dec_huffman_lookup(92, 5) do
    {:more, 96, 9}
  end

  defp dec_huffman_lookup(92, 6) do
    {:more, 96, 23}
  end

  defp dec_huffman_lookup(92, 7) do
    {:ok, 96, 40}
  end

  defp dec_huffman_lookup(92, 8) do
    {:more, 123, 2}
  end

  defp dec_huffman_lookup(92, 9) do
    {:more, 123, 9}
  end

  defp dec_huffman_lookup(92, 10) do
    {:more, 123, 23}
  end

  defp dec_huffman_lookup(92, 11) do
    {:ok, 123, 40}
  end

  defp dec_huffman_lookup(92, 12) do
    {:more, :undefined, 97}
  end

  defp dec_huffman_lookup(92, 13) do
    {:more, :undefined, 101}
  end

  defp dec_huffman_lookup(92, 14) do
    {:more, :undefined, 111}
  end

  defp dec_huffman_lookup(92, 15) do
    {:ok, :undefined, 133}
  end

  defp dec_huffman_lookup(93, 0) do
    {:more, 60, 3}
  end

  defp dec_huffman_lookup(93, 1) do
    {:more, 60, 6}
  end

  defp dec_huffman_lookup(93, 2) do
    {:more, 60, 10}
  end

  defp dec_huffman_lookup(93, 3) do
    {:more, 60, 15}
  end

  defp dec_huffman_lookup(93, 4) do
    {:more, 60, 24}
  end

  defp dec_huffman_lookup(93, 5) do
    {:more, 60, 31}
  end

  defp dec_huffman_lookup(93, 6) do
    {:more, 60, 41}
  end

  defp dec_huffman_lookup(93, 7) do
    {:ok, 60, 56}
  end

  defp dec_huffman_lookup(93, 8) do
    {:more, 96, 3}
  end

  defp dec_huffman_lookup(93, 9) do
    {:more, 96, 6}
  end

  defp dec_huffman_lookup(93, 10) do
    {:more, 96, 10}
  end

  defp dec_huffman_lookup(93, 11) do
    {:more, 96, 15}
  end

  defp dec_huffman_lookup(93, 12) do
    {:more, 96, 24}
  end

  defp dec_huffman_lookup(93, 13) do
    {:more, 96, 31}
  end

  defp dec_huffman_lookup(93, 14) do
    {:more, 96, 41}
  end

  defp dec_huffman_lookup(93, 15) do
    {:ok, 96, 56}
  end

  defp dec_huffman_lookup(94, 0) do
    {:more, 123, 3}
  end

  defp dec_huffman_lookup(94, 1) do
    {:more, 123, 6}
  end

  defp dec_huffman_lookup(94, 2) do
    {:more, 123, 10}
  end

  defp dec_huffman_lookup(94, 3) do
    {:more, 123, 15}
  end

  defp dec_huffman_lookup(94, 4) do
    {:more, 123, 24}
  end

  defp dec_huffman_lookup(94, 5) do
    {:more, 123, 31}
  end

  defp dec_huffman_lookup(94, 6) do
    {:more, 123, 41}
  end

  defp dec_huffman_lookup(94, 7) do
    {:ok, 123, 56}
  end

  defp dec_huffman_lookup(94, 8) do
    {:more, :undefined, 98}
  end

  defp dec_huffman_lookup(94, 9) do
    {:more, :undefined, 99}
  end

  defp dec_huffman_lookup(94, 10) do
    {:more, :undefined, 102}
  end

  defp dec_huffman_lookup(94, 11) do
    {:more, :undefined, 105}
  end

  defp dec_huffman_lookup(94, 12) do
    {:more, :undefined, 112}
  end

  defp dec_huffman_lookup(94, 13) do
    {:more, :undefined, 119}
  end

  defp dec_huffman_lookup(94, 14) do
    {:more, :undefined, 134}
  end

  defp dec_huffman_lookup(94, 15) do
    {:ok, :undefined, 153}
  end

  defp dec_huffman_lookup(95, 0) do
    {:ok, 92, 0}
  end

  defp dec_huffman_lookup(95, 1) do
    {:ok, 195, 0}
  end

  defp dec_huffman_lookup(95, 2) do
    {:ok, 208, 0}
  end

  defp dec_huffman_lookup(95, 3) do
    {:more, :undefined, 100}
  end

  defp dec_huffman_lookup(95, 4) do
    {:more, :undefined, 103}
  end

  defp dec_huffman_lookup(95, 5) do
    {:more, :undefined, 104}
  end

  defp dec_huffman_lookup(95, 6) do
    {:more, :undefined, 106}
  end

  defp dec_huffman_lookup(95, 7) do
    {:more, :undefined, 107}
  end

  defp dec_huffman_lookup(95, 8) do
    {:more, :undefined, 113}
  end

  defp dec_huffman_lookup(95, 9) do
    {:more, :undefined, 116}
  end

  defp dec_huffman_lookup(95, 10) do
    {:more, :undefined, 120}
  end

  defp dec_huffman_lookup(95, 11) do
    {:more, :undefined, 126}
  end

  defp dec_huffman_lookup(95, 12) do
    {:more, :undefined, 135}
  end

  defp dec_huffman_lookup(95, 13) do
    {:more, :undefined, 142}
  end

  defp dec_huffman_lookup(95, 14) do
    {:more, :undefined, 154}
  end

  defp dec_huffman_lookup(95, 15) do
    {:ok, :undefined, 169}
  end

  defp dec_huffman_lookup(96, 0) do
    {:more, 92, 1}
  end

  defp dec_huffman_lookup(96, 1) do
    {:ok, 92, 22}
  end

  defp dec_huffman_lookup(96, 2) do
    {:more, 195, 1}
  end

  defp dec_huffman_lookup(96, 3) do
    {:ok, 195, 22}
  end

  defp dec_huffman_lookup(96, 4) do
    {:more, 208, 1}
  end

  defp dec_huffman_lookup(96, 5) do
    {:ok, 208, 22}
  end

  defp dec_huffman_lookup(96, 6) do
    {:ok, 128, 0}
  end

  defp dec_huffman_lookup(96, 7) do
    {:ok, 130, 0}
  end

  defp dec_huffman_lookup(96, 8) do
    {:ok, 131, 0}
  end

  defp dec_huffman_lookup(96, 9) do
    {:ok, 162, 0}
  end

  defp dec_huffman_lookup(96, 10) do
    {:ok, 184, 0}
  end

  defp dec_huffman_lookup(96, 11) do
    {:ok, 194, 0}
  end

  defp dec_huffman_lookup(96, 12) do
    {:ok, 224, 0}
  end

  defp dec_huffman_lookup(96, 13) do
    {:ok, 226, 0}
  end

  defp dec_huffman_lookup(96, 14) do
    {:more, :undefined, 108}
  end

  defp dec_huffman_lookup(96, 15) do
    {:more, :undefined, 109}
  end

  defp dec_huffman_lookup(97, 0) do
    {:more, 92, 2}
  end

  defp dec_huffman_lookup(97, 1) do
    {:more, 92, 9}
  end

  defp dec_huffman_lookup(97, 2) do
    {:more, 92, 23}
  end

  defp dec_huffman_lookup(97, 3) do
    {:ok, 92, 40}
  end

  defp dec_huffman_lookup(97, 4) do
    {:more, 195, 2}
  end

  defp dec_huffman_lookup(97, 5) do
    {:more, 195, 9}
  end

  defp dec_huffman_lookup(97, 6) do
    {:more, 195, 23}
  end

  defp dec_huffman_lookup(97, 7) do
    {:ok, 195, 40}
  end

  defp dec_huffman_lookup(97, 8) do
    {:more, 208, 2}
  end

  defp dec_huffman_lookup(97, 9) do
    {:more, 208, 9}
  end

  defp dec_huffman_lookup(97, 10) do
    {:more, 208, 23}
  end

  defp dec_huffman_lookup(97, 11) do
    {:ok, 208, 40}
  end

  defp dec_huffman_lookup(97, 12) do
    {:more, 128, 1}
  end

  defp dec_huffman_lookup(97, 13) do
    {:ok, 128, 22}
  end

  defp dec_huffman_lookup(97, 14) do
    {:more, 130, 1}
  end

  defp dec_huffman_lookup(97, 15) do
    {:ok, 130, 22}
  end

  defp dec_huffman_lookup(98, 0) do
    {:more, 92, 3}
  end

  defp dec_huffman_lookup(98, 1) do
    {:more, 92, 6}
  end

  defp dec_huffman_lookup(98, 2) do
    {:more, 92, 10}
  end

  defp dec_huffman_lookup(98, 3) do
    {:more, 92, 15}
  end

  defp dec_huffman_lookup(98, 4) do
    {:more, 92, 24}
  end

  defp dec_huffman_lookup(98, 5) do
    {:more, 92, 31}
  end

  defp dec_huffman_lookup(98, 6) do
    {:more, 92, 41}
  end

  defp dec_huffman_lookup(98, 7) do
    {:ok, 92, 56}
  end

  defp dec_huffman_lookup(98, 8) do
    {:more, 195, 3}
  end

  defp dec_huffman_lookup(98, 9) do
    {:more, 195, 6}
  end

  defp dec_huffman_lookup(98, 10) do
    {:more, 195, 10}
  end

  defp dec_huffman_lookup(98, 11) do
    {:more, 195, 15}
  end

  defp dec_huffman_lookup(98, 12) do
    {:more, 195, 24}
  end

  defp dec_huffman_lookup(98, 13) do
    {:more, 195, 31}
  end

  defp dec_huffman_lookup(98, 14) do
    {:more, 195, 41}
  end

  defp dec_huffman_lookup(98, 15) do
    {:ok, 195, 56}
  end

  defp dec_huffman_lookup(99, 0) do
    {:more, 208, 3}
  end

  defp dec_huffman_lookup(99, 1) do
    {:more, 208, 6}
  end

  defp dec_huffman_lookup(99, 2) do
    {:more, 208, 10}
  end

  defp dec_huffman_lookup(99, 3) do
    {:more, 208, 15}
  end

  defp dec_huffman_lookup(99, 4) do
    {:more, 208, 24}
  end

  defp dec_huffman_lookup(99, 5) do
    {:more, 208, 31}
  end

  defp dec_huffman_lookup(99, 6) do
    {:more, 208, 41}
  end

  defp dec_huffman_lookup(99, 7) do
    {:ok, 208, 56}
  end

  defp dec_huffman_lookup(99, 8) do
    {:more, 128, 2}
  end

  defp dec_huffman_lookup(99, 9) do
    {:more, 128, 9}
  end

  defp dec_huffman_lookup(99, 10) do
    {:more, 128, 23}
  end

  defp dec_huffman_lookup(99, 11) do
    {:ok, 128, 40}
  end

  defp dec_huffman_lookup(99, 12) do
    {:more, 130, 2}
  end

  defp dec_huffman_lookup(99, 13) do
    {:more, 130, 9}
  end

  defp dec_huffman_lookup(99, 14) do
    {:more, 130, 23}
  end

  defp dec_huffman_lookup(99, 15) do
    {:ok, 130, 40}
  end

  defp dec_huffman_lookup(100, 0) do
    {:more, 128, 3}
  end

  defp dec_huffman_lookup(100, 1) do
    {:more, 128, 6}
  end

  defp dec_huffman_lookup(100, 2) do
    {:more, 128, 10}
  end

  defp dec_huffman_lookup(100, 3) do
    {:more, 128, 15}
  end

  defp dec_huffman_lookup(100, 4) do
    {:more, 128, 24}
  end

  defp dec_huffman_lookup(100, 5) do
    {:more, 128, 31}
  end

  defp dec_huffman_lookup(100, 6) do
    {:more, 128, 41}
  end

  defp dec_huffman_lookup(100, 7) do
    {:ok, 128, 56}
  end

  defp dec_huffman_lookup(100, 8) do
    {:more, 130, 3}
  end

  defp dec_huffman_lookup(100, 9) do
    {:more, 130, 6}
  end

  defp dec_huffman_lookup(100, 10) do
    {:more, 130, 10}
  end

  defp dec_huffman_lookup(100, 11) do
    {:more, 130, 15}
  end

  defp dec_huffman_lookup(100, 12) do
    {:more, 130, 24}
  end

  defp dec_huffman_lookup(100, 13) do
    {:more, 130, 31}
  end

  defp dec_huffman_lookup(100, 14) do
    {:more, 130, 41}
  end

  defp dec_huffman_lookup(100, 15) do
    {:ok, 130, 56}
  end

  defp dec_huffman_lookup(101, 0) do
    {:more, 131, 1}
  end

  defp dec_huffman_lookup(101, 1) do
    {:ok, 131, 22}
  end

  defp dec_huffman_lookup(101, 2) do
    {:more, 162, 1}
  end

  defp dec_huffman_lookup(101, 3) do
    {:ok, 162, 22}
  end

  defp dec_huffman_lookup(101, 4) do
    {:more, 184, 1}
  end

  defp dec_huffman_lookup(101, 5) do
    {:ok, 184, 22}
  end

  defp dec_huffman_lookup(101, 6) do
    {:more, 194, 1}
  end

  defp dec_huffman_lookup(101, 7) do
    {:ok, 194, 22}
  end

  defp dec_huffman_lookup(101, 8) do
    {:more, 224, 1}
  end

  defp dec_huffman_lookup(101, 9) do
    {:ok, 224, 22}
  end

  defp dec_huffman_lookup(101, 10) do
    {:more, 226, 1}
  end

  defp dec_huffman_lookup(101, 11) do
    {:ok, 226, 22}
  end

  defp dec_huffman_lookup(101, 12) do
    {:ok, 153, 0}
  end

  defp dec_huffman_lookup(101, 13) do
    {:ok, 161, 0}
  end

  defp dec_huffman_lookup(101, 14) do
    {:ok, 167, 0}
  end

  defp dec_huffman_lookup(101, 15) do
    {:ok, 172, 0}
  end

  defp dec_huffman_lookup(102, 0) do
    {:more, 131, 2}
  end

  defp dec_huffman_lookup(102, 1) do
    {:more, 131, 9}
  end

  defp dec_huffman_lookup(102, 2) do
    {:more, 131, 23}
  end

  defp dec_huffman_lookup(102, 3) do
    {:ok, 131, 40}
  end

  defp dec_huffman_lookup(102, 4) do
    {:more, 162, 2}
  end

  defp dec_huffman_lookup(102, 5) do
    {:more, 162, 9}
  end

  defp dec_huffman_lookup(102, 6) do
    {:more, 162, 23}
  end

  defp dec_huffman_lookup(102, 7) do
    {:ok, 162, 40}
  end

  defp dec_huffman_lookup(102, 8) do
    {:more, 184, 2}
  end

  defp dec_huffman_lookup(102, 9) do
    {:more, 184, 9}
  end

  defp dec_huffman_lookup(102, 10) do
    {:more, 184, 23}
  end

  defp dec_huffman_lookup(102, 11) do
    {:ok, 184, 40}
  end

  defp dec_huffman_lookup(102, 12) do
    {:more, 194, 2}
  end

  defp dec_huffman_lookup(102, 13) do
    {:more, 194, 9}
  end

  defp dec_huffman_lookup(102, 14) do
    {:more, 194, 23}
  end

  defp dec_huffman_lookup(102, 15) do
    {:ok, 194, 40}
  end

  defp dec_huffman_lookup(103, 0) do
    {:more, 131, 3}
  end

  defp dec_huffman_lookup(103, 1) do
    {:more, 131, 6}
  end

  defp dec_huffman_lookup(103, 2) do
    {:more, 131, 10}
  end

  defp dec_huffman_lookup(103, 3) do
    {:more, 131, 15}
  end

  defp dec_huffman_lookup(103, 4) do
    {:more, 131, 24}
  end

  defp dec_huffman_lookup(103, 5) do
    {:more, 131, 31}
  end

  defp dec_huffman_lookup(103, 6) do
    {:more, 131, 41}
  end

  defp dec_huffman_lookup(103, 7) do
    {:ok, 131, 56}
  end

  defp dec_huffman_lookup(103, 8) do
    {:more, 162, 3}
  end

  defp dec_huffman_lookup(103, 9) do
    {:more, 162, 6}
  end

  defp dec_huffman_lookup(103, 10) do
    {:more, 162, 10}
  end

  defp dec_huffman_lookup(103, 11) do
    {:more, 162, 15}
  end

  defp dec_huffman_lookup(103, 12) do
    {:more, 162, 24}
  end

  defp dec_huffman_lookup(103, 13) do
    {:more, 162, 31}
  end

  defp dec_huffman_lookup(103, 14) do
    {:more, 162, 41}
  end

  defp dec_huffman_lookup(103, 15) do
    {:ok, 162, 56}
  end

  defp dec_huffman_lookup(104, 0) do
    {:more, 184, 3}
  end

  defp dec_huffman_lookup(104, 1) do
    {:more, 184, 6}
  end

  defp dec_huffman_lookup(104, 2) do
    {:more, 184, 10}
  end

  defp dec_huffman_lookup(104, 3) do
    {:more, 184, 15}
  end

  defp dec_huffman_lookup(104, 4) do
    {:more, 184, 24}
  end

  defp dec_huffman_lookup(104, 5) do
    {:more, 184, 31}
  end

  defp dec_huffman_lookup(104, 6) do
    {:more, 184, 41}
  end

  defp dec_huffman_lookup(104, 7) do
    {:ok, 184, 56}
  end

  defp dec_huffman_lookup(104, 8) do
    {:more, 194, 3}
  end

  defp dec_huffman_lookup(104, 9) do
    {:more, 194, 6}
  end

  defp dec_huffman_lookup(104, 10) do
    {:more, 194, 10}
  end

  defp dec_huffman_lookup(104, 11) do
    {:more, 194, 15}
  end

  defp dec_huffman_lookup(104, 12) do
    {:more, 194, 24}
  end

  defp dec_huffman_lookup(104, 13) do
    {:more, 194, 31}
  end

  defp dec_huffman_lookup(104, 14) do
    {:more, 194, 41}
  end

  defp dec_huffman_lookup(104, 15) do
    {:ok, 194, 56}
  end

  defp dec_huffman_lookup(105, 0) do
    {:more, 224, 2}
  end

  defp dec_huffman_lookup(105, 1) do
    {:more, 224, 9}
  end

  defp dec_huffman_lookup(105, 2) do
    {:more, 224, 23}
  end

  defp dec_huffman_lookup(105, 3) do
    {:ok, 224, 40}
  end

  defp dec_huffman_lookup(105, 4) do
    {:more, 226, 2}
  end

  defp dec_huffman_lookup(105, 5) do
    {:more, 226, 9}
  end

  defp dec_huffman_lookup(105, 6) do
    {:more, 226, 23}
  end

  defp dec_huffman_lookup(105, 7) do
    {:ok, 226, 40}
  end

  defp dec_huffman_lookup(105, 8) do
    {:more, 153, 1}
  end

  defp dec_huffman_lookup(105, 9) do
    {:ok, 153, 22}
  end

  defp dec_huffman_lookup(105, 10) do
    {:more, 161, 1}
  end

  defp dec_huffman_lookup(105, 11) do
    {:ok, 161, 22}
  end

  defp dec_huffman_lookup(105, 12) do
    {:more, 167, 1}
  end

  defp dec_huffman_lookup(105, 13) do
    {:ok, 167, 22}
  end

  defp dec_huffman_lookup(105, 14) do
    {:more, 172, 1}
  end

  defp dec_huffman_lookup(105, 15) do
    {:ok, 172, 22}
  end

  defp dec_huffman_lookup(106, 0) do
    {:more, 224, 3}
  end

  defp dec_huffman_lookup(106, 1) do
    {:more, 224, 6}
  end

  defp dec_huffman_lookup(106, 2) do
    {:more, 224, 10}
  end

  defp dec_huffman_lookup(106, 3) do
    {:more, 224, 15}
  end

  defp dec_huffman_lookup(106, 4) do
    {:more, 224, 24}
  end

  defp dec_huffman_lookup(106, 5) do
    {:more, 224, 31}
  end

  defp dec_huffman_lookup(106, 6) do
    {:more, 224, 41}
  end

  defp dec_huffman_lookup(106, 7) do
    {:ok, 224, 56}
  end

  defp dec_huffman_lookup(106, 8) do
    {:more, 226, 3}
  end

  defp dec_huffman_lookup(106, 9) do
    {:more, 226, 6}
  end

  defp dec_huffman_lookup(106, 10) do
    {:more, 226, 10}
  end

  defp dec_huffman_lookup(106, 11) do
    {:more, 226, 15}
  end

  defp dec_huffman_lookup(106, 12) do
    {:more, 226, 24}
  end

  defp dec_huffman_lookup(106, 13) do
    {:more, 226, 31}
  end

  defp dec_huffman_lookup(106, 14) do
    {:more, 226, 41}
  end

  defp dec_huffman_lookup(106, 15) do
    {:ok, 226, 56}
  end

  defp dec_huffman_lookup(107, 0) do
    {:more, 153, 2}
  end

  defp dec_huffman_lookup(107, 1) do
    {:more, 153, 9}
  end

  defp dec_huffman_lookup(107, 2) do
    {:more, 153, 23}
  end

  defp dec_huffman_lookup(107, 3) do
    {:ok, 153, 40}
  end

  defp dec_huffman_lookup(107, 4) do
    {:more, 161, 2}
  end

  defp dec_huffman_lookup(107, 5) do
    {:more, 161, 9}
  end

  defp dec_huffman_lookup(107, 6) do
    {:more, 161, 23}
  end

  defp dec_huffman_lookup(107, 7) do
    {:ok, 161, 40}
  end

  defp dec_huffman_lookup(107, 8) do
    {:more, 167, 2}
  end

  defp dec_huffman_lookup(107, 9) do
    {:more, 167, 9}
  end

  defp dec_huffman_lookup(107, 10) do
    {:more, 167, 23}
  end

  defp dec_huffman_lookup(107, 11) do
    {:ok, 167, 40}
  end

  defp dec_huffman_lookup(107, 12) do
    {:more, 172, 2}
  end

  defp dec_huffman_lookup(107, 13) do
    {:more, 172, 9}
  end

  defp dec_huffman_lookup(107, 14) do
    {:more, 172, 23}
  end

  defp dec_huffman_lookup(107, 15) do
    {:ok, 172, 40}
  end

  defp dec_huffman_lookup(108, 0) do
    {:more, 153, 3}
  end

  defp dec_huffman_lookup(108, 1) do
    {:more, 153, 6}
  end

  defp dec_huffman_lookup(108, 2) do
    {:more, 153, 10}
  end

  defp dec_huffman_lookup(108, 3) do
    {:more, 153, 15}
  end

  defp dec_huffman_lookup(108, 4) do
    {:more, 153, 24}
  end

  defp dec_huffman_lookup(108, 5) do
    {:more, 153, 31}
  end

  defp dec_huffman_lookup(108, 6) do
    {:more, 153, 41}
  end

  defp dec_huffman_lookup(108, 7) do
    {:ok, 153, 56}
  end

  defp dec_huffman_lookup(108, 8) do
    {:more, 161, 3}
  end

  defp dec_huffman_lookup(108, 9) do
    {:more, 161, 6}
  end

  defp dec_huffman_lookup(108, 10) do
    {:more, 161, 10}
  end

  defp dec_huffman_lookup(108, 11) do
    {:more, 161, 15}
  end

  defp dec_huffman_lookup(108, 12) do
    {:more, 161, 24}
  end

  defp dec_huffman_lookup(108, 13) do
    {:more, 161, 31}
  end

  defp dec_huffman_lookup(108, 14) do
    {:more, 161, 41}
  end

  defp dec_huffman_lookup(108, 15) do
    {:ok, 161, 56}
  end

  defp dec_huffman_lookup(109, 0) do
    {:more, 167, 3}
  end

  defp dec_huffman_lookup(109, 1) do
    {:more, 167, 6}
  end

  defp dec_huffman_lookup(109, 2) do
    {:more, 167, 10}
  end

  defp dec_huffman_lookup(109, 3) do
    {:more, 167, 15}
  end

  defp dec_huffman_lookup(109, 4) do
    {:more, 167, 24}
  end

  defp dec_huffman_lookup(109, 5) do
    {:more, 167, 31}
  end

  defp dec_huffman_lookup(109, 6) do
    {:more, 167, 41}
  end

  defp dec_huffman_lookup(109, 7) do
    {:ok, 167, 56}
  end

  defp dec_huffman_lookup(109, 8) do
    {:more, 172, 3}
  end

  defp dec_huffman_lookup(109, 9) do
    {:more, 172, 6}
  end

  defp dec_huffman_lookup(109, 10) do
    {:more, 172, 10}
  end

  defp dec_huffman_lookup(109, 11) do
    {:more, 172, 15}
  end

  defp dec_huffman_lookup(109, 12) do
    {:more, 172, 24}
  end

  defp dec_huffman_lookup(109, 13) do
    {:more, 172, 31}
  end

  defp dec_huffman_lookup(109, 14) do
    {:more, 172, 41}
  end

  defp dec_huffman_lookup(109, 15) do
    {:ok, 172, 56}
  end

  defp dec_huffman_lookup(110, 0) do
    {:more, :undefined, 114}
  end

  defp dec_huffman_lookup(110, 1) do
    {:more, :undefined, 115}
  end

  defp dec_huffman_lookup(110, 2) do
    {:more, :undefined, 117}
  end

  defp dec_huffman_lookup(110, 3) do
    {:more, :undefined, 118}
  end

  defp dec_huffman_lookup(110, 4) do
    {:more, :undefined, 121}
  end

  defp dec_huffman_lookup(110, 5) do
    {:more, :undefined, 123}
  end

  defp dec_huffman_lookup(110, 6) do
    {:more, :undefined, 127}
  end

  defp dec_huffman_lookup(110, 7) do
    {:more, :undefined, 130}
  end

  defp dec_huffman_lookup(110, 8) do
    {:more, :undefined, 136}
  end

  defp dec_huffman_lookup(110, 9) do
    {:more, :undefined, 139}
  end

  defp dec_huffman_lookup(110, 10) do
    {:more, :undefined, 143}
  end

  defp dec_huffman_lookup(110, 11) do
    {:more, :undefined, 146}
  end

  defp dec_huffman_lookup(110, 12) do
    {:more, :undefined, 155}
  end

  defp dec_huffman_lookup(110, 13) do
    {:more, :undefined, 162}
  end

  defp dec_huffman_lookup(110, 14) do
    {:more, :undefined, 170}
  end

  defp dec_huffman_lookup(110, 15) do
    {:ok, :undefined, 180}
  end

  defp dec_huffman_lookup(111, 0) do
    {:ok, 176, 0}
  end

  defp dec_huffman_lookup(111, 1) do
    {:ok, 177, 0}
  end

  defp dec_huffman_lookup(111, 2) do
    {:ok, 179, 0}
  end

  defp dec_huffman_lookup(111, 3) do
    {:ok, 209, 0}
  end

  defp dec_huffman_lookup(111, 4) do
    {:ok, 216, 0}
  end

  defp dec_huffman_lookup(111, 5) do
    {:ok, 217, 0}
  end

  defp dec_huffman_lookup(111, 6) do
    {:ok, 227, 0}
  end

  defp dec_huffman_lookup(111, 7) do
    {:ok, 229, 0}
  end

  defp dec_huffman_lookup(111, 8) do
    {:ok, 230, 0}
  end

  defp dec_huffman_lookup(111, 9) do
    {:more, :undefined, 122}
  end

  defp dec_huffman_lookup(111, 10) do
    {:more, :undefined, 124}
  end

  defp dec_huffman_lookup(111, 11) do
    {:more, :undefined, 125}
  end

  defp dec_huffman_lookup(111, 12) do
    {:more, :undefined, 128}
  end

  defp dec_huffman_lookup(111, 13) do
    {:more, :undefined, 129}
  end

  defp dec_huffman_lookup(111, 14) do
    {:more, :undefined, 131}
  end

  defp dec_huffman_lookup(111, 15) do
    {:more, :undefined, 132}
  end

  defp dec_huffman_lookup(112, 0) do
    {:more, 176, 1}
  end

  defp dec_huffman_lookup(112, 1) do
    {:ok, 176, 22}
  end

  defp dec_huffman_lookup(112, 2) do
    {:more, 177, 1}
  end

  defp dec_huffman_lookup(112, 3) do
    {:ok, 177, 22}
  end

  defp dec_huffman_lookup(112, 4) do
    {:more, 179, 1}
  end

  defp dec_huffman_lookup(112, 5) do
    {:ok, 179, 22}
  end

  defp dec_huffman_lookup(112, 6) do
    {:more, 209, 1}
  end

  defp dec_huffman_lookup(112, 7) do
    {:ok, 209, 22}
  end

  defp dec_huffman_lookup(112, 8) do
    {:more, 216, 1}
  end

  defp dec_huffman_lookup(112, 9) do
    {:ok, 216, 22}
  end

  defp dec_huffman_lookup(112, 10) do
    {:more, 217, 1}
  end

  defp dec_huffman_lookup(112, 11) do
    {:ok, 217, 22}
  end

  defp dec_huffman_lookup(112, 12) do
    {:more, 227, 1}
  end

  defp dec_huffman_lookup(112, 13) do
    {:ok, 227, 22}
  end

  defp dec_huffman_lookup(112, 14) do
    {:more, 229, 1}
  end

  defp dec_huffman_lookup(112, 15) do
    {:ok, 229, 22}
  end

  defp dec_huffman_lookup(113, 0) do
    {:more, 176, 2}
  end

  defp dec_huffman_lookup(113, 1) do
    {:more, 176, 9}
  end

  defp dec_huffman_lookup(113, 2) do
    {:more, 176, 23}
  end

  defp dec_huffman_lookup(113, 3) do
    {:ok, 176, 40}
  end

  defp dec_huffman_lookup(113, 4) do
    {:more, 177, 2}
  end

  defp dec_huffman_lookup(113, 5) do
    {:more, 177, 9}
  end

  defp dec_huffman_lookup(113, 6) do
    {:more, 177, 23}
  end

  defp dec_huffman_lookup(113, 7) do
    {:ok, 177, 40}
  end

  defp dec_huffman_lookup(113, 8) do
    {:more, 179, 2}
  end

  defp dec_huffman_lookup(113, 9) do
    {:more, 179, 9}
  end

  defp dec_huffman_lookup(113, 10) do
    {:more, 179, 23}
  end

  defp dec_huffman_lookup(113, 11) do
    {:ok, 179, 40}
  end

  defp dec_huffman_lookup(113, 12) do
    {:more, 209, 2}
  end

  defp dec_huffman_lookup(113, 13) do
    {:more, 209, 9}
  end

  defp dec_huffman_lookup(113, 14) do
    {:more, 209, 23}
  end

  defp dec_huffman_lookup(113, 15) do
    {:ok, 209, 40}
  end

  defp dec_huffman_lookup(114, 0) do
    {:more, 176, 3}
  end

  defp dec_huffman_lookup(114, 1) do
    {:more, 176, 6}
  end

  defp dec_huffman_lookup(114, 2) do
    {:more, 176, 10}
  end

  defp dec_huffman_lookup(114, 3) do
    {:more, 176, 15}
  end

  defp dec_huffman_lookup(114, 4) do
    {:more, 176, 24}
  end

  defp dec_huffman_lookup(114, 5) do
    {:more, 176, 31}
  end

  defp dec_huffman_lookup(114, 6) do
    {:more, 176, 41}
  end

  defp dec_huffman_lookup(114, 7) do
    {:ok, 176, 56}
  end

  defp dec_huffman_lookup(114, 8) do
    {:more, 177, 3}
  end

  defp dec_huffman_lookup(114, 9) do
    {:more, 177, 6}
  end

  defp dec_huffman_lookup(114, 10) do
    {:more, 177, 10}
  end

  defp dec_huffman_lookup(114, 11) do
    {:more, 177, 15}
  end

  defp dec_huffman_lookup(114, 12) do
    {:more, 177, 24}
  end

  defp dec_huffman_lookup(114, 13) do
    {:more, 177, 31}
  end

  defp dec_huffman_lookup(114, 14) do
    {:more, 177, 41}
  end

  defp dec_huffman_lookup(114, 15) do
    {:ok, 177, 56}
  end

  defp dec_huffman_lookup(115, 0) do
    {:more, 179, 3}
  end

  defp dec_huffman_lookup(115, 1) do
    {:more, 179, 6}
  end

  defp dec_huffman_lookup(115, 2) do
    {:more, 179, 10}
  end

  defp dec_huffman_lookup(115, 3) do
    {:more, 179, 15}
  end

  defp dec_huffman_lookup(115, 4) do
    {:more, 179, 24}
  end

  defp dec_huffman_lookup(115, 5) do
    {:more, 179, 31}
  end

  defp dec_huffman_lookup(115, 6) do
    {:more, 179, 41}
  end

  defp dec_huffman_lookup(115, 7) do
    {:ok, 179, 56}
  end

  defp dec_huffman_lookup(115, 8) do
    {:more, 209, 3}
  end

  defp dec_huffman_lookup(115, 9) do
    {:more, 209, 6}
  end

  defp dec_huffman_lookup(115, 10) do
    {:more, 209, 10}
  end

  defp dec_huffman_lookup(115, 11) do
    {:more, 209, 15}
  end

  defp dec_huffman_lookup(115, 12) do
    {:more, 209, 24}
  end

  defp dec_huffman_lookup(115, 13) do
    {:more, 209, 31}
  end

  defp dec_huffman_lookup(115, 14) do
    {:more, 209, 41}
  end

  defp dec_huffman_lookup(115, 15) do
    {:ok, 209, 56}
  end

  defp dec_huffman_lookup(116, 0) do
    {:more, 216, 2}
  end

  defp dec_huffman_lookup(116, 1) do
    {:more, 216, 9}
  end

  defp dec_huffman_lookup(116, 2) do
    {:more, 216, 23}
  end

  defp dec_huffman_lookup(116, 3) do
    {:ok, 216, 40}
  end

  defp dec_huffman_lookup(116, 4) do
    {:more, 217, 2}
  end

  defp dec_huffman_lookup(116, 5) do
    {:more, 217, 9}
  end

  defp dec_huffman_lookup(116, 6) do
    {:more, 217, 23}
  end

  defp dec_huffman_lookup(116, 7) do
    {:ok, 217, 40}
  end

  defp dec_huffman_lookup(116, 8) do
    {:more, 227, 2}
  end

  defp dec_huffman_lookup(116, 9) do
    {:more, 227, 9}
  end

  defp dec_huffman_lookup(116, 10) do
    {:more, 227, 23}
  end

  defp dec_huffman_lookup(116, 11) do
    {:ok, 227, 40}
  end

  defp dec_huffman_lookup(116, 12) do
    {:more, 229, 2}
  end

  defp dec_huffman_lookup(116, 13) do
    {:more, 229, 9}
  end

  defp dec_huffman_lookup(116, 14) do
    {:more, 229, 23}
  end

  defp dec_huffman_lookup(116, 15) do
    {:ok, 229, 40}
  end

  defp dec_huffman_lookup(117, 0) do
    {:more, 216, 3}
  end

  defp dec_huffman_lookup(117, 1) do
    {:more, 216, 6}
  end

  defp dec_huffman_lookup(117, 2) do
    {:more, 216, 10}
  end

  defp dec_huffman_lookup(117, 3) do
    {:more, 216, 15}
  end

  defp dec_huffman_lookup(117, 4) do
    {:more, 216, 24}
  end

  defp dec_huffman_lookup(117, 5) do
    {:more, 216, 31}
  end

  defp dec_huffman_lookup(117, 6) do
    {:more, 216, 41}
  end

  defp dec_huffman_lookup(117, 7) do
    {:ok, 216, 56}
  end

  defp dec_huffman_lookup(117, 8) do
    {:more, 217, 3}
  end

  defp dec_huffman_lookup(117, 9) do
    {:more, 217, 6}
  end

  defp dec_huffman_lookup(117, 10) do
    {:more, 217, 10}
  end

  defp dec_huffman_lookup(117, 11) do
    {:more, 217, 15}
  end

  defp dec_huffman_lookup(117, 12) do
    {:more, 217, 24}
  end

  defp dec_huffman_lookup(117, 13) do
    {:more, 217, 31}
  end

  defp dec_huffman_lookup(117, 14) do
    {:more, 217, 41}
  end

  defp dec_huffman_lookup(117, 15) do
    {:ok, 217, 56}
  end

  defp dec_huffman_lookup(118, 0) do
    {:more, 227, 3}
  end

  defp dec_huffman_lookup(118, 1) do
    {:more, 227, 6}
  end

  defp dec_huffman_lookup(118, 2) do
    {:more, 227, 10}
  end

  defp dec_huffman_lookup(118, 3) do
    {:more, 227, 15}
  end

  defp dec_huffman_lookup(118, 4) do
    {:more, 227, 24}
  end

  defp dec_huffman_lookup(118, 5) do
    {:more, 227, 31}
  end

  defp dec_huffman_lookup(118, 6) do
    {:more, 227, 41}
  end

  defp dec_huffman_lookup(118, 7) do
    {:ok, 227, 56}
  end

  defp dec_huffman_lookup(118, 8) do
    {:more, 229, 3}
  end

  defp dec_huffman_lookup(118, 9) do
    {:more, 229, 6}
  end

  defp dec_huffman_lookup(118, 10) do
    {:more, 229, 10}
  end

  defp dec_huffman_lookup(118, 11) do
    {:more, 229, 15}
  end

  defp dec_huffman_lookup(118, 12) do
    {:more, 229, 24}
  end

  defp dec_huffman_lookup(118, 13) do
    {:more, 229, 31}
  end

  defp dec_huffman_lookup(118, 14) do
    {:more, 229, 41}
  end

  defp dec_huffman_lookup(118, 15) do
    {:ok, 229, 56}
  end

  defp dec_huffman_lookup(119, 0) do
    {:more, 230, 1}
  end

  defp dec_huffman_lookup(119, 1) do
    {:ok, 230, 22}
  end

  defp dec_huffman_lookup(119, 2) do
    {:ok, 129, 0}
  end

  defp dec_huffman_lookup(119, 3) do
    {:ok, 132, 0}
  end

  defp dec_huffman_lookup(119, 4) do
    {:ok, 133, 0}
  end

  defp dec_huffman_lookup(119, 5) do
    {:ok, 134, 0}
  end

  defp dec_huffman_lookup(119, 6) do
    {:ok, 136, 0}
  end

  defp dec_huffman_lookup(119, 7) do
    {:ok, 146, 0}
  end

  defp dec_huffman_lookup(119, 8) do
    {:ok, 154, 0}
  end

  defp dec_huffman_lookup(119, 9) do
    {:ok, 156, 0}
  end

  defp dec_huffman_lookup(119, 10) do
    {:ok, 160, 0}
  end

  defp dec_huffman_lookup(119, 11) do
    {:ok, 163, 0}
  end

  defp dec_huffman_lookup(119, 12) do
    {:ok, 164, 0}
  end

  defp dec_huffman_lookup(119, 13) do
    {:ok, 169, 0}
  end

  defp dec_huffman_lookup(119, 14) do
    {:ok, 170, 0}
  end

  defp dec_huffman_lookup(119, 15) do
    {:ok, 173, 0}
  end

  defp dec_huffman_lookup(120, 0) do
    {:more, 230, 2}
  end

  defp dec_huffman_lookup(120, 1) do
    {:more, 230, 9}
  end

  defp dec_huffman_lookup(120, 2) do
    {:more, 230, 23}
  end

  defp dec_huffman_lookup(120, 3) do
    {:ok, 230, 40}
  end

  defp dec_huffman_lookup(120, 4) do
    {:more, 129, 1}
  end

  defp dec_huffman_lookup(120, 5) do
    {:ok, 129, 22}
  end

  defp dec_huffman_lookup(120, 6) do
    {:more, 132, 1}
  end

  defp dec_huffman_lookup(120, 7) do
    {:ok, 132, 22}
  end

  defp dec_huffman_lookup(120, 8) do
    {:more, 133, 1}
  end

  defp dec_huffman_lookup(120, 9) do
    {:ok, 133, 22}
  end

  defp dec_huffman_lookup(120, 10) do
    {:more, 134, 1}
  end

  defp dec_huffman_lookup(120, 11) do
    {:ok, 134, 22}
  end

  defp dec_huffman_lookup(120, 12) do
    {:more, 136, 1}
  end

  defp dec_huffman_lookup(120, 13) do
    {:ok, 136, 22}
  end

  defp dec_huffman_lookup(120, 14) do
    {:more, 146, 1}
  end

  defp dec_huffman_lookup(120, 15) do
    {:ok, 146, 22}
  end

  defp dec_huffman_lookup(121, 0) do
    {:more, 230, 3}
  end

  defp dec_huffman_lookup(121, 1) do
    {:more, 230, 6}
  end

  defp dec_huffman_lookup(121, 2) do
    {:more, 230, 10}
  end

  defp dec_huffman_lookup(121, 3) do
    {:more, 230, 15}
  end

  defp dec_huffman_lookup(121, 4) do
    {:more, 230, 24}
  end

  defp dec_huffman_lookup(121, 5) do
    {:more, 230, 31}
  end

  defp dec_huffman_lookup(121, 6) do
    {:more, 230, 41}
  end

  defp dec_huffman_lookup(121, 7) do
    {:ok, 230, 56}
  end

  defp dec_huffman_lookup(121, 8) do
    {:more, 129, 2}
  end

  defp dec_huffman_lookup(121, 9) do
    {:more, 129, 9}
  end

  defp dec_huffman_lookup(121, 10) do
    {:more, 129, 23}
  end

  defp dec_huffman_lookup(121, 11) do
    {:ok, 129, 40}
  end

  defp dec_huffman_lookup(121, 12) do
    {:more, 132, 2}
  end

  defp dec_huffman_lookup(121, 13) do
    {:more, 132, 9}
  end

  defp dec_huffman_lookup(121, 14) do
    {:more, 132, 23}
  end

  defp dec_huffman_lookup(121, 15) do
    {:ok, 132, 40}
  end

  defp dec_huffman_lookup(122, 0) do
    {:more, 129, 3}
  end

  defp dec_huffman_lookup(122, 1) do
    {:more, 129, 6}
  end

  defp dec_huffman_lookup(122, 2) do
    {:more, 129, 10}
  end

  defp dec_huffman_lookup(122, 3) do
    {:more, 129, 15}
  end

  defp dec_huffman_lookup(122, 4) do
    {:more, 129, 24}
  end

  defp dec_huffman_lookup(122, 5) do
    {:more, 129, 31}
  end

  defp dec_huffman_lookup(122, 6) do
    {:more, 129, 41}
  end

  defp dec_huffman_lookup(122, 7) do
    {:ok, 129, 56}
  end

  defp dec_huffman_lookup(122, 8) do
    {:more, 132, 3}
  end

  defp dec_huffman_lookup(122, 9) do
    {:more, 132, 6}
  end

  defp dec_huffman_lookup(122, 10) do
    {:more, 132, 10}
  end

  defp dec_huffman_lookup(122, 11) do
    {:more, 132, 15}
  end

  defp dec_huffman_lookup(122, 12) do
    {:more, 132, 24}
  end

  defp dec_huffman_lookup(122, 13) do
    {:more, 132, 31}
  end

  defp dec_huffman_lookup(122, 14) do
    {:more, 132, 41}
  end

  defp dec_huffman_lookup(122, 15) do
    {:ok, 132, 56}
  end

  defp dec_huffman_lookup(123, 0) do
    {:more, 133, 2}
  end

  defp dec_huffman_lookup(123, 1) do
    {:more, 133, 9}
  end

  defp dec_huffman_lookup(123, 2) do
    {:more, 133, 23}
  end

  defp dec_huffman_lookup(123, 3) do
    {:ok, 133, 40}
  end

  defp dec_huffman_lookup(123, 4) do
    {:more, 134, 2}
  end

  defp dec_huffman_lookup(123, 5) do
    {:more, 134, 9}
  end

  defp dec_huffman_lookup(123, 6) do
    {:more, 134, 23}
  end

  defp dec_huffman_lookup(123, 7) do
    {:ok, 134, 40}
  end

  defp dec_huffman_lookup(123, 8) do
    {:more, 136, 2}
  end

  defp dec_huffman_lookup(123, 9) do
    {:more, 136, 9}
  end

  defp dec_huffman_lookup(123, 10) do
    {:more, 136, 23}
  end

  defp dec_huffman_lookup(123, 11) do
    {:ok, 136, 40}
  end

  defp dec_huffman_lookup(123, 12) do
    {:more, 146, 2}
  end

  defp dec_huffman_lookup(123, 13) do
    {:more, 146, 9}
  end

  defp dec_huffman_lookup(123, 14) do
    {:more, 146, 23}
  end

  defp dec_huffman_lookup(123, 15) do
    {:ok, 146, 40}
  end

  defp dec_huffman_lookup(124, 0) do
    {:more, 133, 3}
  end

  defp dec_huffman_lookup(124, 1) do
    {:more, 133, 6}
  end

  defp dec_huffman_lookup(124, 2) do
    {:more, 133, 10}
  end

  defp dec_huffman_lookup(124, 3) do
    {:more, 133, 15}
  end

  defp dec_huffman_lookup(124, 4) do
    {:more, 133, 24}
  end

  defp dec_huffman_lookup(124, 5) do
    {:more, 133, 31}
  end

  defp dec_huffman_lookup(124, 6) do
    {:more, 133, 41}
  end

  defp dec_huffman_lookup(124, 7) do
    {:ok, 133, 56}
  end

  defp dec_huffman_lookup(124, 8) do
    {:more, 134, 3}
  end

  defp dec_huffman_lookup(124, 9) do
    {:more, 134, 6}
  end

  defp dec_huffman_lookup(124, 10) do
    {:more, 134, 10}
  end

  defp dec_huffman_lookup(124, 11) do
    {:more, 134, 15}
  end

  defp dec_huffman_lookup(124, 12) do
    {:more, 134, 24}
  end

  defp dec_huffman_lookup(124, 13) do
    {:more, 134, 31}
  end

  defp dec_huffman_lookup(124, 14) do
    {:more, 134, 41}
  end

  defp dec_huffman_lookup(124, 15) do
    {:ok, 134, 56}
  end

  defp dec_huffman_lookup(125, 0) do
    {:more, 136, 3}
  end

  defp dec_huffman_lookup(125, 1) do
    {:more, 136, 6}
  end

  defp dec_huffman_lookup(125, 2) do
    {:more, 136, 10}
  end

  defp dec_huffman_lookup(125, 3) do
    {:more, 136, 15}
  end

  defp dec_huffman_lookup(125, 4) do
    {:more, 136, 24}
  end

  defp dec_huffman_lookup(125, 5) do
    {:more, 136, 31}
  end

  defp dec_huffman_lookup(125, 6) do
    {:more, 136, 41}
  end

  defp dec_huffman_lookup(125, 7) do
    {:ok, 136, 56}
  end

  defp dec_huffman_lookup(125, 8) do
    {:more, 146, 3}
  end

  defp dec_huffman_lookup(125, 9) do
    {:more, 146, 6}
  end

  defp dec_huffman_lookup(125, 10) do
    {:more, 146, 10}
  end

  defp dec_huffman_lookup(125, 11) do
    {:more, 146, 15}
  end

  defp dec_huffman_lookup(125, 12) do
    {:more, 146, 24}
  end

  defp dec_huffman_lookup(125, 13) do
    {:more, 146, 31}
  end

  defp dec_huffman_lookup(125, 14) do
    {:more, 146, 41}
  end

  defp dec_huffman_lookup(125, 15) do
    {:ok, 146, 56}
  end

  defp dec_huffman_lookup(126, 0) do
    {:more, 154, 1}
  end

  defp dec_huffman_lookup(126, 1) do
    {:ok, 154, 22}
  end

  defp dec_huffman_lookup(126, 2) do
    {:more, 156, 1}
  end

  defp dec_huffman_lookup(126, 3) do
    {:ok, 156, 22}
  end

  defp dec_huffman_lookup(126, 4) do
    {:more, 160, 1}
  end

  defp dec_huffman_lookup(126, 5) do
    {:ok, 160, 22}
  end

  defp dec_huffman_lookup(126, 6) do
    {:more, 163, 1}
  end

  defp dec_huffman_lookup(126, 7) do
    {:ok, 163, 22}
  end

  defp dec_huffman_lookup(126, 8) do
    {:more, 164, 1}
  end

  defp dec_huffman_lookup(126, 9) do
    {:ok, 164, 22}
  end

  defp dec_huffman_lookup(126, 10) do
    {:more, 169, 1}
  end

  defp dec_huffman_lookup(126, 11) do
    {:ok, 169, 22}
  end

  defp dec_huffman_lookup(126, 12) do
    {:more, 170, 1}
  end

  defp dec_huffman_lookup(126, 13) do
    {:ok, 170, 22}
  end

  defp dec_huffman_lookup(126, 14) do
    {:more, 173, 1}
  end

  defp dec_huffman_lookup(126, 15) do
    {:ok, 173, 22}
  end

  defp dec_huffman_lookup(127, 0) do
    {:more, 154, 2}
  end

  defp dec_huffman_lookup(127, 1) do
    {:more, 154, 9}
  end

  defp dec_huffman_lookup(127, 2) do
    {:more, 154, 23}
  end

  defp dec_huffman_lookup(127, 3) do
    {:ok, 154, 40}
  end

  defp dec_huffman_lookup(127, 4) do
    {:more, 156, 2}
  end

  defp dec_huffman_lookup(127, 5) do
    {:more, 156, 9}
  end

  defp dec_huffman_lookup(127, 6) do
    {:more, 156, 23}
  end

  defp dec_huffman_lookup(127, 7) do
    {:ok, 156, 40}
  end

  defp dec_huffman_lookup(127, 8) do
    {:more, 160, 2}
  end

  defp dec_huffman_lookup(127, 9) do
    {:more, 160, 9}
  end

  defp dec_huffman_lookup(127, 10) do
    {:more, 160, 23}
  end

  defp dec_huffman_lookup(127, 11) do
    {:ok, 160, 40}
  end

  defp dec_huffman_lookup(127, 12) do
    {:more, 163, 2}
  end

  defp dec_huffman_lookup(127, 13) do
    {:more, 163, 9}
  end

  defp dec_huffman_lookup(127, 14) do
    {:more, 163, 23}
  end

  defp dec_huffman_lookup(127, 15) do
    {:ok, 163, 40}
  end

  defp dec_huffman_lookup(128, 0) do
    {:more, 154, 3}
  end

  defp dec_huffman_lookup(128, 1) do
    {:more, 154, 6}
  end

  defp dec_huffman_lookup(128, 2) do
    {:more, 154, 10}
  end

  defp dec_huffman_lookup(128, 3) do
    {:more, 154, 15}
  end

  defp dec_huffman_lookup(128, 4) do
    {:more, 154, 24}
  end

  defp dec_huffman_lookup(128, 5) do
    {:more, 154, 31}
  end

  defp dec_huffman_lookup(128, 6) do
    {:more, 154, 41}
  end

  defp dec_huffman_lookup(128, 7) do
    {:ok, 154, 56}
  end

  defp dec_huffman_lookup(128, 8) do
    {:more, 156, 3}
  end

  defp dec_huffman_lookup(128, 9) do
    {:more, 156, 6}
  end

  defp dec_huffman_lookup(128, 10) do
    {:more, 156, 10}
  end

  defp dec_huffman_lookup(128, 11) do
    {:more, 156, 15}
  end

  defp dec_huffman_lookup(128, 12) do
    {:more, 156, 24}
  end

  defp dec_huffman_lookup(128, 13) do
    {:more, 156, 31}
  end

  defp dec_huffman_lookup(128, 14) do
    {:more, 156, 41}
  end

  defp dec_huffman_lookup(128, 15) do
    {:ok, 156, 56}
  end

  defp dec_huffman_lookup(129, 0) do
    {:more, 160, 3}
  end

  defp dec_huffman_lookup(129, 1) do
    {:more, 160, 6}
  end

  defp dec_huffman_lookup(129, 2) do
    {:more, 160, 10}
  end

  defp dec_huffman_lookup(129, 3) do
    {:more, 160, 15}
  end

  defp dec_huffman_lookup(129, 4) do
    {:more, 160, 24}
  end

  defp dec_huffman_lookup(129, 5) do
    {:more, 160, 31}
  end

  defp dec_huffman_lookup(129, 6) do
    {:more, 160, 41}
  end

  defp dec_huffman_lookup(129, 7) do
    {:ok, 160, 56}
  end

  defp dec_huffman_lookup(129, 8) do
    {:more, 163, 3}
  end

  defp dec_huffman_lookup(129, 9) do
    {:more, 163, 6}
  end

  defp dec_huffman_lookup(129, 10) do
    {:more, 163, 10}
  end

  defp dec_huffman_lookup(129, 11) do
    {:more, 163, 15}
  end

  defp dec_huffman_lookup(129, 12) do
    {:more, 163, 24}
  end

  defp dec_huffman_lookup(129, 13) do
    {:more, 163, 31}
  end

  defp dec_huffman_lookup(129, 14) do
    {:more, 163, 41}
  end

  defp dec_huffman_lookup(129, 15) do
    {:ok, 163, 56}
  end

  defp dec_huffman_lookup(130, 0) do
    {:more, 164, 2}
  end

  defp dec_huffman_lookup(130, 1) do
    {:more, 164, 9}
  end

  defp dec_huffman_lookup(130, 2) do
    {:more, 164, 23}
  end

  defp dec_huffman_lookup(130, 3) do
    {:ok, 164, 40}
  end

  defp dec_huffman_lookup(130, 4) do
    {:more, 169, 2}
  end

  defp dec_huffman_lookup(130, 5) do
    {:more, 169, 9}
  end

  defp dec_huffman_lookup(130, 6) do
    {:more, 169, 23}
  end

  defp dec_huffman_lookup(130, 7) do
    {:ok, 169, 40}
  end

  defp dec_huffman_lookup(130, 8) do
    {:more, 170, 2}
  end

  defp dec_huffman_lookup(130, 9) do
    {:more, 170, 9}
  end

  defp dec_huffman_lookup(130, 10) do
    {:more, 170, 23}
  end

  defp dec_huffman_lookup(130, 11) do
    {:ok, 170, 40}
  end

  defp dec_huffman_lookup(130, 12) do
    {:more, 173, 2}
  end

  defp dec_huffman_lookup(130, 13) do
    {:more, 173, 9}
  end

  defp dec_huffman_lookup(130, 14) do
    {:more, 173, 23}
  end

  defp dec_huffman_lookup(130, 15) do
    {:ok, 173, 40}
  end

  defp dec_huffman_lookup(131, 0) do
    {:more, 164, 3}
  end

  defp dec_huffman_lookup(131, 1) do
    {:more, 164, 6}
  end

  defp dec_huffman_lookup(131, 2) do
    {:more, 164, 10}
  end

  defp dec_huffman_lookup(131, 3) do
    {:more, 164, 15}
  end

  defp dec_huffman_lookup(131, 4) do
    {:more, 164, 24}
  end

  defp dec_huffman_lookup(131, 5) do
    {:more, 164, 31}
  end

  defp dec_huffman_lookup(131, 6) do
    {:more, 164, 41}
  end

  defp dec_huffman_lookup(131, 7) do
    {:ok, 164, 56}
  end

  defp dec_huffman_lookup(131, 8) do
    {:more, 169, 3}
  end

  defp dec_huffman_lookup(131, 9) do
    {:more, 169, 6}
  end

  defp dec_huffman_lookup(131, 10) do
    {:more, 169, 10}
  end

  defp dec_huffman_lookup(131, 11) do
    {:more, 169, 15}
  end

  defp dec_huffman_lookup(131, 12) do
    {:more, 169, 24}
  end

  defp dec_huffman_lookup(131, 13) do
    {:more, 169, 31}
  end

  defp dec_huffman_lookup(131, 14) do
    {:more, 169, 41}
  end

  defp dec_huffman_lookup(131, 15) do
    {:ok, 169, 56}
  end

  defp dec_huffman_lookup(132, 0) do
    {:more, 170, 3}
  end

  defp dec_huffman_lookup(132, 1) do
    {:more, 170, 6}
  end

  defp dec_huffman_lookup(132, 2) do
    {:more, 170, 10}
  end

  defp dec_huffman_lookup(132, 3) do
    {:more, 170, 15}
  end

  defp dec_huffman_lookup(132, 4) do
    {:more, 170, 24}
  end

  defp dec_huffman_lookup(132, 5) do
    {:more, 170, 31}
  end

  defp dec_huffman_lookup(132, 6) do
    {:more, 170, 41}
  end

  defp dec_huffman_lookup(132, 7) do
    {:ok, 170, 56}
  end

  defp dec_huffman_lookup(132, 8) do
    {:more, 173, 3}
  end

  defp dec_huffman_lookup(132, 9) do
    {:more, 173, 6}
  end

  defp dec_huffman_lookup(132, 10) do
    {:more, 173, 10}
  end

  defp dec_huffman_lookup(132, 11) do
    {:more, 173, 15}
  end

  defp dec_huffman_lookup(132, 12) do
    {:more, 173, 24}
  end

  defp dec_huffman_lookup(132, 13) do
    {:more, 173, 31}
  end

  defp dec_huffman_lookup(132, 14) do
    {:more, 173, 41}
  end

  defp dec_huffman_lookup(132, 15) do
    {:ok, 173, 56}
  end

  defp dec_huffman_lookup(133, 0) do
    {:more, :undefined, 137}
  end

  defp dec_huffman_lookup(133, 1) do
    {:more, :undefined, 138}
  end

  defp dec_huffman_lookup(133, 2) do
    {:more, :undefined, 140}
  end

  defp dec_huffman_lookup(133, 3) do
    {:more, :undefined, 141}
  end

  defp dec_huffman_lookup(133, 4) do
    {:more, :undefined, 144}
  end

  defp dec_huffman_lookup(133, 5) do
    {:more, :undefined, 145}
  end

  defp dec_huffman_lookup(133, 6) do
    {:more, :undefined, 147}
  end

  defp dec_huffman_lookup(133, 7) do
    {:more, :undefined, 150}
  end

  defp dec_huffman_lookup(133, 8) do
    {:more, :undefined, 156}
  end

  defp dec_huffman_lookup(133, 9) do
    {:more, :undefined, 159}
  end

  defp dec_huffman_lookup(133, 10) do
    {:more, :undefined, 163}
  end

  defp dec_huffman_lookup(133, 11) do
    {:more, :undefined, 166}
  end

  defp dec_huffman_lookup(133, 12) do
    {:more, :undefined, 171}
  end

  defp dec_huffman_lookup(133, 13) do
    {:more, :undefined, 174}
  end

  defp dec_huffman_lookup(133, 14) do
    {:more, :undefined, 181}
  end

  defp dec_huffman_lookup(133, 15) do
    {:ok, :undefined, 190}
  end

  defp dec_huffman_lookup(134, 0) do
    {:ok, 178, 0}
  end

  defp dec_huffman_lookup(134, 1) do
    {:ok, 181, 0}
  end

  defp dec_huffman_lookup(134, 2) do
    {:ok, 185, 0}
  end

  defp dec_huffman_lookup(134, 3) do
    {:ok, 186, 0}
  end

  defp dec_huffman_lookup(134, 4) do
    {:ok, 187, 0}
  end

  defp dec_huffman_lookup(134, 5) do
    {:ok, 189, 0}
  end

  defp dec_huffman_lookup(134, 6) do
    {:ok, 190, 0}
  end

  defp dec_huffman_lookup(134, 7) do
    {:ok, 196, 0}
  end

  defp dec_huffman_lookup(134, 8) do
    {:ok, 198, 0}
  end

  defp dec_huffman_lookup(134, 9) do
    {:ok, 228, 0}
  end

  defp dec_huffman_lookup(134, 10) do
    {:ok, 232, 0}
  end

  defp dec_huffman_lookup(134, 11) do
    {:ok, 233, 0}
  end

  defp dec_huffman_lookup(134, 12) do
    {:more, :undefined, 148}
  end

  defp dec_huffman_lookup(134, 13) do
    {:more, :undefined, 149}
  end

  defp dec_huffman_lookup(134, 14) do
    {:more, :undefined, 151}
  end

  defp dec_huffman_lookup(134, 15) do
    {:more, :undefined, 152}
  end

  defp dec_huffman_lookup(135, 0) do
    {:more, 178, 1}
  end

  defp dec_huffman_lookup(135, 1) do
    {:ok, 178, 22}
  end

  defp dec_huffman_lookup(135, 2) do
    {:more, 181, 1}
  end

  defp dec_huffman_lookup(135, 3) do
    {:ok, 181, 22}
  end

  defp dec_huffman_lookup(135, 4) do
    {:more, 185, 1}
  end

  defp dec_huffman_lookup(135, 5) do
    {:ok, 185, 22}
  end

  defp dec_huffman_lookup(135, 6) do
    {:more, 186, 1}
  end

  defp dec_huffman_lookup(135, 7) do
    {:ok, 186, 22}
  end

  defp dec_huffman_lookup(135, 8) do
    {:more, 187, 1}
  end

  defp dec_huffman_lookup(135, 9) do
    {:ok, 187, 22}
  end

  defp dec_huffman_lookup(135, 10) do
    {:more, 189, 1}
  end

  defp dec_huffman_lookup(135, 11) do
    {:ok, 189, 22}
  end

  defp dec_huffman_lookup(135, 12) do
    {:more, 190, 1}
  end

  defp dec_huffman_lookup(135, 13) do
    {:ok, 190, 22}
  end

  defp dec_huffman_lookup(135, 14) do
    {:more, 196, 1}
  end

  defp dec_huffman_lookup(135, 15) do
    {:ok, 196, 22}
  end

  defp dec_huffman_lookup(136, 0) do
    {:more, 178, 2}
  end

  defp dec_huffman_lookup(136, 1) do
    {:more, 178, 9}
  end

  defp dec_huffman_lookup(136, 2) do
    {:more, 178, 23}
  end

  defp dec_huffman_lookup(136, 3) do
    {:ok, 178, 40}
  end

  defp dec_huffman_lookup(136, 4) do
    {:more, 181, 2}
  end

  defp dec_huffman_lookup(136, 5) do
    {:more, 181, 9}
  end

  defp dec_huffman_lookup(136, 6) do
    {:more, 181, 23}
  end

  defp dec_huffman_lookup(136, 7) do
    {:ok, 181, 40}
  end

  defp dec_huffman_lookup(136, 8) do
    {:more, 185, 2}
  end

  defp dec_huffman_lookup(136, 9) do
    {:more, 185, 9}
  end

  defp dec_huffman_lookup(136, 10) do
    {:more, 185, 23}
  end

  defp dec_huffman_lookup(136, 11) do
    {:ok, 185, 40}
  end

  defp dec_huffman_lookup(136, 12) do
    {:more, 186, 2}
  end

  defp dec_huffman_lookup(136, 13) do
    {:more, 186, 9}
  end

  defp dec_huffman_lookup(136, 14) do
    {:more, 186, 23}
  end

  defp dec_huffman_lookup(136, 15) do
    {:ok, 186, 40}
  end

  defp dec_huffman_lookup(137, 0) do
    {:more, 178, 3}
  end

  defp dec_huffman_lookup(137, 1) do
    {:more, 178, 6}
  end

  defp dec_huffman_lookup(137, 2) do
    {:more, 178, 10}
  end

  defp dec_huffman_lookup(137, 3) do
    {:more, 178, 15}
  end

  defp dec_huffman_lookup(137, 4) do
    {:more, 178, 24}
  end

  defp dec_huffman_lookup(137, 5) do
    {:more, 178, 31}
  end

  defp dec_huffman_lookup(137, 6) do
    {:more, 178, 41}
  end

  defp dec_huffman_lookup(137, 7) do
    {:ok, 178, 56}
  end

  defp dec_huffman_lookup(137, 8) do
    {:more, 181, 3}
  end

  defp dec_huffman_lookup(137, 9) do
    {:more, 181, 6}
  end

  defp dec_huffman_lookup(137, 10) do
    {:more, 181, 10}
  end

  defp dec_huffman_lookup(137, 11) do
    {:more, 181, 15}
  end

  defp dec_huffman_lookup(137, 12) do
    {:more, 181, 24}
  end

  defp dec_huffman_lookup(137, 13) do
    {:more, 181, 31}
  end

  defp dec_huffman_lookup(137, 14) do
    {:more, 181, 41}
  end

  defp dec_huffman_lookup(137, 15) do
    {:ok, 181, 56}
  end

  defp dec_huffman_lookup(138, 0) do
    {:more, 185, 3}
  end

  defp dec_huffman_lookup(138, 1) do
    {:more, 185, 6}
  end

  defp dec_huffman_lookup(138, 2) do
    {:more, 185, 10}
  end

  defp dec_huffman_lookup(138, 3) do
    {:more, 185, 15}
  end

  defp dec_huffman_lookup(138, 4) do
    {:more, 185, 24}
  end

  defp dec_huffman_lookup(138, 5) do
    {:more, 185, 31}
  end

  defp dec_huffman_lookup(138, 6) do
    {:more, 185, 41}
  end

  defp dec_huffman_lookup(138, 7) do
    {:ok, 185, 56}
  end

  defp dec_huffman_lookup(138, 8) do
    {:more, 186, 3}
  end

  defp dec_huffman_lookup(138, 9) do
    {:more, 186, 6}
  end

  defp dec_huffman_lookup(138, 10) do
    {:more, 186, 10}
  end

  defp dec_huffman_lookup(138, 11) do
    {:more, 186, 15}
  end

  defp dec_huffman_lookup(138, 12) do
    {:more, 186, 24}
  end

  defp dec_huffman_lookup(138, 13) do
    {:more, 186, 31}
  end

  defp dec_huffman_lookup(138, 14) do
    {:more, 186, 41}
  end

  defp dec_huffman_lookup(138, 15) do
    {:ok, 186, 56}
  end

  defp dec_huffman_lookup(139, 0) do
    {:more, 187, 2}
  end

  defp dec_huffman_lookup(139, 1) do
    {:more, 187, 9}
  end

  defp dec_huffman_lookup(139, 2) do
    {:more, 187, 23}
  end

  defp dec_huffman_lookup(139, 3) do
    {:ok, 187, 40}
  end

  defp dec_huffman_lookup(139, 4) do
    {:more, 189, 2}
  end

  defp dec_huffman_lookup(139, 5) do
    {:more, 189, 9}
  end

  defp dec_huffman_lookup(139, 6) do
    {:more, 189, 23}
  end

  defp dec_huffman_lookup(139, 7) do
    {:ok, 189, 40}
  end

  defp dec_huffman_lookup(139, 8) do
    {:more, 190, 2}
  end

  defp dec_huffman_lookup(139, 9) do
    {:more, 190, 9}
  end

  defp dec_huffman_lookup(139, 10) do
    {:more, 190, 23}
  end

  defp dec_huffman_lookup(139, 11) do
    {:ok, 190, 40}
  end

  defp dec_huffman_lookup(139, 12) do
    {:more, 196, 2}
  end

  defp dec_huffman_lookup(139, 13) do
    {:more, 196, 9}
  end

  defp dec_huffman_lookup(139, 14) do
    {:more, 196, 23}
  end

  defp dec_huffman_lookup(139, 15) do
    {:ok, 196, 40}
  end

  defp dec_huffman_lookup(140, 0) do
    {:more, 187, 3}
  end

  defp dec_huffman_lookup(140, 1) do
    {:more, 187, 6}
  end

  defp dec_huffman_lookup(140, 2) do
    {:more, 187, 10}
  end

  defp dec_huffman_lookup(140, 3) do
    {:more, 187, 15}
  end

  defp dec_huffman_lookup(140, 4) do
    {:more, 187, 24}
  end

  defp dec_huffman_lookup(140, 5) do
    {:more, 187, 31}
  end

  defp dec_huffman_lookup(140, 6) do
    {:more, 187, 41}
  end

  defp dec_huffman_lookup(140, 7) do
    {:ok, 187, 56}
  end

  defp dec_huffman_lookup(140, 8) do
    {:more, 189, 3}
  end

  defp dec_huffman_lookup(140, 9) do
    {:more, 189, 6}
  end

  defp dec_huffman_lookup(140, 10) do
    {:more, 189, 10}
  end

  defp dec_huffman_lookup(140, 11) do
    {:more, 189, 15}
  end

  defp dec_huffman_lookup(140, 12) do
    {:more, 189, 24}
  end

  defp dec_huffman_lookup(140, 13) do
    {:more, 189, 31}
  end

  defp dec_huffman_lookup(140, 14) do
    {:more, 189, 41}
  end

  defp dec_huffman_lookup(140, 15) do
    {:ok, 189, 56}
  end

  defp dec_huffman_lookup(141, 0) do
    {:more, 190, 3}
  end

  defp dec_huffman_lookup(141, 1) do
    {:more, 190, 6}
  end

  defp dec_huffman_lookup(141, 2) do
    {:more, 190, 10}
  end

  defp dec_huffman_lookup(141, 3) do
    {:more, 190, 15}
  end

  defp dec_huffman_lookup(141, 4) do
    {:more, 190, 24}
  end

  defp dec_huffman_lookup(141, 5) do
    {:more, 190, 31}
  end

  defp dec_huffman_lookup(141, 6) do
    {:more, 190, 41}
  end

  defp dec_huffman_lookup(141, 7) do
    {:ok, 190, 56}
  end

  defp dec_huffman_lookup(141, 8) do
    {:more, 196, 3}
  end

  defp dec_huffman_lookup(141, 9) do
    {:more, 196, 6}
  end

  defp dec_huffman_lookup(141, 10) do
    {:more, 196, 10}
  end

  defp dec_huffman_lookup(141, 11) do
    {:more, 196, 15}
  end

  defp dec_huffman_lookup(141, 12) do
    {:more, 196, 24}
  end

  defp dec_huffman_lookup(141, 13) do
    {:more, 196, 31}
  end

  defp dec_huffman_lookup(141, 14) do
    {:more, 196, 41}
  end

  defp dec_huffman_lookup(141, 15) do
    {:ok, 196, 56}
  end

  defp dec_huffman_lookup(142, 0) do
    {:more, 198, 1}
  end

  defp dec_huffman_lookup(142, 1) do
    {:ok, 198, 22}
  end

  defp dec_huffman_lookup(142, 2) do
    {:more, 228, 1}
  end

  defp dec_huffman_lookup(142, 3) do
    {:ok, 228, 22}
  end

  defp dec_huffman_lookup(142, 4) do
    {:more, 232, 1}
  end

  defp dec_huffman_lookup(142, 5) do
    {:ok, 232, 22}
  end

  defp dec_huffman_lookup(142, 6) do
    {:more, 233, 1}
  end

  defp dec_huffman_lookup(142, 7) do
    {:ok, 233, 22}
  end

  defp dec_huffman_lookup(142, 8) do
    {:ok, 1, 0}
  end

  defp dec_huffman_lookup(142, 9) do
    {:ok, 135, 0}
  end

  defp dec_huffman_lookup(142, 10) do
    {:ok, 137, 0}
  end

  defp dec_huffman_lookup(142, 11) do
    {:ok, 138, 0}
  end

  defp dec_huffman_lookup(142, 12) do
    {:ok, 139, 0}
  end

  defp dec_huffman_lookup(142, 13) do
    {:ok, 140, 0}
  end

  defp dec_huffman_lookup(142, 14) do
    {:ok, 141, 0}
  end

  defp dec_huffman_lookup(142, 15) do
    {:ok, 143, 0}
  end

  defp dec_huffman_lookup(143, 0) do
    {:more, 198, 2}
  end

  defp dec_huffman_lookup(143, 1) do
    {:more, 198, 9}
  end

  defp dec_huffman_lookup(143, 2) do
    {:more, 198, 23}
  end

  defp dec_huffman_lookup(143, 3) do
    {:ok, 198, 40}
  end

  defp dec_huffman_lookup(143, 4) do
    {:more, 228, 2}
  end

  defp dec_huffman_lookup(143, 5) do
    {:more, 228, 9}
  end

  defp dec_huffman_lookup(143, 6) do
    {:more, 228, 23}
  end

  defp dec_huffman_lookup(143, 7) do
    {:ok, 228, 40}
  end

  defp dec_huffman_lookup(143, 8) do
    {:more, 232, 2}
  end

  defp dec_huffman_lookup(143, 9) do
    {:more, 232, 9}
  end

  defp dec_huffman_lookup(143, 10) do
    {:more, 232, 23}
  end

  defp dec_huffman_lookup(143, 11) do
    {:ok, 232, 40}
  end

  defp dec_huffman_lookup(143, 12) do
    {:more, 233, 2}
  end

  defp dec_huffman_lookup(143, 13) do
    {:more, 233, 9}
  end

  defp dec_huffman_lookup(143, 14) do
    {:more, 233, 23}
  end

  defp dec_huffman_lookup(143, 15) do
    {:ok, 233, 40}
  end

  defp dec_huffman_lookup(144, 0) do
    {:more, 198, 3}
  end

  defp dec_huffman_lookup(144, 1) do
    {:more, 198, 6}
  end

  defp dec_huffman_lookup(144, 2) do
    {:more, 198, 10}
  end

  defp dec_huffman_lookup(144, 3) do
    {:more, 198, 15}
  end

  defp dec_huffman_lookup(144, 4) do
    {:more, 198, 24}
  end

  defp dec_huffman_lookup(144, 5) do
    {:more, 198, 31}
  end

  defp dec_huffman_lookup(144, 6) do
    {:more, 198, 41}
  end

  defp dec_huffman_lookup(144, 7) do
    {:ok, 198, 56}
  end

  defp dec_huffman_lookup(144, 8) do
    {:more, 228, 3}
  end

  defp dec_huffman_lookup(144, 9) do
    {:more, 228, 6}
  end

  defp dec_huffman_lookup(144, 10) do
    {:more, 228, 10}
  end

  defp dec_huffman_lookup(144, 11) do
    {:more, 228, 15}
  end

  defp dec_huffman_lookup(144, 12) do
    {:more, 228, 24}
  end

  defp dec_huffman_lookup(144, 13) do
    {:more, 228, 31}
  end

  defp dec_huffman_lookup(144, 14) do
    {:more, 228, 41}
  end

  defp dec_huffman_lookup(144, 15) do
    {:ok, 228, 56}
  end

  defp dec_huffman_lookup(145, 0) do
    {:more, 232, 3}
  end

  defp dec_huffman_lookup(145, 1) do
    {:more, 232, 6}
  end

  defp dec_huffman_lookup(145, 2) do
    {:more, 232, 10}
  end

  defp dec_huffman_lookup(145, 3) do
    {:more, 232, 15}
  end

  defp dec_huffman_lookup(145, 4) do
    {:more, 232, 24}
  end

  defp dec_huffman_lookup(145, 5) do
    {:more, 232, 31}
  end

  defp dec_huffman_lookup(145, 6) do
    {:more, 232, 41}
  end

  defp dec_huffman_lookup(145, 7) do
    {:ok, 232, 56}
  end

  defp dec_huffman_lookup(145, 8) do
    {:more, 233, 3}
  end

  defp dec_huffman_lookup(145, 9) do
    {:more, 233, 6}
  end

  defp dec_huffman_lookup(145, 10) do
    {:more, 233, 10}
  end

  defp dec_huffman_lookup(145, 11) do
    {:more, 233, 15}
  end

  defp dec_huffman_lookup(145, 12) do
    {:more, 233, 24}
  end

  defp dec_huffman_lookup(145, 13) do
    {:more, 233, 31}
  end

  defp dec_huffman_lookup(145, 14) do
    {:more, 233, 41}
  end

  defp dec_huffman_lookup(145, 15) do
    {:ok, 233, 56}
  end

  defp dec_huffman_lookup(146, 0) do
    {:more, 1, 1}
  end

  defp dec_huffman_lookup(146, 1) do
    {:ok, 1, 22}
  end

  defp dec_huffman_lookup(146, 2) do
    {:more, 135, 1}
  end

  defp dec_huffman_lookup(146, 3) do
    {:ok, 135, 22}
  end

  defp dec_huffman_lookup(146, 4) do
    {:more, 137, 1}
  end

  defp dec_huffman_lookup(146, 5) do
    {:ok, 137, 22}
  end

  defp dec_huffman_lookup(146, 6) do
    {:more, 138, 1}
  end

  defp dec_huffman_lookup(146, 7) do
    {:ok, 138, 22}
  end

  defp dec_huffman_lookup(146, 8) do
    {:more, 139, 1}
  end

  defp dec_huffman_lookup(146, 9) do
    {:ok, 139, 22}
  end

  defp dec_huffman_lookup(146, 10) do
    {:more, 140, 1}
  end

  defp dec_huffman_lookup(146, 11) do
    {:ok, 140, 22}
  end

  defp dec_huffman_lookup(146, 12) do
    {:more, 141, 1}
  end

  defp dec_huffman_lookup(146, 13) do
    {:ok, 141, 22}
  end

  defp dec_huffman_lookup(146, 14) do
    {:more, 143, 1}
  end

  defp dec_huffman_lookup(146, 15) do
    {:ok, 143, 22}
  end

  defp dec_huffman_lookup(147, 0) do
    {:more, 1, 2}
  end

  defp dec_huffman_lookup(147, 1) do
    {:more, 1, 9}
  end

  defp dec_huffman_lookup(147, 2) do
    {:more, 1, 23}
  end

  defp dec_huffman_lookup(147, 3) do
    {:ok, 1, 40}
  end

  defp dec_huffman_lookup(147, 4) do
    {:more, 135, 2}
  end

  defp dec_huffman_lookup(147, 5) do
    {:more, 135, 9}
  end

  defp dec_huffman_lookup(147, 6) do
    {:more, 135, 23}
  end

  defp dec_huffman_lookup(147, 7) do
    {:ok, 135, 40}
  end

  defp dec_huffman_lookup(147, 8) do
    {:more, 137, 2}
  end

  defp dec_huffman_lookup(147, 9) do
    {:more, 137, 9}
  end

  defp dec_huffman_lookup(147, 10) do
    {:more, 137, 23}
  end

  defp dec_huffman_lookup(147, 11) do
    {:ok, 137, 40}
  end

  defp dec_huffman_lookup(147, 12) do
    {:more, 138, 2}
  end

  defp dec_huffman_lookup(147, 13) do
    {:more, 138, 9}
  end

  defp dec_huffman_lookup(147, 14) do
    {:more, 138, 23}
  end

  defp dec_huffman_lookup(147, 15) do
    {:ok, 138, 40}
  end

  defp dec_huffman_lookup(148, 0) do
    {:more, 1, 3}
  end

  defp dec_huffman_lookup(148, 1) do
    {:more, 1, 6}
  end

  defp dec_huffman_lookup(148, 2) do
    {:more, 1, 10}
  end

  defp dec_huffman_lookup(148, 3) do
    {:more, 1, 15}
  end

  defp dec_huffman_lookup(148, 4) do
    {:more, 1, 24}
  end

  defp dec_huffman_lookup(148, 5) do
    {:more, 1, 31}
  end

  defp dec_huffman_lookup(148, 6) do
    {:more, 1, 41}
  end

  defp dec_huffman_lookup(148, 7) do
    {:ok, 1, 56}
  end

  defp dec_huffman_lookup(148, 8) do
    {:more, 135, 3}
  end

  defp dec_huffman_lookup(148, 9) do
    {:more, 135, 6}
  end

  defp dec_huffman_lookup(148, 10) do
    {:more, 135, 10}
  end

  defp dec_huffman_lookup(148, 11) do
    {:more, 135, 15}
  end

  defp dec_huffman_lookup(148, 12) do
    {:more, 135, 24}
  end

  defp dec_huffman_lookup(148, 13) do
    {:more, 135, 31}
  end

  defp dec_huffman_lookup(148, 14) do
    {:more, 135, 41}
  end

  defp dec_huffman_lookup(148, 15) do
    {:ok, 135, 56}
  end

  defp dec_huffman_lookup(149, 0) do
    {:more, 137, 3}
  end

  defp dec_huffman_lookup(149, 1) do
    {:more, 137, 6}
  end

  defp dec_huffman_lookup(149, 2) do
    {:more, 137, 10}
  end

  defp dec_huffman_lookup(149, 3) do
    {:more, 137, 15}
  end

  defp dec_huffman_lookup(149, 4) do
    {:more, 137, 24}
  end

  defp dec_huffman_lookup(149, 5) do
    {:more, 137, 31}
  end

  defp dec_huffman_lookup(149, 6) do
    {:more, 137, 41}
  end

  defp dec_huffman_lookup(149, 7) do
    {:ok, 137, 56}
  end

  defp dec_huffman_lookup(149, 8) do
    {:more, 138, 3}
  end

  defp dec_huffman_lookup(149, 9) do
    {:more, 138, 6}
  end

  defp dec_huffman_lookup(149, 10) do
    {:more, 138, 10}
  end

  defp dec_huffman_lookup(149, 11) do
    {:more, 138, 15}
  end

  defp dec_huffman_lookup(149, 12) do
    {:more, 138, 24}
  end

  defp dec_huffman_lookup(149, 13) do
    {:more, 138, 31}
  end

  defp dec_huffman_lookup(149, 14) do
    {:more, 138, 41}
  end

  defp dec_huffman_lookup(149, 15) do
    {:ok, 138, 56}
  end

  defp dec_huffman_lookup(150, 0) do
    {:more, 139, 2}
  end

  defp dec_huffman_lookup(150, 1) do
    {:more, 139, 9}
  end

  defp dec_huffman_lookup(150, 2) do
    {:more, 139, 23}
  end

  defp dec_huffman_lookup(150, 3) do
    {:ok, 139, 40}
  end

  defp dec_huffman_lookup(150, 4) do
    {:more, 140, 2}
  end

  defp dec_huffman_lookup(150, 5) do
    {:more, 140, 9}
  end

  defp dec_huffman_lookup(150, 6) do
    {:more, 140, 23}
  end

  defp dec_huffman_lookup(150, 7) do
    {:ok, 140, 40}
  end

  defp dec_huffman_lookup(150, 8) do
    {:more, 141, 2}
  end

  defp dec_huffman_lookup(150, 9) do
    {:more, 141, 9}
  end

  defp dec_huffman_lookup(150, 10) do
    {:more, 141, 23}
  end

  defp dec_huffman_lookup(150, 11) do
    {:ok, 141, 40}
  end

  defp dec_huffman_lookup(150, 12) do
    {:more, 143, 2}
  end

  defp dec_huffman_lookup(150, 13) do
    {:more, 143, 9}
  end

  defp dec_huffman_lookup(150, 14) do
    {:more, 143, 23}
  end

  defp dec_huffman_lookup(150, 15) do
    {:ok, 143, 40}
  end

  defp dec_huffman_lookup(151, 0) do
    {:more, 139, 3}
  end

  defp dec_huffman_lookup(151, 1) do
    {:more, 139, 6}
  end

  defp dec_huffman_lookup(151, 2) do
    {:more, 139, 10}
  end

  defp dec_huffman_lookup(151, 3) do
    {:more, 139, 15}
  end

  defp dec_huffman_lookup(151, 4) do
    {:more, 139, 24}
  end

  defp dec_huffman_lookup(151, 5) do
    {:more, 139, 31}
  end

  defp dec_huffman_lookup(151, 6) do
    {:more, 139, 41}
  end

  defp dec_huffman_lookup(151, 7) do
    {:ok, 139, 56}
  end

  defp dec_huffman_lookup(151, 8) do
    {:more, 140, 3}
  end

  defp dec_huffman_lookup(151, 9) do
    {:more, 140, 6}
  end

  defp dec_huffman_lookup(151, 10) do
    {:more, 140, 10}
  end

  defp dec_huffman_lookup(151, 11) do
    {:more, 140, 15}
  end

  defp dec_huffman_lookup(151, 12) do
    {:more, 140, 24}
  end

  defp dec_huffman_lookup(151, 13) do
    {:more, 140, 31}
  end

  defp dec_huffman_lookup(151, 14) do
    {:more, 140, 41}
  end

  defp dec_huffman_lookup(151, 15) do
    {:ok, 140, 56}
  end

  defp dec_huffman_lookup(152, 0) do
    {:more, 141, 3}
  end

  defp dec_huffman_lookup(152, 1) do
    {:more, 141, 6}
  end

  defp dec_huffman_lookup(152, 2) do
    {:more, 141, 10}
  end

  defp dec_huffman_lookup(152, 3) do
    {:more, 141, 15}
  end

  defp dec_huffman_lookup(152, 4) do
    {:more, 141, 24}
  end

  defp dec_huffman_lookup(152, 5) do
    {:more, 141, 31}
  end

  defp dec_huffman_lookup(152, 6) do
    {:more, 141, 41}
  end

  defp dec_huffman_lookup(152, 7) do
    {:ok, 141, 56}
  end

  defp dec_huffman_lookup(152, 8) do
    {:more, 143, 3}
  end

  defp dec_huffman_lookup(152, 9) do
    {:more, 143, 6}
  end

  defp dec_huffman_lookup(152, 10) do
    {:more, 143, 10}
  end

  defp dec_huffman_lookup(152, 11) do
    {:more, 143, 15}
  end

  defp dec_huffman_lookup(152, 12) do
    {:more, 143, 24}
  end

  defp dec_huffman_lookup(152, 13) do
    {:more, 143, 31}
  end

  defp dec_huffman_lookup(152, 14) do
    {:more, 143, 41}
  end

  defp dec_huffman_lookup(152, 15) do
    {:ok, 143, 56}
  end

  defp dec_huffman_lookup(153, 0) do
    {:more, :undefined, 157}
  end

  defp dec_huffman_lookup(153, 1) do
    {:more, :undefined, 158}
  end

  defp dec_huffman_lookup(153, 2) do
    {:more, :undefined, 160}
  end

  defp dec_huffman_lookup(153, 3) do
    {:more, :undefined, 161}
  end

  defp dec_huffman_lookup(153, 4) do
    {:more, :undefined, 164}
  end

  defp dec_huffman_lookup(153, 5) do
    {:more, :undefined, 165}
  end

  defp dec_huffman_lookup(153, 6) do
    {:more, :undefined, 167}
  end

  defp dec_huffman_lookup(153, 7) do
    {:more, :undefined, 168}
  end

  defp dec_huffman_lookup(153, 8) do
    {:more, :undefined, 172}
  end

  defp dec_huffman_lookup(153, 9) do
    {:more, :undefined, 173}
  end

  defp dec_huffman_lookup(153, 10) do
    {:more, :undefined, 175}
  end

  defp dec_huffman_lookup(153, 11) do
    {:more, :undefined, 177}
  end

  defp dec_huffman_lookup(153, 12) do
    {:more, :undefined, 182}
  end

  defp dec_huffman_lookup(153, 13) do
    {:more, :undefined, 185}
  end

  defp dec_huffman_lookup(153, 14) do
    {:more, :undefined, 191}
  end

  defp dec_huffman_lookup(153, 15) do
    {:ok, :undefined, 207}
  end

  defp dec_huffman_lookup(154, 0) do
    {:ok, 147, 0}
  end

  defp dec_huffman_lookup(154, 1) do
    {:ok, 149, 0}
  end

  defp dec_huffman_lookup(154, 2) do
    {:ok, 150, 0}
  end

  defp dec_huffman_lookup(154, 3) do
    {:ok, 151, 0}
  end

  defp dec_huffman_lookup(154, 4) do
    {:ok, 152, 0}
  end

  defp dec_huffman_lookup(154, 5) do
    {:ok, 155, 0}
  end

  defp dec_huffman_lookup(154, 6) do
    {:ok, 157, 0}
  end

  defp dec_huffman_lookup(154, 7) do
    {:ok, 158, 0}
  end

  defp dec_huffman_lookup(154, 8) do
    {:ok, 165, 0}
  end

  defp dec_huffman_lookup(154, 9) do
    {:ok, 166, 0}
  end

  defp dec_huffman_lookup(154, 10) do
    {:ok, 168, 0}
  end

  defp dec_huffman_lookup(154, 11) do
    {:ok, 174, 0}
  end

  defp dec_huffman_lookup(154, 12) do
    {:ok, 175, 0}
  end

  defp dec_huffman_lookup(154, 13) do
    {:ok, 180, 0}
  end

  defp dec_huffman_lookup(154, 14) do
    {:ok, 182, 0}
  end

  defp dec_huffman_lookup(154, 15) do
    {:ok, 183, 0}
  end

  defp dec_huffman_lookup(155, 0) do
    {:more, 147, 1}
  end

  defp dec_huffman_lookup(155, 1) do
    {:ok, 147, 22}
  end

  defp dec_huffman_lookup(155, 2) do
    {:more, 149, 1}
  end

  defp dec_huffman_lookup(155, 3) do
    {:ok, 149, 22}
  end

  defp dec_huffman_lookup(155, 4) do
    {:more, 150, 1}
  end

  defp dec_huffman_lookup(155, 5) do
    {:ok, 150, 22}
  end

  defp dec_huffman_lookup(155, 6) do
    {:more, 151, 1}
  end

  defp dec_huffman_lookup(155, 7) do
    {:ok, 151, 22}
  end

  defp dec_huffman_lookup(155, 8) do
    {:more, 152, 1}
  end

  defp dec_huffman_lookup(155, 9) do
    {:ok, 152, 22}
  end

  defp dec_huffman_lookup(155, 10) do
    {:more, 155, 1}
  end

  defp dec_huffman_lookup(155, 11) do
    {:ok, 155, 22}
  end

  defp dec_huffman_lookup(155, 12) do
    {:more, 157, 1}
  end

  defp dec_huffman_lookup(155, 13) do
    {:ok, 157, 22}
  end

  defp dec_huffman_lookup(155, 14) do
    {:more, 158, 1}
  end

  defp dec_huffman_lookup(155, 15) do
    {:ok, 158, 22}
  end

  defp dec_huffman_lookup(156, 0) do
    {:more, 147, 2}
  end

  defp dec_huffman_lookup(156, 1) do
    {:more, 147, 9}
  end

  defp dec_huffman_lookup(156, 2) do
    {:more, 147, 23}
  end

  defp dec_huffman_lookup(156, 3) do
    {:ok, 147, 40}
  end

  defp dec_huffman_lookup(156, 4) do
    {:more, 149, 2}
  end

  defp dec_huffman_lookup(156, 5) do
    {:more, 149, 9}
  end

  defp dec_huffman_lookup(156, 6) do
    {:more, 149, 23}
  end

  defp dec_huffman_lookup(156, 7) do
    {:ok, 149, 40}
  end

  defp dec_huffman_lookup(156, 8) do
    {:more, 150, 2}
  end

  defp dec_huffman_lookup(156, 9) do
    {:more, 150, 9}
  end

  defp dec_huffman_lookup(156, 10) do
    {:more, 150, 23}
  end

  defp dec_huffman_lookup(156, 11) do
    {:ok, 150, 40}
  end

  defp dec_huffman_lookup(156, 12) do
    {:more, 151, 2}
  end

  defp dec_huffman_lookup(156, 13) do
    {:more, 151, 9}
  end

  defp dec_huffman_lookup(156, 14) do
    {:more, 151, 23}
  end

  defp dec_huffman_lookup(156, 15) do
    {:ok, 151, 40}
  end

  defp dec_huffman_lookup(157, 0) do
    {:more, 147, 3}
  end

  defp dec_huffman_lookup(157, 1) do
    {:more, 147, 6}
  end

  defp dec_huffman_lookup(157, 2) do
    {:more, 147, 10}
  end

  defp dec_huffman_lookup(157, 3) do
    {:more, 147, 15}
  end

  defp dec_huffman_lookup(157, 4) do
    {:more, 147, 24}
  end

  defp dec_huffman_lookup(157, 5) do
    {:more, 147, 31}
  end

  defp dec_huffman_lookup(157, 6) do
    {:more, 147, 41}
  end

  defp dec_huffman_lookup(157, 7) do
    {:ok, 147, 56}
  end

  defp dec_huffman_lookup(157, 8) do
    {:more, 149, 3}
  end

  defp dec_huffman_lookup(157, 9) do
    {:more, 149, 6}
  end

  defp dec_huffman_lookup(157, 10) do
    {:more, 149, 10}
  end

  defp dec_huffman_lookup(157, 11) do
    {:more, 149, 15}
  end

  defp dec_huffman_lookup(157, 12) do
    {:more, 149, 24}
  end

  defp dec_huffman_lookup(157, 13) do
    {:more, 149, 31}
  end

  defp dec_huffman_lookup(157, 14) do
    {:more, 149, 41}
  end

  defp dec_huffman_lookup(157, 15) do
    {:ok, 149, 56}
  end

  defp dec_huffman_lookup(158, 0) do
    {:more, 150, 3}
  end

  defp dec_huffman_lookup(158, 1) do
    {:more, 150, 6}
  end

  defp dec_huffman_lookup(158, 2) do
    {:more, 150, 10}
  end

  defp dec_huffman_lookup(158, 3) do
    {:more, 150, 15}
  end

  defp dec_huffman_lookup(158, 4) do
    {:more, 150, 24}
  end

  defp dec_huffman_lookup(158, 5) do
    {:more, 150, 31}
  end

  defp dec_huffman_lookup(158, 6) do
    {:more, 150, 41}
  end

  defp dec_huffman_lookup(158, 7) do
    {:ok, 150, 56}
  end

  defp dec_huffman_lookup(158, 8) do
    {:more, 151, 3}
  end

  defp dec_huffman_lookup(158, 9) do
    {:more, 151, 6}
  end

  defp dec_huffman_lookup(158, 10) do
    {:more, 151, 10}
  end

  defp dec_huffman_lookup(158, 11) do
    {:more, 151, 15}
  end

  defp dec_huffman_lookup(158, 12) do
    {:more, 151, 24}
  end

  defp dec_huffman_lookup(158, 13) do
    {:more, 151, 31}
  end

  defp dec_huffman_lookup(158, 14) do
    {:more, 151, 41}
  end

  defp dec_huffman_lookup(158, 15) do
    {:ok, 151, 56}
  end

  defp dec_huffman_lookup(159, 0) do
    {:more, 152, 2}
  end

  defp dec_huffman_lookup(159, 1) do
    {:more, 152, 9}
  end

  defp dec_huffman_lookup(159, 2) do
    {:more, 152, 23}
  end

  defp dec_huffman_lookup(159, 3) do
    {:ok, 152, 40}
  end

  defp dec_huffman_lookup(159, 4) do
    {:more, 155, 2}
  end

  defp dec_huffman_lookup(159, 5) do
    {:more, 155, 9}
  end

  defp dec_huffman_lookup(159, 6) do
    {:more, 155, 23}
  end

  defp dec_huffman_lookup(159, 7) do
    {:ok, 155, 40}
  end

  defp dec_huffman_lookup(159, 8) do
    {:more, 157, 2}
  end

  defp dec_huffman_lookup(159, 9) do
    {:more, 157, 9}
  end

  defp dec_huffman_lookup(159, 10) do
    {:more, 157, 23}
  end

  defp dec_huffman_lookup(159, 11) do
    {:ok, 157, 40}
  end

  defp dec_huffman_lookup(159, 12) do
    {:more, 158, 2}
  end

  defp dec_huffman_lookup(159, 13) do
    {:more, 158, 9}
  end

  defp dec_huffman_lookup(159, 14) do
    {:more, 158, 23}
  end

  defp dec_huffman_lookup(159, 15) do
    {:ok, 158, 40}
  end

  defp dec_huffman_lookup(160, 0) do
    {:more, 152, 3}
  end

  defp dec_huffman_lookup(160, 1) do
    {:more, 152, 6}
  end

  defp dec_huffman_lookup(160, 2) do
    {:more, 152, 10}
  end

  defp dec_huffman_lookup(160, 3) do
    {:more, 152, 15}
  end

  defp dec_huffman_lookup(160, 4) do
    {:more, 152, 24}
  end

  defp dec_huffman_lookup(160, 5) do
    {:more, 152, 31}
  end

  defp dec_huffman_lookup(160, 6) do
    {:more, 152, 41}
  end

  defp dec_huffman_lookup(160, 7) do
    {:ok, 152, 56}
  end

  defp dec_huffman_lookup(160, 8) do
    {:more, 155, 3}
  end

  defp dec_huffman_lookup(160, 9) do
    {:more, 155, 6}
  end

  defp dec_huffman_lookup(160, 10) do
    {:more, 155, 10}
  end

  defp dec_huffman_lookup(160, 11) do
    {:more, 155, 15}
  end

  defp dec_huffman_lookup(160, 12) do
    {:more, 155, 24}
  end

  defp dec_huffman_lookup(160, 13) do
    {:more, 155, 31}
  end

  defp dec_huffman_lookup(160, 14) do
    {:more, 155, 41}
  end

  defp dec_huffman_lookup(160, 15) do
    {:ok, 155, 56}
  end

  defp dec_huffman_lookup(161, 0) do
    {:more, 157, 3}
  end

  defp dec_huffman_lookup(161, 1) do
    {:more, 157, 6}
  end

  defp dec_huffman_lookup(161, 2) do
    {:more, 157, 10}
  end

  defp dec_huffman_lookup(161, 3) do
    {:more, 157, 15}
  end

  defp dec_huffman_lookup(161, 4) do
    {:more, 157, 24}
  end

  defp dec_huffman_lookup(161, 5) do
    {:more, 157, 31}
  end

  defp dec_huffman_lookup(161, 6) do
    {:more, 157, 41}
  end

  defp dec_huffman_lookup(161, 7) do
    {:ok, 157, 56}
  end

  defp dec_huffman_lookup(161, 8) do
    {:more, 158, 3}
  end

  defp dec_huffman_lookup(161, 9) do
    {:more, 158, 6}
  end

  defp dec_huffman_lookup(161, 10) do
    {:more, 158, 10}
  end

  defp dec_huffman_lookup(161, 11) do
    {:more, 158, 15}
  end

  defp dec_huffman_lookup(161, 12) do
    {:more, 158, 24}
  end

  defp dec_huffman_lookup(161, 13) do
    {:more, 158, 31}
  end

  defp dec_huffman_lookup(161, 14) do
    {:more, 158, 41}
  end

  defp dec_huffman_lookup(161, 15) do
    {:ok, 158, 56}
  end

  defp dec_huffman_lookup(162, 0) do
    {:more, 165, 1}
  end

  defp dec_huffman_lookup(162, 1) do
    {:ok, 165, 22}
  end

  defp dec_huffman_lookup(162, 2) do
    {:more, 166, 1}
  end

  defp dec_huffman_lookup(162, 3) do
    {:ok, 166, 22}
  end

  defp dec_huffman_lookup(162, 4) do
    {:more, 168, 1}
  end

  defp dec_huffman_lookup(162, 5) do
    {:ok, 168, 22}
  end

  defp dec_huffman_lookup(162, 6) do
    {:more, 174, 1}
  end

  defp dec_huffman_lookup(162, 7) do
    {:ok, 174, 22}
  end

  defp dec_huffman_lookup(162, 8) do
    {:more, 175, 1}
  end

  defp dec_huffman_lookup(162, 9) do
    {:ok, 175, 22}
  end

  defp dec_huffman_lookup(162, 10) do
    {:more, 180, 1}
  end

  defp dec_huffman_lookup(162, 11) do
    {:ok, 180, 22}
  end

  defp dec_huffman_lookup(162, 12) do
    {:more, 182, 1}
  end

  defp dec_huffman_lookup(162, 13) do
    {:ok, 182, 22}
  end

  defp dec_huffman_lookup(162, 14) do
    {:more, 183, 1}
  end

  defp dec_huffman_lookup(162, 15) do
    {:ok, 183, 22}
  end

  defp dec_huffman_lookup(163, 0) do
    {:more, 165, 2}
  end

  defp dec_huffman_lookup(163, 1) do
    {:more, 165, 9}
  end

  defp dec_huffman_lookup(163, 2) do
    {:more, 165, 23}
  end

  defp dec_huffman_lookup(163, 3) do
    {:ok, 165, 40}
  end

  defp dec_huffman_lookup(163, 4) do
    {:more, 166, 2}
  end

  defp dec_huffman_lookup(163, 5) do
    {:more, 166, 9}
  end

  defp dec_huffman_lookup(163, 6) do
    {:more, 166, 23}
  end

  defp dec_huffman_lookup(163, 7) do
    {:ok, 166, 40}
  end

  defp dec_huffman_lookup(163, 8) do
    {:more, 168, 2}
  end

  defp dec_huffman_lookup(163, 9) do
    {:more, 168, 9}
  end

  defp dec_huffman_lookup(163, 10) do
    {:more, 168, 23}
  end

  defp dec_huffman_lookup(163, 11) do
    {:ok, 168, 40}
  end

  defp dec_huffman_lookup(163, 12) do
    {:more, 174, 2}
  end

  defp dec_huffman_lookup(163, 13) do
    {:more, 174, 9}
  end

  defp dec_huffman_lookup(163, 14) do
    {:more, 174, 23}
  end

  defp dec_huffman_lookup(163, 15) do
    {:ok, 174, 40}
  end

  defp dec_huffman_lookup(164, 0) do
    {:more, 165, 3}
  end

  defp dec_huffman_lookup(164, 1) do
    {:more, 165, 6}
  end

  defp dec_huffman_lookup(164, 2) do
    {:more, 165, 10}
  end

  defp dec_huffman_lookup(164, 3) do
    {:more, 165, 15}
  end

  defp dec_huffman_lookup(164, 4) do
    {:more, 165, 24}
  end

  defp dec_huffman_lookup(164, 5) do
    {:more, 165, 31}
  end

  defp dec_huffman_lookup(164, 6) do
    {:more, 165, 41}
  end

  defp dec_huffman_lookup(164, 7) do
    {:ok, 165, 56}
  end

  defp dec_huffman_lookup(164, 8) do
    {:more, 166, 3}
  end

  defp dec_huffman_lookup(164, 9) do
    {:more, 166, 6}
  end

  defp dec_huffman_lookup(164, 10) do
    {:more, 166, 10}
  end

  defp dec_huffman_lookup(164, 11) do
    {:more, 166, 15}
  end

  defp dec_huffman_lookup(164, 12) do
    {:more, 166, 24}
  end

  defp dec_huffman_lookup(164, 13) do
    {:more, 166, 31}
  end

  defp dec_huffman_lookup(164, 14) do
    {:more, 166, 41}
  end

  defp dec_huffman_lookup(164, 15) do
    {:ok, 166, 56}
  end

  defp dec_huffman_lookup(165, 0) do
    {:more, 168, 3}
  end

  defp dec_huffman_lookup(165, 1) do
    {:more, 168, 6}
  end

  defp dec_huffman_lookup(165, 2) do
    {:more, 168, 10}
  end

  defp dec_huffman_lookup(165, 3) do
    {:more, 168, 15}
  end

  defp dec_huffman_lookup(165, 4) do
    {:more, 168, 24}
  end

  defp dec_huffman_lookup(165, 5) do
    {:more, 168, 31}
  end

  defp dec_huffman_lookup(165, 6) do
    {:more, 168, 41}
  end

  defp dec_huffman_lookup(165, 7) do
    {:ok, 168, 56}
  end

  defp dec_huffman_lookup(165, 8) do
    {:more, 174, 3}
  end

  defp dec_huffman_lookup(165, 9) do
    {:more, 174, 6}
  end

  defp dec_huffman_lookup(165, 10) do
    {:more, 174, 10}
  end

  defp dec_huffman_lookup(165, 11) do
    {:more, 174, 15}
  end

  defp dec_huffman_lookup(165, 12) do
    {:more, 174, 24}
  end

  defp dec_huffman_lookup(165, 13) do
    {:more, 174, 31}
  end

  defp dec_huffman_lookup(165, 14) do
    {:more, 174, 41}
  end

  defp dec_huffman_lookup(165, 15) do
    {:ok, 174, 56}
  end

  defp dec_huffman_lookup(166, 0) do
    {:more, 175, 2}
  end

  defp dec_huffman_lookup(166, 1) do
    {:more, 175, 9}
  end

  defp dec_huffman_lookup(166, 2) do
    {:more, 175, 23}
  end

  defp dec_huffman_lookup(166, 3) do
    {:ok, 175, 40}
  end

  defp dec_huffman_lookup(166, 4) do
    {:more, 180, 2}
  end

  defp dec_huffman_lookup(166, 5) do
    {:more, 180, 9}
  end

  defp dec_huffman_lookup(166, 6) do
    {:more, 180, 23}
  end

  defp dec_huffman_lookup(166, 7) do
    {:ok, 180, 40}
  end

  defp dec_huffman_lookup(166, 8) do
    {:more, 182, 2}
  end

  defp dec_huffman_lookup(166, 9) do
    {:more, 182, 9}
  end

  defp dec_huffman_lookup(166, 10) do
    {:more, 182, 23}
  end

  defp dec_huffman_lookup(166, 11) do
    {:ok, 182, 40}
  end

  defp dec_huffman_lookup(166, 12) do
    {:more, 183, 2}
  end

  defp dec_huffman_lookup(166, 13) do
    {:more, 183, 9}
  end

  defp dec_huffman_lookup(166, 14) do
    {:more, 183, 23}
  end

  defp dec_huffman_lookup(166, 15) do
    {:ok, 183, 40}
  end

  defp dec_huffman_lookup(167, 0) do
    {:more, 175, 3}
  end

  defp dec_huffman_lookup(167, 1) do
    {:more, 175, 6}
  end

  defp dec_huffman_lookup(167, 2) do
    {:more, 175, 10}
  end

  defp dec_huffman_lookup(167, 3) do
    {:more, 175, 15}
  end

  defp dec_huffman_lookup(167, 4) do
    {:more, 175, 24}
  end

  defp dec_huffman_lookup(167, 5) do
    {:more, 175, 31}
  end

  defp dec_huffman_lookup(167, 6) do
    {:more, 175, 41}
  end

  defp dec_huffman_lookup(167, 7) do
    {:ok, 175, 56}
  end

  defp dec_huffman_lookup(167, 8) do
    {:more, 180, 3}
  end

  defp dec_huffman_lookup(167, 9) do
    {:more, 180, 6}
  end

  defp dec_huffman_lookup(167, 10) do
    {:more, 180, 10}
  end

  defp dec_huffman_lookup(167, 11) do
    {:more, 180, 15}
  end

  defp dec_huffman_lookup(167, 12) do
    {:more, 180, 24}
  end

  defp dec_huffman_lookup(167, 13) do
    {:more, 180, 31}
  end

  defp dec_huffman_lookup(167, 14) do
    {:more, 180, 41}
  end

  defp dec_huffman_lookup(167, 15) do
    {:ok, 180, 56}
  end

  defp dec_huffman_lookup(168, 0) do
    {:more, 182, 3}
  end

  defp dec_huffman_lookup(168, 1) do
    {:more, 182, 6}
  end

  defp dec_huffman_lookup(168, 2) do
    {:more, 182, 10}
  end

  defp dec_huffman_lookup(168, 3) do
    {:more, 182, 15}
  end

  defp dec_huffman_lookup(168, 4) do
    {:more, 182, 24}
  end

  defp dec_huffman_lookup(168, 5) do
    {:more, 182, 31}
  end

  defp dec_huffman_lookup(168, 6) do
    {:more, 182, 41}
  end

  defp dec_huffman_lookup(168, 7) do
    {:ok, 182, 56}
  end

  defp dec_huffman_lookup(168, 8) do
    {:more, 183, 3}
  end

  defp dec_huffman_lookup(168, 9) do
    {:more, 183, 6}
  end

  defp dec_huffman_lookup(168, 10) do
    {:more, 183, 10}
  end

  defp dec_huffman_lookup(168, 11) do
    {:more, 183, 15}
  end

  defp dec_huffman_lookup(168, 12) do
    {:more, 183, 24}
  end

  defp dec_huffman_lookup(168, 13) do
    {:more, 183, 31}
  end

  defp dec_huffman_lookup(168, 14) do
    {:more, 183, 41}
  end

  defp dec_huffman_lookup(168, 15) do
    {:ok, 183, 56}
  end

  defp dec_huffman_lookup(169, 0) do
    {:ok, 188, 0}
  end

  defp dec_huffman_lookup(169, 1) do
    {:ok, 191, 0}
  end

  defp dec_huffman_lookup(169, 2) do
    {:ok, 197, 0}
  end

  defp dec_huffman_lookup(169, 3) do
    {:ok, 231, 0}
  end

  defp dec_huffman_lookup(169, 4) do
    {:ok, 239, 0}
  end

  defp dec_huffman_lookup(169, 5) do
    {:more, :undefined, 176}
  end

  defp dec_huffman_lookup(169, 6) do
    {:more, :undefined, 178}
  end

  defp dec_huffman_lookup(169, 7) do
    {:more, :undefined, 179}
  end

  defp dec_huffman_lookup(169, 8) do
    {:more, :undefined, 183}
  end

  defp dec_huffman_lookup(169, 9) do
    {:more, :undefined, 184}
  end

  defp dec_huffman_lookup(169, 10) do
    {:more, :undefined, 186}
  end

  defp dec_huffman_lookup(169, 11) do
    {:more, :undefined, 187}
  end

  defp dec_huffman_lookup(169, 12) do
    {:more, :undefined, 192}
  end

  defp dec_huffman_lookup(169, 13) do
    {:more, :undefined, 199}
  end

  defp dec_huffman_lookup(169, 14) do
    {:more, :undefined, 208}
  end

  defp dec_huffman_lookup(169, 15) do
    {:ok, :undefined, 223}
  end

  defp dec_huffman_lookup(170, 0) do
    {:more, 188, 1}
  end

  defp dec_huffman_lookup(170, 1) do
    {:ok, 188, 22}
  end

  defp dec_huffman_lookup(170, 2) do
    {:more, 191, 1}
  end

  defp dec_huffman_lookup(170, 3) do
    {:ok, 191, 22}
  end

  defp dec_huffman_lookup(170, 4) do
    {:more, 197, 1}
  end

  defp dec_huffman_lookup(170, 5) do
    {:ok, 197, 22}
  end

  defp dec_huffman_lookup(170, 6) do
    {:more, 231, 1}
  end

  defp dec_huffman_lookup(170, 7) do
    {:ok, 231, 22}
  end

  defp dec_huffman_lookup(170, 8) do
    {:more, 239, 1}
  end

  defp dec_huffman_lookup(170, 9) do
    {:ok, 239, 22}
  end

  defp dec_huffman_lookup(170, 10) do
    {:ok, 9, 0}
  end

  defp dec_huffman_lookup(170, 11) do
    {:ok, 142, 0}
  end

  defp dec_huffman_lookup(170, 12) do
    {:ok, 144, 0}
  end

  defp dec_huffman_lookup(170, 13) do
    {:ok, 145, 0}
  end

  defp dec_huffman_lookup(170, 14) do
    {:ok, 148, 0}
  end

  defp dec_huffman_lookup(170, 15) do
    {:ok, 159, 0}
  end

  defp dec_huffman_lookup(171, 0) do
    {:more, 188, 2}
  end

  defp dec_huffman_lookup(171, 1) do
    {:more, 188, 9}
  end

  defp dec_huffman_lookup(171, 2) do
    {:more, 188, 23}
  end

  defp dec_huffman_lookup(171, 3) do
    {:ok, 188, 40}
  end

  defp dec_huffman_lookup(171, 4) do
    {:more, 191, 2}
  end

  defp dec_huffman_lookup(171, 5) do
    {:more, 191, 9}
  end

  defp dec_huffman_lookup(171, 6) do
    {:more, 191, 23}
  end

  defp dec_huffman_lookup(171, 7) do
    {:ok, 191, 40}
  end

  defp dec_huffman_lookup(171, 8) do
    {:more, 197, 2}
  end

  defp dec_huffman_lookup(171, 9) do
    {:more, 197, 9}
  end

  defp dec_huffman_lookup(171, 10) do
    {:more, 197, 23}
  end

  defp dec_huffman_lookup(171, 11) do
    {:ok, 197, 40}
  end

  defp dec_huffman_lookup(171, 12) do
    {:more, 231, 2}
  end

  defp dec_huffman_lookup(171, 13) do
    {:more, 231, 9}
  end

  defp dec_huffman_lookup(171, 14) do
    {:more, 231, 23}
  end

  defp dec_huffman_lookup(171, 15) do
    {:ok, 231, 40}
  end

  defp dec_huffman_lookup(172, 0) do
    {:more, 188, 3}
  end

  defp dec_huffman_lookup(172, 1) do
    {:more, 188, 6}
  end

  defp dec_huffman_lookup(172, 2) do
    {:more, 188, 10}
  end

  defp dec_huffman_lookup(172, 3) do
    {:more, 188, 15}
  end

  defp dec_huffman_lookup(172, 4) do
    {:more, 188, 24}
  end

  defp dec_huffman_lookup(172, 5) do
    {:more, 188, 31}
  end

  defp dec_huffman_lookup(172, 6) do
    {:more, 188, 41}
  end

  defp dec_huffman_lookup(172, 7) do
    {:ok, 188, 56}
  end

  defp dec_huffman_lookup(172, 8) do
    {:more, 191, 3}
  end

  defp dec_huffman_lookup(172, 9) do
    {:more, 191, 6}
  end

  defp dec_huffman_lookup(172, 10) do
    {:more, 191, 10}
  end

  defp dec_huffman_lookup(172, 11) do
    {:more, 191, 15}
  end

  defp dec_huffman_lookup(172, 12) do
    {:more, 191, 24}
  end

  defp dec_huffman_lookup(172, 13) do
    {:more, 191, 31}
  end

  defp dec_huffman_lookup(172, 14) do
    {:more, 191, 41}
  end

  defp dec_huffman_lookup(172, 15) do
    {:ok, 191, 56}
  end

  defp dec_huffman_lookup(173, 0) do
    {:more, 197, 3}
  end

  defp dec_huffman_lookup(173, 1) do
    {:more, 197, 6}
  end

  defp dec_huffman_lookup(173, 2) do
    {:more, 197, 10}
  end

  defp dec_huffman_lookup(173, 3) do
    {:more, 197, 15}
  end

  defp dec_huffman_lookup(173, 4) do
    {:more, 197, 24}
  end

  defp dec_huffman_lookup(173, 5) do
    {:more, 197, 31}
  end

  defp dec_huffman_lookup(173, 6) do
    {:more, 197, 41}
  end

  defp dec_huffman_lookup(173, 7) do
    {:ok, 197, 56}
  end

  defp dec_huffman_lookup(173, 8) do
    {:more, 231, 3}
  end

  defp dec_huffman_lookup(173, 9) do
    {:more, 231, 6}
  end

  defp dec_huffman_lookup(173, 10) do
    {:more, 231, 10}
  end

  defp dec_huffman_lookup(173, 11) do
    {:more, 231, 15}
  end

  defp dec_huffman_lookup(173, 12) do
    {:more, 231, 24}
  end

  defp dec_huffman_lookup(173, 13) do
    {:more, 231, 31}
  end

  defp dec_huffman_lookup(173, 14) do
    {:more, 231, 41}
  end

  defp dec_huffman_lookup(173, 15) do
    {:ok, 231, 56}
  end

  defp dec_huffman_lookup(174, 0) do
    {:more, 239, 2}
  end

  defp dec_huffman_lookup(174, 1) do
    {:more, 239, 9}
  end

  defp dec_huffman_lookup(174, 2) do
    {:more, 239, 23}
  end

  defp dec_huffman_lookup(174, 3) do
    {:ok, 239, 40}
  end

  defp dec_huffman_lookup(174, 4) do
    {:more, 9, 1}
  end

  defp dec_huffman_lookup(174, 5) do
    {:ok, 9, 22}
  end

  defp dec_huffman_lookup(174, 6) do
    {:more, 142, 1}
  end

  defp dec_huffman_lookup(174, 7) do
    {:ok, 142, 22}
  end

  defp dec_huffman_lookup(174, 8) do
    {:more, 144, 1}
  end

  defp dec_huffman_lookup(174, 9) do
    {:ok, 144, 22}
  end

  defp dec_huffman_lookup(174, 10) do
    {:more, 145, 1}
  end

  defp dec_huffman_lookup(174, 11) do
    {:ok, 145, 22}
  end

  defp dec_huffman_lookup(174, 12) do
    {:more, 148, 1}
  end

  defp dec_huffman_lookup(174, 13) do
    {:ok, 148, 22}
  end

  defp dec_huffman_lookup(174, 14) do
    {:more, 159, 1}
  end

  defp dec_huffman_lookup(174, 15) do
    {:ok, 159, 22}
  end

  defp dec_huffman_lookup(175, 0) do
    {:more, 239, 3}
  end

  defp dec_huffman_lookup(175, 1) do
    {:more, 239, 6}
  end

  defp dec_huffman_lookup(175, 2) do
    {:more, 239, 10}
  end

  defp dec_huffman_lookup(175, 3) do
    {:more, 239, 15}
  end

  defp dec_huffman_lookup(175, 4) do
    {:more, 239, 24}
  end

  defp dec_huffman_lookup(175, 5) do
    {:more, 239, 31}
  end

  defp dec_huffman_lookup(175, 6) do
    {:more, 239, 41}
  end

  defp dec_huffman_lookup(175, 7) do
    {:ok, 239, 56}
  end

  defp dec_huffman_lookup(175, 8) do
    {:more, 9, 2}
  end

  defp dec_huffman_lookup(175, 9) do
    {:more, 9, 9}
  end

  defp dec_huffman_lookup(175, 10) do
    {:more, 9, 23}
  end

  defp dec_huffman_lookup(175, 11) do
    {:ok, 9, 40}
  end

  defp dec_huffman_lookup(175, 12) do
    {:more, 142, 2}
  end

  defp dec_huffman_lookup(175, 13) do
    {:more, 142, 9}
  end

  defp dec_huffman_lookup(175, 14) do
    {:more, 142, 23}
  end

  defp dec_huffman_lookup(175, 15) do
    {:ok, 142, 40}
  end

  defp dec_huffman_lookup(176, 0) do
    {:more, 9, 3}
  end

  defp dec_huffman_lookup(176, 1) do
    {:more, 9, 6}
  end

  defp dec_huffman_lookup(176, 2) do
    {:more, 9, 10}
  end

  defp dec_huffman_lookup(176, 3) do
    {:more, 9, 15}
  end

  defp dec_huffman_lookup(176, 4) do
    {:more, 9, 24}
  end

  defp dec_huffman_lookup(176, 5) do
    {:more, 9, 31}
  end

  defp dec_huffman_lookup(176, 6) do
    {:more, 9, 41}
  end

  defp dec_huffman_lookup(176, 7) do
    {:ok, 9, 56}
  end

  defp dec_huffman_lookup(176, 8) do
    {:more, 142, 3}
  end

  defp dec_huffman_lookup(176, 9) do
    {:more, 142, 6}
  end

  defp dec_huffman_lookup(176, 10) do
    {:more, 142, 10}
  end

  defp dec_huffman_lookup(176, 11) do
    {:more, 142, 15}
  end

  defp dec_huffman_lookup(176, 12) do
    {:more, 142, 24}
  end

  defp dec_huffman_lookup(176, 13) do
    {:more, 142, 31}
  end

  defp dec_huffman_lookup(176, 14) do
    {:more, 142, 41}
  end

  defp dec_huffman_lookup(176, 15) do
    {:ok, 142, 56}
  end

  defp dec_huffman_lookup(177, 0) do
    {:more, 144, 2}
  end

  defp dec_huffman_lookup(177, 1) do
    {:more, 144, 9}
  end

  defp dec_huffman_lookup(177, 2) do
    {:more, 144, 23}
  end

  defp dec_huffman_lookup(177, 3) do
    {:ok, 144, 40}
  end

  defp dec_huffman_lookup(177, 4) do
    {:more, 145, 2}
  end

  defp dec_huffman_lookup(177, 5) do
    {:more, 145, 9}
  end

  defp dec_huffman_lookup(177, 6) do
    {:more, 145, 23}
  end

  defp dec_huffman_lookup(177, 7) do
    {:ok, 145, 40}
  end

  defp dec_huffman_lookup(177, 8) do
    {:more, 148, 2}
  end

  defp dec_huffman_lookup(177, 9) do
    {:more, 148, 9}
  end

  defp dec_huffman_lookup(177, 10) do
    {:more, 148, 23}
  end

  defp dec_huffman_lookup(177, 11) do
    {:ok, 148, 40}
  end

  defp dec_huffman_lookup(177, 12) do
    {:more, 159, 2}
  end

  defp dec_huffman_lookup(177, 13) do
    {:more, 159, 9}
  end

  defp dec_huffman_lookup(177, 14) do
    {:more, 159, 23}
  end

  defp dec_huffman_lookup(177, 15) do
    {:ok, 159, 40}
  end

  defp dec_huffman_lookup(178, 0) do
    {:more, 144, 3}
  end

  defp dec_huffman_lookup(178, 1) do
    {:more, 144, 6}
  end

  defp dec_huffman_lookup(178, 2) do
    {:more, 144, 10}
  end

  defp dec_huffman_lookup(178, 3) do
    {:more, 144, 15}
  end

  defp dec_huffman_lookup(178, 4) do
    {:more, 144, 24}
  end

  defp dec_huffman_lookup(178, 5) do
    {:more, 144, 31}
  end

  defp dec_huffman_lookup(178, 6) do
    {:more, 144, 41}
  end

  defp dec_huffman_lookup(178, 7) do
    {:ok, 144, 56}
  end

  defp dec_huffman_lookup(178, 8) do
    {:more, 145, 3}
  end

  defp dec_huffman_lookup(178, 9) do
    {:more, 145, 6}
  end

  defp dec_huffman_lookup(178, 10) do
    {:more, 145, 10}
  end

  defp dec_huffman_lookup(178, 11) do
    {:more, 145, 15}
  end

  defp dec_huffman_lookup(178, 12) do
    {:more, 145, 24}
  end

  defp dec_huffman_lookup(178, 13) do
    {:more, 145, 31}
  end

  defp dec_huffman_lookup(178, 14) do
    {:more, 145, 41}
  end

  defp dec_huffman_lookup(178, 15) do
    {:ok, 145, 56}
  end

  defp dec_huffman_lookup(179, 0) do
    {:more, 148, 3}
  end

  defp dec_huffman_lookup(179, 1) do
    {:more, 148, 6}
  end

  defp dec_huffman_lookup(179, 2) do
    {:more, 148, 10}
  end

  defp dec_huffman_lookup(179, 3) do
    {:more, 148, 15}
  end

  defp dec_huffman_lookup(179, 4) do
    {:more, 148, 24}
  end

  defp dec_huffman_lookup(179, 5) do
    {:more, 148, 31}
  end

  defp dec_huffman_lookup(179, 6) do
    {:more, 148, 41}
  end

  defp dec_huffman_lookup(179, 7) do
    {:ok, 148, 56}
  end

  defp dec_huffman_lookup(179, 8) do
    {:more, 159, 3}
  end

  defp dec_huffman_lookup(179, 9) do
    {:more, 159, 6}
  end

  defp dec_huffman_lookup(179, 10) do
    {:more, 159, 10}
  end

  defp dec_huffman_lookup(179, 11) do
    {:more, 159, 15}
  end

  defp dec_huffman_lookup(179, 12) do
    {:more, 159, 24}
  end

  defp dec_huffman_lookup(179, 13) do
    {:more, 159, 31}
  end

  defp dec_huffman_lookup(179, 14) do
    {:more, 159, 41}
  end

  defp dec_huffman_lookup(179, 15) do
    {:ok, 159, 56}
  end

  defp dec_huffman_lookup(180, 0) do
    {:ok, 171, 0}
  end

  defp dec_huffman_lookup(180, 1) do
    {:ok, 206, 0}
  end

  defp dec_huffman_lookup(180, 2) do
    {:ok, 215, 0}
  end

  defp dec_huffman_lookup(180, 3) do
    {:ok, 225, 0}
  end

  defp dec_huffman_lookup(180, 4) do
    {:ok, 236, 0}
  end

  defp dec_huffman_lookup(180, 5) do
    {:ok, 237, 0}
  end

  defp dec_huffman_lookup(180, 6) do
    {:more, :undefined, 188}
  end

  defp dec_huffman_lookup(180, 7) do
    {:more, :undefined, 189}
  end

  defp dec_huffman_lookup(180, 8) do
    {:more, :undefined, 193}
  end

  defp dec_huffman_lookup(180, 9) do
    {:more, :undefined, 196}
  end

  defp dec_huffman_lookup(180, 10) do
    {:more, :undefined, 200}
  end

  defp dec_huffman_lookup(180, 11) do
    {:more, :undefined, 203}
  end

  defp dec_huffman_lookup(180, 12) do
    {:more, :undefined, 209}
  end

  defp dec_huffman_lookup(180, 13) do
    {:more, :undefined, 216}
  end

  defp dec_huffman_lookup(180, 14) do
    {:more, :undefined, 224}
  end

  defp dec_huffman_lookup(180, 15) do
    {:ok, :undefined, 238}
  end

  defp dec_huffman_lookup(181, 0) do
    {:more, 171, 1}
  end

  defp dec_huffman_lookup(181, 1) do
    {:ok, 171, 22}
  end

  defp dec_huffman_lookup(181, 2) do
    {:more, 206, 1}
  end

  defp dec_huffman_lookup(181, 3) do
    {:ok, 206, 22}
  end

  defp dec_huffman_lookup(181, 4) do
    {:more, 215, 1}
  end

  defp dec_huffman_lookup(181, 5) do
    {:ok, 215, 22}
  end

  defp dec_huffman_lookup(181, 6) do
    {:more, 225, 1}
  end

  defp dec_huffman_lookup(181, 7) do
    {:ok, 225, 22}
  end

  defp dec_huffman_lookup(181, 8) do
    {:more, 236, 1}
  end

  defp dec_huffman_lookup(181, 9) do
    {:ok, 236, 22}
  end

  defp dec_huffman_lookup(181, 10) do
    {:more, 237, 1}
  end

  defp dec_huffman_lookup(181, 11) do
    {:ok, 237, 22}
  end

  defp dec_huffman_lookup(181, 12) do
    {:ok, 199, 0}
  end

  defp dec_huffman_lookup(181, 13) do
    {:ok, 207, 0}
  end

  defp dec_huffman_lookup(181, 14) do
    {:ok, 234, 0}
  end

  defp dec_huffman_lookup(181, 15) do
    {:ok, 235, 0}
  end

  defp dec_huffman_lookup(182, 0) do
    {:more, 171, 2}
  end

  defp dec_huffman_lookup(182, 1) do
    {:more, 171, 9}
  end

  defp dec_huffman_lookup(182, 2) do
    {:more, 171, 23}
  end

  defp dec_huffman_lookup(182, 3) do
    {:ok, 171, 40}
  end

  defp dec_huffman_lookup(182, 4) do
    {:more, 206, 2}
  end

  defp dec_huffman_lookup(182, 5) do
    {:more, 206, 9}
  end

  defp dec_huffman_lookup(182, 6) do
    {:more, 206, 23}
  end

  defp dec_huffman_lookup(182, 7) do
    {:ok, 206, 40}
  end

  defp dec_huffman_lookup(182, 8) do
    {:more, 215, 2}
  end

  defp dec_huffman_lookup(182, 9) do
    {:more, 215, 9}
  end

  defp dec_huffman_lookup(182, 10) do
    {:more, 215, 23}
  end

  defp dec_huffman_lookup(182, 11) do
    {:ok, 215, 40}
  end

  defp dec_huffman_lookup(182, 12) do
    {:more, 225, 2}
  end

  defp dec_huffman_lookup(182, 13) do
    {:more, 225, 9}
  end

  defp dec_huffman_lookup(182, 14) do
    {:more, 225, 23}
  end

  defp dec_huffman_lookup(182, 15) do
    {:ok, 225, 40}
  end

  defp dec_huffman_lookup(183, 0) do
    {:more, 171, 3}
  end

  defp dec_huffman_lookup(183, 1) do
    {:more, 171, 6}
  end

  defp dec_huffman_lookup(183, 2) do
    {:more, 171, 10}
  end

  defp dec_huffman_lookup(183, 3) do
    {:more, 171, 15}
  end

  defp dec_huffman_lookup(183, 4) do
    {:more, 171, 24}
  end

  defp dec_huffman_lookup(183, 5) do
    {:more, 171, 31}
  end

  defp dec_huffman_lookup(183, 6) do
    {:more, 171, 41}
  end

  defp dec_huffman_lookup(183, 7) do
    {:ok, 171, 56}
  end

  defp dec_huffman_lookup(183, 8) do
    {:more, 206, 3}
  end

  defp dec_huffman_lookup(183, 9) do
    {:more, 206, 6}
  end

  defp dec_huffman_lookup(183, 10) do
    {:more, 206, 10}
  end

  defp dec_huffman_lookup(183, 11) do
    {:more, 206, 15}
  end

  defp dec_huffman_lookup(183, 12) do
    {:more, 206, 24}
  end

  defp dec_huffman_lookup(183, 13) do
    {:more, 206, 31}
  end

  defp dec_huffman_lookup(183, 14) do
    {:more, 206, 41}
  end

  defp dec_huffman_lookup(183, 15) do
    {:ok, 206, 56}
  end

  defp dec_huffman_lookup(184, 0) do
    {:more, 215, 3}
  end

  defp dec_huffman_lookup(184, 1) do
    {:more, 215, 6}
  end

  defp dec_huffman_lookup(184, 2) do
    {:more, 215, 10}
  end

  defp dec_huffman_lookup(184, 3) do
    {:more, 215, 15}
  end

  defp dec_huffman_lookup(184, 4) do
    {:more, 215, 24}
  end

  defp dec_huffman_lookup(184, 5) do
    {:more, 215, 31}
  end

  defp dec_huffman_lookup(184, 6) do
    {:more, 215, 41}
  end

  defp dec_huffman_lookup(184, 7) do
    {:ok, 215, 56}
  end

  defp dec_huffman_lookup(184, 8) do
    {:more, 225, 3}
  end

  defp dec_huffman_lookup(184, 9) do
    {:more, 225, 6}
  end

  defp dec_huffman_lookup(184, 10) do
    {:more, 225, 10}
  end

  defp dec_huffman_lookup(184, 11) do
    {:more, 225, 15}
  end

  defp dec_huffman_lookup(184, 12) do
    {:more, 225, 24}
  end

  defp dec_huffman_lookup(184, 13) do
    {:more, 225, 31}
  end

  defp dec_huffman_lookup(184, 14) do
    {:more, 225, 41}
  end

  defp dec_huffman_lookup(184, 15) do
    {:ok, 225, 56}
  end

  defp dec_huffman_lookup(185, 0) do
    {:more, 236, 2}
  end

  defp dec_huffman_lookup(185, 1) do
    {:more, 236, 9}
  end

  defp dec_huffman_lookup(185, 2) do
    {:more, 236, 23}
  end

  defp dec_huffman_lookup(185, 3) do
    {:ok, 236, 40}
  end

  defp dec_huffman_lookup(185, 4) do
    {:more, 237, 2}
  end

  defp dec_huffman_lookup(185, 5) do
    {:more, 237, 9}
  end

  defp dec_huffman_lookup(185, 6) do
    {:more, 237, 23}
  end

  defp dec_huffman_lookup(185, 7) do
    {:ok, 237, 40}
  end

  defp dec_huffman_lookup(185, 8) do
    {:more, 199, 1}
  end

  defp dec_huffman_lookup(185, 9) do
    {:ok, 199, 22}
  end

  defp dec_huffman_lookup(185, 10) do
    {:more, 207, 1}
  end

  defp dec_huffman_lookup(185, 11) do
    {:ok, 207, 22}
  end

  defp dec_huffman_lookup(185, 12) do
    {:more, 234, 1}
  end

  defp dec_huffman_lookup(185, 13) do
    {:ok, 234, 22}
  end

  defp dec_huffman_lookup(185, 14) do
    {:more, 235, 1}
  end

  defp dec_huffman_lookup(185, 15) do
    {:ok, 235, 22}
  end

  defp dec_huffman_lookup(186, 0) do
    {:more, 236, 3}
  end

  defp dec_huffman_lookup(186, 1) do
    {:more, 236, 6}
  end

  defp dec_huffman_lookup(186, 2) do
    {:more, 236, 10}
  end

  defp dec_huffman_lookup(186, 3) do
    {:more, 236, 15}
  end

  defp dec_huffman_lookup(186, 4) do
    {:more, 236, 24}
  end

  defp dec_huffman_lookup(186, 5) do
    {:more, 236, 31}
  end

  defp dec_huffman_lookup(186, 6) do
    {:more, 236, 41}
  end

  defp dec_huffman_lookup(186, 7) do
    {:ok, 236, 56}
  end

  defp dec_huffman_lookup(186, 8) do
    {:more, 237, 3}
  end

  defp dec_huffman_lookup(186, 9) do
    {:more, 237, 6}
  end

  defp dec_huffman_lookup(186, 10) do
    {:more, 237, 10}
  end

  defp dec_huffman_lookup(186, 11) do
    {:more, 237, 15}
  end

  defp dec_huffman_lookup(186, 12) do
    {:more, 237, 24}
  end

  defp dec_huffman_lookup(186, 13) do
    {:more, 237, 31}
  end

  defp dec_huffman_lookup(186, 14) do
    {:more, 237, 41}
  end

  defp dec_huffman_lookup(186, 15) do
    {:ok, 237, 56}
  end

  defp dec_huffman_lookup(187, 0) do
    {:more, 199, 2}
  end

  defp dec_huffman_lookup(187, 1) do
    {:more, 199, 9}
  end

  defp dec_huffman_lookup(187, 2) do
    {:more, 199, 23}
  end

  defp dec_huffman_lookup(187, 3) do
    {:ok, 199, 40}
  end

  defp dec_huffman_lookup(187, 4) do
    {:more, 207, 2}
  end

  defp dec_huffman_lookup(187, 5) do
    {:more, 207, 9}
  end

  defp dec_huffman_lookup(187, 6) do
    {:more, 207, 23}
  end

  defp dec_huffman_lookup(187, 7) do
    {:ok, 207, 40}
  end

  defp dec_huffman_lookup(187, 8) do
    {:more, 234, 2}
  end

  defp dec_huffman_lookup(187, 9) do
    {:more, 234, 9}
  end

  defp dec_huffman_lookup(187, 10) do
    {:more, 234, 23}
  end

  defp dec_huffman_lookup(187, 11) do
    {:ok, 234, 40}
  end

  defp dec_huffman_lookup(187, 12) do
    {:more, 235, 2}
  end

  defp dec_huffman_lookup(187, 13) do
    {:more, 235, 9}
  end

  defp dec_huffman_lookup(187, 14) do
    {:more, 235, 23}
  end

  defp dec_huffman_lookup(187, 15) do
    {:ok, 235, 40}
  end

  defp dec_huffman_lookup(188, 0) do
    {:more, 199, 3}
  end

  defp dec_huffman_lookup(188, 1) do
    {:more, 199, 6}
  end

  defp dec_huffman_lookup(188, 2) do
    {:more, 199, 10}
  end

  defp dec_huffman_lookup(188, 3) do
    {:more, 199, 15}
  end

  defp dec_huffman_lookup(188, 4) do
    {:more, 199, 24}
  end

  defp dec_huffman_lookup(188, 5) do
    {:more, 199, 31}
  end

  defp dec_huffman_lookup(188, 6) do
    {:more, 199, 41}
  end

  defp dec_huffman_lookup(188, 7) do
    {:ok, 199, 56}
  end

  defp dec_huffman_lookup(188, 8) do
    {:more, 207, 3}
  end

  defp dec_huffman_lookup(188, 9) do
    {:more, 207, 6}
  end

  defp dec_huffman_lookup(188, 10) do
    {:more, 207, 10}
  end

  defp dec_huffman_lookup(188, 11) do
    {:more, 207, 15}
  end

  defp dec_huffman_lookup(188, 12) do
    {:more, 207, 24}
  end

  defp dec_huffman_lookup(188, 13) do
    {:more, 207, 31}
  end

  defp dec_huffman_lookup(188, 14) do
    {:more, 207, 41}
  end

  defp dec_huffman_lookup(188, 15) do
    {:ok, 207, 56}
  end

  defp dec_huffman_lookup(189, 0) do
    {:more, 234, 3}
  end

  defp dec_huffman_lookup(189, 1) do
    {:more, 234, 6}
  end

  defp dec_huffman_lookup(189, 2) do
    {:more, 234, 10}
  end

  defp dec_huffman_lookup(189, 3) do
    {:more, 234, 15}
  end

  defp dec_huffman_lookup(189, 4) do
    {:more, 234, 24}
  end

  defp dec_huffman_lookup(189, 5) do
    {:more, 234, 31}
  end

  defp dec_huffman_lookup(189, 6) do
    {:more, 234, 41}
  end

  defp dec_huffman_lookup(189, 7) do
    {:ok, 234, 56}
  end

  defp dec_huffman_lookup(189, 8) do
    {:more, 235, 3}
  end

  defp dec_huffman_lookup(189, 9) do
    {:more, 235, 6}
  end

  defp dec_huffman_lookup(189, 10) do
    {:more, 235, 10}
  end

  defp dec_huffman_lookup(189, 11) do
    {:more, 235, 15}
  end

  defp dec_huffman_lookup(189, 12) do
    {:more, 235, 24}
  end

  defp dec_huffman_lookup(189, 13) do
    {:more, 235, 31}
  end

  defp dec_huffman_lookup(189, 14) do
    {:more, 235, 41}
  end

  defp dec_huffman_lookup(189, 15) do
    {:ok, 235, 56}
  end

  defp dec_huffman_lookup(190, 0) do
    {:more, :undefined, 194}
  end

  defp dec_huffman_lookup(190, 1) do
    {:more, :undefined, 195}
  end

  defp dec_huffman_lookup(190, 2) do
    {:more, :undefined, 197}
  end

  defp dec_huffman_lookup(190, 3) do
    {:more, :undefined, 198}
  end

  defp dec_huffman_lookup(190, 4) do
    {:more, :undefined, 201}
  end

  defp dec_huffman_lookup(190, 5) do
    {:more, :undefined, 202}
  end

  defp dec_huffman_lookup(190, 6) do
    {:more, :undefined, 204}
  end

  defp dec_huffman_lookup(190, 7) do
    {:more, :undefined, 205}
  end

  defp dec_huffman_lookup(190, 8) do
    {:more, :undefined, 210}
  end

  defp dec_huffman_lookup(190, 9) do
    {:more, :undefined, 213}
  end

  defp dec_huffman_lookup(190, 10) do
    {:more, :undefined, 217}
  end

  defp dec_huffman_lookup(190, 11) do
    {:more, :undefined, 220}
  end

  defp dec_huffman_lookup(190, 12) do
    {:more, :undefined, 225}
  end

  defp dec_huffman_lookup(190, 13) do
    {:more, :undefined, 231}
  end

  defp dec_huffman_lookup(190, 14) do
    {:more, :undefined, 239}
  end

  defp dec_huffman_lookup(190, 15) do
    {:ok, :undefined, 246}
  end

  defp dec_huffman_lookup(191, 0) do
    {:ok, 192, 0}
  end

  defp dec_huffman_lookup(191, 1) do
    {:ok, 193, 0}
  end

  defp dec_huffman_lookup(191, 2) do
    {:ok, 200, 0}
  end

  defp dec_huffman_lookup(191, 3) do
    {:ok, 201, 0}
  end

  defp dec_huffman_lookup(191, 4) do
    {:ok, 202, 0}
  end

  defp dec_huffman_lookup(191, 5) do
    {:ok, 205, 0}
  end

  defp dec_huffman_lookup(191, 6) do
    {:ok, 210, 0}
  end

  defp dec_huffman_lookup(191, 7) do
    {:ok, 213, 0}
  end

  defp dec_huffman_lookup(191, 8) do
    {:ok, 218, 0}
  end

  defp dec_huffman_lookup(191, 9) do
    {:ok, 219, 0}
  end

  defp dec_huffman_lookup(191, 10) do
    {:ok, 238, 0}
  end

  defp dec_huffman_lookup(191, 11) do
    {:ok, 240, 0}
  end

  defp dec_huffman_lookup(191, 12) do
    {:ok, 242, 0}
  end

  defp dec_huffman_lookup(191, 13) do
    {:ok, 243, 0}
  end

  defp dec_huffman_lookup(191, 14) do
    {:ok, 255, 0}
  end

  defp dec_huffman_lookup(191, 15) do
    {:more, :undefined, 206}
  end

  defp dec_huffman_lookup(192, 0) do
    {:more, 192, 1}
  end

  defp dec_huffman_lookup(192, 1) do
    {:ok, 192, 22}
  end

  defp dec_huffman_lookup(192, 2) do
    {:more, 193, 1}
  end

  defp dec_huffman_lookup(192, 3) do
    {:ok, 193, 22}
  end

  defp dec_huffman_lookup(192, 4) do
    {:more, 200, 1}
  end

  defp dec_huffman_lookup(192, 5) do
    {:ok, 200, 22}
  end

  defp dec_huffman_lookup(192, 6) do
    {:more, 201, 1}
  end

  defp dec_huffman_lookup(192, 7) do
    {:ok, 201, 22}
  end

  defp dec_huffman_lookup(192, 8) do
    {:more, 202, 1}
  end

  defp dec_huffman_lookup(192, 9) do
    {:ok, 202, 22}
  end

  defp dec_huffman_lookup(192, 10) do
    {:more, 205, 1}
  end

  defp dec_huffman_lookup(192, 11) do
    {:ok, 205, 22}
  end

  defp dec_huffman_lookup(192, 12) do
    {:more, 210, 1}
  end

  defp dec_huffman_lookup(192, 13) do
    {:ok, 210, 22}
  end

  defp dec_huffman_lookup(192, 14) do
    {:more, 213, 1}
  end

  defp dec_huffman_lookup(192, 15) do
    {:ok, 213, 22}
  end

  defp dec_huffman_lookup(193, 0) do
    {:more, 192, 2}
  end

  defp dec_huffman_lookup(193, 1) do
    {:more, 192, 9}
  end

  defp dec_huffman_lookup(193, 2) do
    {:more, 192, 23}
  end

  defp dec_huffman_lookup(193, 3) do
    {:ok, 192, 40}
  end

  defp dec_huffman_lookup(193, 4) do
    {:more, 193, 2}
  end

  defp dec_huffman_lookup(193, 5) do
    {:more, 193, 9}
  end

  defp dec_huffman_lookup(193, 6) do
    {:more, 193, 23}
  end

  defp dec_huffman_lookup(193, 7) do
    {:ok, 193, 40}
  end

  defp dec_huffman_lookup(193, 8) do
    {:more, 200, 2}
  end

  defp dec_huffman_lookup(193, 9) do
    {:more, 200, 9}
  end

  defp dec_huffman_lookup(193, 10) do
    {:more, 200, 23}
  end

  defp dec_huffman_lookup(193, 11) do
    {:ok, 200, 40}
  end

  defp dec_huffman_lookup(193, 12) do
    {:more, 201, 2}
  end

  defp dec_huffman_lookup(193, 13) do
    {:more, 201, 9}
  end

  defp dec_huffman_lookup(193, 14) do
    {:more, 201, 23}
  end

  defp dec_huffman_lookup(193, 15) do
    {:ok, 201, 40}
  end

  defp dec_huffman_lookup(194, 0) do
    {:more, 192, 3}
  end

  defp dec_huffman_lookup(194, 1) do
    {:more, 192, 6}
  end

  defp dec_huffman_lookup(194, 2) do
    {:more, 192, 10}
  end

  defp dec_huffman_lookup(194, 3) do
    {:more, 192, 15}
  end

  defp dec_huffman_lookup(194, 4) do
    {:more, 192, 24}
  end

  defp dec_huffman_lookup(194, 5) do
    {:more, 192, 31}
  end

  defp dec_huffman_lookup(194, 6) do
    {:more, 192, 41}
  end

  defp dec_huffman_lookup(194, 7) do
    {:ok, 192, 56}
  end

  defp dec_huffman_lookup(194, 8) do
    {:more, 193, 3}
  end

  defp dec_huffman_lookup(194, 9) do
    {:more, 193, 6}
  end

  defp dec_huffman_lookup(194, 10) do
    {:more, 193, 10}
  end

  defp dec_huffman_lookup(194, 11) do
    {:more, 193, 15}
  end

  defp dec_huffman_lookup(194, 12) do
    {:more, 193, 24}
  end

  defp dec_huffman_lookup(194, 13) do
    {:more, 193, 31}
  end

  defp dec_huffman_lookup(194, 14) do
    {:more, 193, 41}
  end

  defp dec_huffman_lookup(194, 15) do
    {:ok, 193, 56}
  end

  defp dec_huffman_lookup(195, 0) do
    {:more, 200, 3}
  end

  defp dec_huffman_lookup(195, 1) do
    {:more, 200, 6}
  end

  defp dec_huffman_lookup(195, 2) do
    {:more, 200, 10}
  end

  defp dec_huffman_lookup(195, 3) do
    {:more, 200, 15}
  end

  defp dec_huffman_lookup(195, 4) do
    {:more, 200, 24}
  end

  defp dec_huffman_lookup(195, 5) do
    {:more, 200, 31}
  end

  defp dec_huffman_lookup(195, 6) do
    {:more, 200, 41}
  end

  defp dec_huffman_lookup(195, 7) do
    {:ok, 200, 56}
  end

  defp dec_huffman_lookup(195, 8) do
    {:more, 201, 3}
  end

  defp dec_huffman_lookup(195, 9) do
    {:more, 201, 6}
  end

  defp dec_huffman_lookup(195, 10) do
    {:more, 201, 10}
  end

  defp dec_huffman_lookup(195, 11) do
    {:more, 201, 15}
  end

  defp dec_huffman_lookup(195, 12) do
    {:more, 201, 24}
  end

  defp dec_huffman_lookup(195, 13) do
    {:more, 201, 31}
  end

  defp dec_huffman_lookup(195, 14) do
    {:more, 201, 41}
  end

  defp dec_huffman_lookup(195, 15) do
    {:ok, 201, 56}
  end

  defp dec_huffman_lookup(196, 0) do
    {:more, 202, 2}
  end

  defp dec_huffman_lookup(196, 1) do
    {:more, 202, 9}
  end

  defp dec_huffman_lookup(196, 2) do
    {:more, 202, 23}
  end

  defp dec_huffman_lookup(196, 3) do
    {:ok, 202, 40}
  end

  defp dec_huffman_lookup(196, 4) do
    {:more, 205, 2}
  end

  defp dec_huffman_lookup(196, 5) do
    {:more, 205, 9}
  end

  defp dec_huffman_lookup(196, 6) do
    {:more, 205, 23}
  end

  defp dec_huffman_lookup(196, 7) do
    {:ok, 205, 40}
  end

  defp dec_huffman_lookup(196, 8) do
    {:more, 210, 2}
  end

  defp dec_huffman_lookup(196, 9) do
    {:more, 210, 9}
  end

  defp dec_huffman_lookup(196, 10) do
    {:more, 210, 23}
  end

  defp dec_huffman_lookup(196, 11) do
    {:ok, 210, 40}
  end

  defp dec_huffman_lookup(196, 12) do
    {:more, 213, 2}
  end

  defp dec_huffman_lookup(196, 13) do
    {:more, 213, 9}
  end

  defp dec_huffman_lookup(196, 14) do
    {:more, 213, 23}
  end

  defp dec_huffman_lookup(196, 15) do
    {:ok, 213, 40}
  end

  defp dec_huffman_lookup(197, 0) do
    {:more, 202, 3}
  end

  defp dec_huffman_lookup(197, 1) do
    {:more, 202, 6}
  end

  defp dec_huffman_lookup(197, 2) do
    {:more, 202, 10}
  end

  defp dec_huffman_lookup(197, 3) do
    {:more, 202, 15}
  end

  defp dec_huffman_lookup(197, 4) do
    {:more, 202, 24}
  end

  defp dec_huffman_lookup(197, 5) do
    {:more, 202, 31}
  end

  defp dec_huffman_lookup(197, 6) do
    {:more, 202, 41}
  end

  defp dec_huffman_lookup(197, 7) do
    {:ok, 202, 56}
  end

  defp dec_huffman_lookup(197, 8) do
    {:more, 205, 3}
  end

  defp dec_huffman_lookup(197, 9) do
    {:more, 205, 6}
  end

  defp dec_huffman_lookup(197, 10) do
    {:more, 205, 10}
  end

  defp dec_huffman_lookup(197, 11) do
    {:more, 205, 15}
  end

  defp dec_huffman_lookup(197, 12) do
    {:more, 205, 24}
  end

  defp dec_huffman_lookup(197, 13) do
    {:more, 205, 31}
  end

  defp dec_huffman_lookup(197, 14) do
    {:more, 205, 41}
  end

  defp dec_huffman_lookup(197, 15) do
    {:ok, 205, 56}
  end

  defp dec_huffman_lookup(198, 0) do
    {:more, 210, 3}
  end

  defp dec_huffman_lookup(198, 1) do
    {:more, 210, 6}
  end

  defp dec_huffman_lookup(198, 2) do
    {:more, 210, 10}
  end

  defp dec_huffman_lookup(198, 3) do
    {:more, 210, 15}
  end

  defp dec_huffman_lookup(198, 4) do
    {:more, 210, 24}
  end

  defp dec_huffman_lookup(198, 5) do
    {:more, 210, 31}
  end

  defp dec_huffman_lookup(198, 6) do
    {:more, 210, 41}
  end

  defp dec_huffman_lookup(198, 7) do
    {:ok, 210, 56}
  end

  defp dec_huffman_lookup(198, 8) do
    {:more, 213, 3}
  end

  defp dec_huffman_lookup(198, 9) do
    {:more, 213, 6}
  end

  defp dec_huffman_lookup(198, 10) do
    {:more, 213, 10}
  end

  defp dec_huffman_lookup(198, 11) do
    {:more, 213, 15}
  end

  defp dec_huffman_lookup(198, 12) do
    {:more, 213, 24}
  end

  defp dec_huffman_lookup(198, 13) do
    {:more, 213, 31}
  end

  defp dec_huffman_lookup(198, 14) do
    {:more, 213, 41}
  end

  defp dec_huffman_lookup(198, 15) do
    {:ok, 213, 56}
  end

  defp dec_huffman_lookup(199, 0) do
    {:more, 218, 1}
  end

  defp dec_huffman_lookup(199, 1) do
    {:ok, 218, 22}
  end

  defp dec_huffman_lookup(199, 2) do
    {:more, 219, 1}
  end

  defp dec_huffman_lookup(199, 3) do
    {:ok, 219, 22}
  end

  defp dec_huffman_lookup(199, 4) do
    {:more, 238, 1}
  end

  defp dec_huffman_lookup(199, 5) do
    {:ok, 238, 22}
  end

  defp dec_huffman_lookup(199, 6) do
    {:more, 240, 1}
  end

  defp dec_huffman_lookup(199, 7) do
    {:ok, 240, 22}
  end

  defp dec_huffman_lookup(199, 8) do
    {:more, 242, 1}
  end

  defp dec_huffman_lookup(199, 9) do
    {:ok, 242, 22}
  end

  defp dec_huffman_lookup(199, 10) do
    {:more, 243, 1}
  end

  defp dec_huffman_lookup(199, 11) do
    {:ok, 243, 22}
  end

  defp dec_huffman_lookup(199, 12) do
    {:more, 255, 1}
  end

  defp dec_huffman_lookup(199, 13) do
    {:ok, 255, 22}
  end

  defp dec_huffman_lookup(199, 14) do
    {:ok, 203, 0}
  end

  defp dec_huffman_lookup(199, 15) do
    {:ok, 204, 0}
  end

  defp dec_huffman_lookup(200, 0) do
    {:more, 218, 2}
  end

  defp dec_huffman_lookup(200, 1) do
    {:more, 218, 9}
  end

  defp dec_huffman_lookup(200, 2) do
    {:more, 218, 23}
  end

  defp dec_huffman_lookup(200, 3) do
    {:ok, 218, 40}
  end

  defp dec_huffman_lookup(200, 4) do
    {:more, 219, 2}
  end

  defp dec_huffman_lookup(200, 5) do
    {:more, 219, 9}
  end

  defp dec_huffman_lookup(200, 6) do
    {:more, 219, 23}
  end

  defp dec_huffman_lookup(200, 7) do
    {:ok, 219, 40}
  end

  defp dec_huffman_lookup(200, 8) do
    {:more, 238, 2}
  end

  defp dec_huffman_lookup(200, 9) do
    {:more, 238, 9}
  end

  defp dec_huffman_lookup(200, 10) do
    {:more, 238, 23}
  end

  defp dec_huffman_lookup(200, 11) do
    {:ok, 238, 40}
  end

  defp dec_huffman_lookup(200, 12) do
    {:more, 240, 2}
  end

  defp dec_huffman_lookup(200, 13) do
    {:more, 240, 9}
  end

  defp dec_huffman_lookup(200, 14) do
    {:more, 240, 23}
  end

  defp dec_huffman_lookup(200, 15) do
    {:ok, 240, 40}
  end

  defp dec_huffman_lookup(201, 0) do
    {:more, 218, 3}
  end

  defp dec_huffman_lookup(201, 1) do
    {:more, 218, 6}
  end

  defp dec_huffman_lookup(201, 2) do
    {:more, 218, 10}
  end

  defp dec_huffman_lookup(201, 3) do
    {:more, 218, 15}
  end

  defp dec_huffman_lookup(201, 4) do
    {:more, 218, 24}
  end

  defp dec_huffman_lookup(201, 5) do
    {:more, 218, 31}
  end

  defp dec_huffman_lookup(201, 6) do
    {:more, 218, 41}
  end

  defp dec_huffman_lookup(201, 7) do
    {:ok, 218, 56}
  end

  defp dec_huffman_lookup(201, 8) do
    {:more, 219, 3}
  end

  defp dec_huffman_lookup(201, 9) do
    {:more, 219, 6}
  end

  defp dec_huffman_lookup(201, 10) do
    {:more, 219, 10}
  end

  defp dec_huffman_lookup(201, 11) do
    {:more, 219, 15}
  end

  defp dec_huffman_lookup(201, 12) do
    {:more, 219, 24}
  end

  defp dec_huffman_lookup(201, 13) do
    {:more, 219, 31}
  end

  defp dec_huffman_lookup(201, 14) do
    {:more, 219, 41}
  end

  defp dec_huffman_lookup(201, 15) do
    {:ok, 219, 56}
  end

  defp dec_huffman_lookup(202, 0) do
    {:more, 238, 3}
  end

  defp dec_huffman_lookup(202, 1) do
    {:more, 238, 6}
  end

  defp dec_huffman_lookup(202, 2) do
    {:more, 238, 10}
  end

  defp dec_huffman_lookup(202, 3) do
    {:more, 238, 15}
  end

  defp dec_huffman_lookup(202, 4) do
    {:more, 238, 24}
  end

  defp dec_huffman_lookup(202, 5) do
    {:more, 238, 31}
  end

  defp dec_huffman_lookup(202, 6) do
    {:more, 238, 41}
  end

  defp dec_huffman_lookup(202, 7) do
    {:ok, 238, 56}
  end

  defp dec_huffman_lookup(202, 8) do
    {:more, 240, 3}
  end

  defp dec_huffman_lookup(202, 9) do
    {:more, 240, 6}
  end

  defp dec_huffman_lookup(202, 10) do
    {:more, 240, 10}
  end

  defp dec_huffman_lookup(202, 11) do
    {:more, 240, 15}
  end

  defp dec_huffman_lookup(202, 12) do
    {:more, 240, 24}
  end

  defp dec_huffman_lookup(202, 13) do
    {:more, 240, 31}
  end

  defp dec_huffman_lookup(202, 14) do
    {:more, 240, 41}
  end

  defp dec_huffman_lookup(202, 15) do
    {:ok, 240, 56}
  end

  defp dec_huffman_lookup(203, 0) do
    {:more, 242, 2}
  end

  defp dec_huffman_lookup(203, 1) do
    {:more, 242, 9}
  end

  defp dec_huffman_lookup(203, 2) do
    {:more, 242, 23}
  end

  defp dec_huffman_lookup(203, 3) do
    {:ok, 242, 40}
  end

  defp dec_huffman_lookup(203, 4) do
    {:more, 243, 2}
  end

  defp dec_huffman_lookup(203, 5) do
    {:more, 243, 9}
  end

  defp dec_huffman_lookup(203, 6) do
    {:more, 243, 23}
  end

  defp dec_huffman_lookup(203, 7) do
    {:ok, 243, 40}
  end

  defp dec_huffman_lookup(203, 8) do
    {:more, 255, 2}
  end

  defp dec_huffman_lookup(203, 9) do
    {:more, 255, 9}
  end

  defp dec_huffman_lookup(203, 10) do
    {:more, 255, 23}
  end

  defp dec_huffman_lookup(203, 11) do
    {:ok, 255, 40}
  end

  defp dec_huffman_lookup(203, 12) do
    {:more, 203, 1}
  end

  defp dec_huffman_lookup(203, 13) do
    {:ok, 203, 22}
  end

  defp dec_huffman_lookup(203, 14) do
    {:more, 204, 1}
  end

  defp dec_huffman_lookup(203, 15) do
    {:ok, 204, 22}
  end

  defp dec_huffman_lookup(204, 0) do
    {:more, 242, 3}
  end

  defp dec_huffman_lookup(204, 1) do
    {:more, 242, 6}
  end

  defp dec_huffman_lookup(204, 2) do
    {:more, 242, 10}
  end

  defp dec_huffman_lookup(204, 3) do
    {:more, 242, 15}
  end

  defp dec_huffman_lookup(204, 4) do
    {:more, 242, 24}
  end

  defp dec_huffman_lookup(204, 5) do
    {:more, 242, 31}
  end

  defp dec_huffman_lookup(204, 6) do
    {:more, 242, 41}
  end

  defp dec_huffman_lookup(204, 7) do
    {:ok, 242, 56}
  end

  defp dec_huffman_lookup(204, 8) do
    {:more, 243, 3}
  end

  defp dec_huffman_lookup(204, 9) do
    {:more, 243, 6}
  end

  defp dec_huffman_lookup(204, 10) do
    {:more, 243, 10}
  end

  defp dec_huffman_lookup(204, 11) do
    {:more, 243, 15}
  end

  defp dec_huffman_lookup(204, 12) do
    {:more, 243, 24}
  end

  defp dec_huffman_lookup(204, 13) do
    {:more, 243, 31}
  end

  defp dec_huffman_lookup(204, 14) do
    {:more, 243, 41}
  end

  defp dec_huffman_lookup(204, 15) do
    {:ok, 243, 56}
  end

  defp dec_huffman_lookup(205, 0) do
    {:more, 255, 3}
  end

  defp dec_huffman_lookup(205, 1) do
    {:more, 255, 6}
  end

  defp dec_huffman_lookup(205, 2) do
    {:more, 255, 10}
  end

  defp dec_huffman_lookup(205, 3) do
    {:more, 255, 15}
  end

  defp dec_huffman_lookup(205, 4) do
    {:more, 255, 24}
  end

  defp dec_huffman_lookup(205, 5) do
    {:more, 255, 31}
  end

  defp dec_huffman_lookup(205, 6) do
    {:more, 255, 41}
  end

  defp dec_huffman_lookup(205, 7) do
    {:ok, 255, 56}
  end

  defp dec_huffman_lookup(205, 8) do
    {:more, 203, 2}
  end

  defp dec_huffman_lookup(205, 9) do
    {:more, 203, 9}
  end

  defp dec_huffman_lookup(205, 10) do
    {:more, 203, 23}
  end

  defp dec_huffman_lookup(205, 11) do
    {:ok, 203, 40}
  end

  defp dec_huffman_lookup(205, 12) do
    {:more, 204, 2}
  end

  defp dec_huffman_lookup(205, 13) do
    {:more, 204, 9}
  end

  defp dec_huffman_lookup(205, 14) do
    {:more, 204, 23}
  end

  defp dec_huffman_lookup(205, 15) do
    {:ok, 204, 40}
  end

  defp dec_huffman_lookup(206, 0) do
    {:more, 203, 3}
  end

  defp dec_huffman_lookup(206, 1) do
    {:more, 203, 6}
  end

  defp dec_huffman_lookup(206, 2) do
    {:more, 203, 10}
  end

  defp dec_huffman_lookup(206, 3) do
    {:more, 203, 15}
  end

  defp dec_huffman_lookup(206, 4) do
    {:more, 203, 24}
  end

  defp dec_huffman_lookup(206, 5) do
    {:more, 203, 31}
  end

  defp dec_huffman_lookup(206, 6) do
    {:more, 203, 41}
  end

  defp dec_huffman_lookup(206, 7) do
    {:ok, 203, 56}
  end

  defp dec_huffman_lookup(206, 8) do
    {:more, 204, 3}
  end

  defp dec_huffman_lookup(206, 9) do
    {:more, 204, 6}
  end

  defp dec_huffman_lookup(206, 10) do
    {:more, 204, 10}
  end

  defp dec_huffman_lookup(206, 11) do
    {:more, 204, 15}
  end

  defp dec_huffman_lookup(206, 12) do
    {:more, 204, 24}
  end

  defp dec_huffman_lookup(206, 13) do
    {:more, 204, 31}
  end

  defp dec_huffman_lookup(206, 14) do
    {:more, 204, 41}
  end

  defp dec_huffman_lookup(206, 15) do
    {:ok, 204, 56}
  end

  defp dec_huffman_lookup(207, 0) do
    {:more, :undefined, 211}
  end

  defp dec_huffman_lookup(207, 1) do
    {:more, :undefined, 212}
  end

  defp dec_huffman_lookup(207, 2) do
    {:more, :undefined, 214}
  end

  defp dec_huffman_lookup(207, 3) do
    {:more, :undefined, 215}
  end

  defp dec_huffman_lookup(207, 4) do
    {:more, :undefined, 218}
  end

  defp dec_huffman_lookup(207, 5) do
    {:more, :undefined, 219}
  end

  defp dec_huffman_lookup(207, 6) do
    {:more, :undefined, 221}
  end

  defp dec_huffman_lookup(207, 7) do
    {:more, :undefined, 222}
  end

  defp dec_huffman_lookup(207, 8) do
    {:more, :undefined, 226}
  end

  defp dec_huffman_lookup(207, 9) do
    {:more, :undefined, 228}
  end

  defp dec_huffman_lookup(207, 10) do
    {:more, :undefined, 232}
  end

  defp dec_huffman_lookup(207, 11) do
    {:more, :undefined, 235}
  end

  defp dec_huffman_lookup(207, 12) do
    {:more, :undefined, 240}
  end

  defp dec_huffman_lookup(207, 13) do
    {:more, :undefined, 243}
  end

  defp dec_huffman_lookup(207, 14) do
    {:more, :undefined, 247}
  end

  defp dec_huffman_lookup(207, 15) do
    {:ok, :undefined, 250}
  end

  defp dec_huffman_lookup(208, 0) do
    {:ok, 211, 0}
  end

  defp dec_huffman_lookup(208, 1) do
    {:ok, 212, 0}
  end

  defp dec_huffman_lookup(208, 2) do
    {:ok, 214, 0}
  end

  defp dec_huffman_lookup(208, 3) do
    {:ok, 221, 0}
  end

  defp dec_huffman_lookup(208, 4) do
    {:ok, 222, 0}
  end

  defp dec_huffman_lookup(208, 5) do
    {:ok, 223, 0}
  end

  defp dec_huffman_lookup(208, 6) do
    {:ok, 241, 0}
  end

  defp dec_huffman_lookup(208, 7) do
    {:ok, 244, 0}
  end

  defp dec_huffman_lookup(208, 8) do
    {:ok, 245, 0}
  end

  defp dec_huffman_lookup(208, 9) do
    {:ok, 246, 0}
  end

  defp dec_huffman_lookup(208, 10) do
    {:ok, 247, 0}
  end

  defp dec_huffman_lookup(208, 11) do
    {:ok, 248, 0}
  end

  defp dec_huffman_lookup(208, 12) do
    {:ok, 250, 0}
  end

  defp dec_huffman_lookup(208, 13) do
    {:ok, 251, 0}
  end

  defp dec_huffman_lookup(208, 14) do
    {:ok, 252, 0}
  end

  defp dec_huffman_lookup(208, 15) do
    {:ok, 253, 0}
  end

  defp dec_huffman_lookup(209, 0) do
    {:more, 211, 1}
  end

  defp dec_huffman_lookup(209, 1) do
    {:ok, 211, 22}
  end

  defp dec_huffman_lookup(209, 2) do
    {:more, 212, 1}
  end

  defp dec_huffman_lookup(209, 3) do
    {:ok, 212, 22}
  end

  defp dec_huffman_lookup(209, 4) do
    {:more, 214, 1}
  end

  defp dec_huffman_lookup(209, 5) do
    {:ok, 214, 22}
  end

  defp dec_huffman_lookup(209, 6) do
    {:more, 221, 1}
  end

  defp dec_huffman_lookup(209, 7) do
    {:ok, 221, 22}
  end

  defp dec_huffman_lookup(209, 8) do
    {:more, 222, 1}
  end

  defp dec_huffman_lookup(209, 9) do
    {:ok, 222, 22}
  end

  defp dec_huffman_lookup(209, 10) do
    {:more, 223, 1}
  end

  defp dec_huffman_lookup(209, 11) do
    {:ok, 223, 22}
  end

  defp dec_huffman_lookup(209, 12) do
    {:more, 241, 1}
  end

  defp dec_huffman_lookup(209, 13) do
    {:ok, 241, 22}
  end

  defp dec_huffman_lookup(209, 14) do
    {:more, 244, 1}
  end

  defp dec_huffman_lookup(209, 15) do
    {:ok, 244, 22}
  end

  defp dec_huffman_lookup(210, 0) do
    {:more, 211, 2}
  end

  defp dec_huffman_lookup(210, 1) do
    {:more, 211, 9}
  end

  defp dec_huffman_lookup(210, 2) do
    {:more, 211, 23}
  end

  defp dec_huffman_lookup(210, 3) do
    {:ok, 211, 40}
  end

  defp dec_huffman_lookup(210, 4) do
    {:more, 212, 2}
  end

  defp dec_huffman_lookup(210, 5) do
    {:more, 212, 9}
  end

  defp dec_huffman_lookup(210, 6) do
    {:more, 212, 23}
  end

  defp dec_huffman_lookup(210, 7) do
    {:ok, 212, 40}
  end

  defp dec_huffman_lookup(210, 8) do
    {:more, 214, 2}
  end

  defp dec_huffman_lookup(210, 9) do
    {:more, 214, 9}
  end

  defp dec_huffman_lookup(210, 10) do
    {:more, 214, 23}
  end

  defp dec_huffman_lookup(210, 11) do
    {:ok, 214, 40}
  end

  defp dec_huffman_lookup(210, 12) do
    {:more, 221, 2}
  end

  defp dec_huffman_lookup(210, 13) do
    {:more, 221, 9}
  end

  defp dec_huffman_lookup(210, 14) do
    {:more, 221, 23}
  end

  defp dec_huffman_lookup(210, 15) do
    {:ok, 221, 40}
  end

  defp dec_huffman_lookup(211, 0) do
    {:more, 211, 3}
  end

  defp dec_huffman_lookup(211, 1) do
    {:more, 211, 6}
  end

  defp dec_huffman_lookup(211, 2) do
    {:more, 211, 10}
  end

  defp dec_huffman_lookup(211, 3) do
    {:more, 211, 15}
  end

  defp dec_huffman_lookup(211, 4) do
    {:more, 211, 24}
  end

  defp dec_huffman_lookup(211, 5) do
    {:more, 211, 31}
  end

  defp dec_huffman_lookup(211, 6) do
    {:more, 211, 41}
  end

  defp dec_huffman_lookup(211, 7) do
    {:ok, 211, 56}
  end

  defp dec_huffman_lookup(211, 8) do
    {:more, 212, 3}
  end

  defp dec_huffman_lookup(211, 9) do
    {:more, 212, 6}
  end

  defp dec_huffman_lookup(211, 10) do
    {:more, 212, 10}
  end

  defp dec_huffman_lookup(211, 11) do
    {:more, 212, 15}
  end

  defp dec_huffman_lookup(211, 12) do
    {:more, 212, 24}
  end

  defp dec_huffman_lookup(211, 13) do
    {:more, 212, 31}
  end

  defp dec_huffman_lookup(211, 14) do
    {:more, 212, 41}
  end

  defp dec_huffman_lookup(211, 15) do
    {:ok, 212, 56}
  end

  defp dec_huffman_lookup(212, 0) do
    {:more, 214, 3}
  end

  defp dec_huffman_lookup(212, 1) do
    {:more, 214, 6}
  end

  defp dec_huffman_lookup(212, 2) do
    {:more, 214, 10}
  end

  defp dec_huffman_lookup(212, 3) do
    {:more, 214, 15}
  end

  defp dec_huffman_lookup(212, 4) do
    {:more, 214, 24}
  end

  defp dec_huffman_lookup(212, 5) do
    {:more, 214, 31}
  end

  defp dec_huffman_lookup(212, 6) do
    {:more, 214, 41}
  end

  defp dec_huffman_lookup(212, 7) do
    {:ok, 214, 56}
  end

  defp dec_huffman_lookup(212, 8) do
    {:more, 221, 3}
  end

  defp dec_huffman_lookup(212, 9) do
    {:more, 221, 6}
  end

  defp dec_huffman_lookup(212, 10) do
    {:more, 221, 10}
  end

  defp dec_huffman_lookup(212, 11) do
    {:more, 221, 15}
  end

  defp dec_huffman_lookup(212, 12) do
    {:more, 221, 24}
  end

  defp dec_huffman_lookup(212, 13) do
    {:more, 221, 31}
  end

  defp dec_huffman_lookup(212, 14) do
    {:more, 221, 41}
  end

  defp dec_huffman_lookup(212, 15) do
    {:ok, 221, 56}
  end

  defp dec_huffman_lookup(213, 0) do
    {:more, 222, 2}
  end

  defp dec_huffman_lookup(213, 1) do
    {:more, 222, 9}
  end

  defp dec_huffman_lookup(213, 2) do
    {:more, 222, 23}
  end

  defp dec_huffman_lookup(213, 3) do
    {:ok, 222, 40}
  end

  defp dec_huffman_lookup(213, 4) do
    {:more, 223, 2}
  end

  defp dec_huffman_lookup(213, 5) do
    {:more, 223, 9}
  end

  defp dec_huffman_lookup(213, 6) do
    {:more, 223, 23}
  end

  defp dec_huffman_lookup(213, 7) do
    {:ok, 223, 40}
  end

  defp dec_huffman_lookup(213, 8) do
    {:more, 241, 2}
  end

  defp dec_huffman_lookup(213, 9) do
    {:more, 241, 9}
  end

  defp dec_huffman_lookup(213, 10) do
    {:more, 241, 23}
  end

  defp dec_huffman_lookup(213, 11) do
    {:ok, 241, 40}
  end

  defp dec_huffman_lookup(213, 12) do
    {:more, 244, 2}
  end

  defp dec_huffman_lookup(213, 13) do
    {:more, 244, 9}
  end

  defp dec_huffman_lookup(213, 14) do
    {:more, 244, 23}
  end

  defp dec_huffman_lookup(213, 15) do
    {:ok, 244, 40}
  end

  defp dec_huffman_lookup(214, 0) do
    {:more, 222, 3}
  end

  defp dec_huffman_lookup(214, 1) do
    {:more, 222, 6}
  end

  defp dec_huffman_lookup(214, 2) do
    {:more, 222, 10}
  end

  defp dec_huffman_lookup(214, 3) do
    {:more, 222, 15}
  end

  defp dec_huffman_lookup(214, 4) do
    {:more, 222, 24}
  end

  defp dec_huffman_lookup(214, 5) do
    {:more, 222, 31}
  end

  defp dec_huffman_lookup(214, 6) do
    {:more, 222, 41}
  end

  defp dec_huffman_lookup(214, 7) do
    {:ok, 222, 56}
  end

  defp dec_huffman_lookup(214, 8) do
    {:more, 223, 3}
  end

  defp dec_huffman_lookup(214, 9) do
    {:more, 223, 6}
  end

  defp dec_huffman_lookup(214, 10) do
    {:more, 223, 10}
  end

  defp dec_huffman_lookup(214, 11) do
    {:more, 223, 15}
  end

  defp dec_huffman_lookup(214, 12) do
    {:more, 223, 24}
  end

  defp dec_huffman_lookup(214, 13) do
    {:more, 223, 31}
  end

  defp dec_huffman_lookup(214, 14) do
    {:more, 223, 41}
  end

  defp dec_huffman_lookup(214, 15) do
    {:ok, 223, 56}
  end

  defp dec_huffman_lookup(215, 0) do
    {:more, 241, 3}
  end

  defp dec_huffman_lookup(215, 1) do
    {:more, 241, 6}
  end

  defp dec_huffman_lookup(215, 2) do
    {:more, 241, 10}
  end

  defp dec_huffman_lookup(215, 3) do
    {:more, 241, 15}
  end

  defp dec_huffman_lookup(215, 4) do
    {:more, 241, 24}
  end

  defp dec_huffman_lookup(215, 5) do
    {:more, 241, 31}
  end

  defp dec_huffman_lookup(215, 6) do
    {:more, 241, 41}
  end

  defp dec_huffman_lookup(215, 7) do
    {:ok, 241, 56}
  end

  defp dec_huffman_lookup(215, 8) do
    {:more, 244, 3}
  end

  defp dec_huffman_lookup(215, 9) do
    {:more, 244, 6}
  end

  defp dec_huffman_lookup(215, 10) do
    {:more, 244, 10}
  end

  defp dec_huffman_lookup(215, 11) do
    {:more, 244, 15}
  end

  defp dec_huffman_lookup(215, 12) do
    {:more, 244, 24}
  end

  defp dec_huffman_lookup(215, 13) do
    {:more, 244, 31}
  end

  defp dec_huffman_lookup(215, 14) do
    {:more, 244, 41}
  end

  defp dec_huffman_lookup(215, 15) do
    {:ok, 244, 56}
  end

  defp dec_huffman_lookup(216, 0) do
    {:more, 245, 1}
  end

  defp dec_huffman_lookup(216, 1) do
    {:ok, 245, 22}
  end

  defp dec_huffman_lookup(216, 2) do
    {:more, 246, 1}
  end

  defp dec_huffman_lookup(216, 3) do
    {:ok, 246, 22}
  end

  defp dec_huffman_lookup(216, 4) do
    {:more, 247, 1}
  end

  defp dec_huffman_lookup(216, 5) do
    {:ok, 247, 22}
  end

  defp dec_huffman_lookup(216, 6) do
    {:more, 248, 1}
  end

  defp dec_huffman_lookup(216, 7) do
    {:ok, 248, 22}
  end

  defp dec_huffman_lookup(216, 8) do
    {:more, 250, 1}
  end

  defp dec_huffman_lookup(216, 9) do
    {:ok, 250, 22}
  end

  defp dec_huffman_lookup(216, 10) do
    {:more, 251, 1}
  end

  defp dec_huffman_lookup(216, 11) do
    {:ok, 251, 22}
  end

  defp dec_huffman_lookup(216, 12) do
    {:more, 252, 1}
  end

  defp dec_huffman_lookup(216, 13) do
    {:ok, 252, 22}
  end

  defp dec_huffman_lookup(216, 14) do
    {:more, 253, 1}
  end

  defp dec_huffman_lookup(216, 15) do
    {:ok, 253, 22}
  end

  defp dec_huffman_lookup(217, 0) do
    {:more, 245, 2}
  end

  defp dec_huffman_lookup(217, 1) do
    {:more, 245, 9}
  end

  defp dec_huffman_lookup(217, 2) do
    {:more, 245, 23}
  end

  defp dec_huffman_lookup(217, 3) do
    {:ok, 245, 40}
  end

  defp dec_huffman_lookup(217, 4) do
    {:more, 246, 2}
  end

  defp dec_huffman_lookup(217, 5) do
    {:more, 246, 9}
  end

  defp dec_huffman_lookup(217, 6) do
    {:more, 246, 23}
  end

  defp dec_huffman_lookup(217, 7) do
    {:ok, 246, 40}
  end

  defp dec_huffman_lookup(217, 8) do
    {:more, 247, 2}
  end

  defp dec_huffman_lookup(217, 9) do
    {:more, 247, 9}
  end

  defp dec_huffman_lookup(217, 10) do
    {:more, 247, 23}
  end

  defp dec_huffman_lookup(217, 11) do
    {:ok, 247, 40}
  end

  defp dec_huffman_lookup(217, 12) do
    {:more, 248, 2}
  end

  defp dec_huffman_lookup(217, 13) do
    {:more, 248, 9}
  end

  defp dec_huffman_lookup(217, 14) do
    {:more, 248, 23}
  end

  defp dec_huffman_lookup(217, 15) do
    {:ok, 248, 40}
  end

  defp dec_huffman_lookup(218, 0) do
    {:more, 245, 3}
  end

  defp dec_huffman_lookup(218, 1) do
    {:more, 245, 6}
  end

  defp dec_huffman_lookup(218, 2) do
    {:more, 245, 10}
  end

  defp dec_huffman_lookup(218, 3) do
    {:more, 245, 15}
  end

  defp dec_huffman_lookup(218, 4) do
    {:more, 245, 24}
  end

  defp dec_huffman_lookup(218, 5) do
    {:more, 245, 31}
  end

  defp dec_huffman_lookup(218, 6) do
    {:more, 245, 41}
  end

  defp dec_huffman_lookup(218, 7) do
    {:ok, 245, 56}
  end

  defp dec_huffman_lookup(218, 8) do
    {:more, 246, 3}
  end

  defp dec_huffman_lookup(218, 9) do
    {:more, 246, 6}
  end

  defp dec_huffman_lookup(218, 10) do
    {:more, 246, 10}
  end

  defp dec_huffman_lookup(218, 11) do
    {:more, 246, 15}
  end

  defp dec_huffman_lookup(218, 12) do
    {:more, 246, 24}
  end

  defp dec_huffman_lookup(218, 13) do
    {:more, 246, 31}
  end

  defp dec_huffman_lookup(218, 14) do
    {:more, 246, 41}
  end

  defp dec_huffman_lookup(218, 15) do
    {:ok, 246, 56}
  end

  defp dec_huffman_lookup(219, 0) do
    {:more, 247, 3}
  end

  defp dec_huffman_lookup(219, 1) do
    {:more, 247, 6}
  end

  defp dec_huffman_lookup(219, 2) do
    {:more, 247, 10}
  end

  defp dec_huffman_lookup(219, 3) do
    {:more, 247, 15}
  end

  defp dec_huffman_lookup(219, 4) do
    {:more, 247, 24}
  end

  defp dec_huffman_lookup(219, 5) do
    {:more, 247, 31}
  end

  defp dec_huffman_lookup(219, 6) do
    {:more, 247, 41}
  end

  defp dec_huffman_lookup(219, 7) do
    {:ok, 247, 56}
  end

  defp dec_huffman_lookup(219, 8) do
    {:more, 248, 3}
  end

  defp dec_huffman_lookup(219, 9) do
    {:more, 248, 6}
  end

  defp dec_huffman_lookup(219, 10) do
    {:more, 248, 10}
  end

  defp dec_huffman_lookup(219, 11) do
    {:more, 248, 15}
  end

  defp dec_huffman_lookup(219, 12) do
    {:more, 248, 24}
  end

  defp dec_huffman_lookup(219, 13) do
    {:more, 248, 31}
  end

  defp dec_huffman_lookup(219, 14) do
    {:more, 248, 41}
  end

  defp dec_huffman_lookup(219, 15) do
    {:ok, 248, 56}
  end

  defp dec_huffman_lookup(220, 0) do
    {:more, 250, 2}
  end

  defp dec_huffman_lookup(220, 1) do
    {:more, 250, 9}
  end

  defp dec_huffman_lookup(220, 2) do
    {:more, 250, 23}
  end

  defp dec_huffman_lookup(220, 3) do
    {:ok, 250, 40}
  end

  defp dec_huffman_lookup(220, 4) do
    {:more, 251, 2}
  end

  defp dec_huffman_lookup(220, 5) do
    {:more, 251, 9}
  end

  defp dec_huffman_lookup(220, 6) do
    {:more, 251, 23}
  end

  defp dec_huffman_lookup(220, 7) do
    {:ok, 251, 40}
  end

  defp dec_huffman_lookup(220, 8) do
    {:more, 252, 2}
  end

  defp dec_huffman_lookup(220, 9) do
    {:more, 252, 9}
  end

  defp dec_huffman_lookup(220, 10) do
    {:more, 252, 23}
  end

  defp dec_huffman_lookup(220, 11) do
    {:ok, 252, 40}
  end

  defp dec_huffman_lookup(220, 12) do
    {:more, 253, 2}
  end

  defp dec_huffman_lookup(220, 13) do
    {:more, 253, 9}
  end

  defp dec_huffman_lookup(220, 14) do
    {:more, 253, 23}
  end

  defp dec_huffman_lookup(220, 15) do
    {:ok, 253, 40}
  end

  defp dec_huffman_lookup(221, 0) do
    {:more, 250, 3}
  end

  defp dec_huffman_lookup(221, 1) do
    {:more, 250, 6}
  end

  defp dec_huffman_lookup(221, 2) do
    {:more, 250, 10}
  end

  defp dec_huffman_lookup(221, 3) do
    {:more, 250, 15}
  end

  defp dec_huffman_lookup(221, 4) do
    {:more, 250, 24}
  end

  defp dec_huffman_lookup(221, 5) do
    {:more, 250, 31}
  end

  defp dec_huffman_lookup(221, 6) do
    {:more, 250, 41}
  end

  defp dec_huffman_lookup(221, 7) do
    {:ok, 250, 56}
  end

  defp dec_huffman_lookup(221, 8) do
    {:more, 251, 3}
  end

  defp dec_huffman_lookup(221, 9) do
    {:more, 251, 6}
  end

  defp dec_huffman_lookup(221, 10) do
    {:more, 251, 10}
  end

  defp dec_huffman_lookup(221, 11) do
    {:more, 251, 15}
  end

  defp dec_huffman_lookup(221, 12) do
    {:more, 251, 24}
  end

  defp dec_huffman_lookup(221, 13) do
    {:more, 251, 31}
  end

  defp dec_huffman_lookup(221, 14) do
    {:more, 251, 41}
  end

  defp dec_huffman_lookup(221, 15) do
    {:ok, 251, 56}
  end

  defp dec_huffman_lookup(222, 0) do
    {:more, 252, 3}
  end

  defp dec_huffman_lookup(222, 1) do
    {:more, 252, 6}
  end

  defp dec_huffman_lookup(222, 2) do
    {:more, 252, 10}
  end

  defp dec_huffman_lookup(222, 3) do
    {:more, 252, 15}
  end

  defp dec_huffman_lookup(222, 4) do
    {:more, 252, 24}
  end

  defp dec_huffman_lookup(222, 5) do
    {:more, 252, 31}
  end

  defp dec_huffman_lookup(222, 6) do
    {:more, 252, 41}
  end

  defp dec_huffman_lookup(222, 7) do
    {:ok, 252, 56}
  end

  defp dec_huffman_lookup(222, 8) do
    {:more, 253, 3}
  end

  defp dec_huffman_lookup(222, 9) do
    {:more, 253, 6}
  end

  defp dec_huffman_lookup(222, 10) do
    {:more, 253, 10}
  end

  defp dec_huffman_lookup(222, 11) do
    {:more, 253, 15}
  end

  defp dec_huffman_lookup(222, 12) do
    {:more, 253, 24}
  end

  defp dec_huffman_lookup(222, 13) do
    {:more, 253, 31}
  end

  defp dec_huffman_lookup(222, 14) do
    {:more, 253, 41}
  end

  defp dec_huffman_lookup(222, 15) do
    {:ok, 253, 56}
  end

  defp dec_huffman_lookup(223, 0) do
    {:ok, 254, 0}
  end

  defp dec_huffman_lookup(223, 1) do
    {:more, :undefined, 227}
  end

  defp dec_huffman_lookup(223, 2) do
    {:more, :undefined, 229}
  end

  defp dec_huffman_lookup(223, 3) do
    {:more, :undefined, 230}
  end

  defp dec_huffman_lookup(223, 4) do
    {:more, :undefined, 233}
  end

  defp dec_huffman_lookup(223, 5) do
    {:more, :undefined, 234}
  end

  defp dec_huffman_lookup(223, 6) do
    {:more, :undefined, 236}
  end

  defp dec_huffman_lookup(223, 7) do
    {:more, :undefined, 237}
  end

  defp dec_huffman_lookup(223, 8) do
    {:more, :undefined, 241}
  end

  defp dec_huffman_lookup(223, 9) do
    {:more, :undefined, 242}
  end

  defp dec_huffman_lookup(223, 10) do
    {:more, :undefined, 244}
  end

  defp dec_huffman_lookup(223, 11) do
    {:more, :undefined, 245}
  end

  defp dec_huffman_lookup(223, 12) do
    {:more, :undefined, 248}
  end

  defp dec_huffman_lookup(223, 13) do
    {:more, :undefined, 249}
  end

  defp dec_huffman_lookup(223, 14) do
    {:more, :undefined, 251}
  end

  defp dec_huffman_lookup(223, 15) do
    {:ok, :undefined, 252}
  end

  defp dec_huffman_lookup(224, 0) do
    {:more, 254, 1}
  end

  defp dec_huffman_lookup(224, 1) do
    {:ok, 254, 22}
  end

  defp dec_huffman_lookup(224, 2) do
    {:ok, 2, 0}
  end

  defp dec_huffman_lookup(224, 3) do
    {:ok, 3, 0}
  end

  defp dec_huffman_lookup(224, 4) do
    {:ok, 4, 0}
  end

  defp dec_huffman_lookup(224, 5) do
    {:ok, 5, 0}
  end

  defp dec_huffman_lookup(224, 6) do
    {:ok, 6, 0}
  end

  defp dec_huffman_lookup(224, 7) do
    {:ok, 7, 0}
  end

  defp dec_huffman_lookup(224, 8) do
    {:ok, 8, 0}
  end

  defp dec_huffman_lookup(224, 9) do
    {:ok, 11, 0}
  end

  defp dec_huffman_lookup(224, 10) do
    {:ok, 12, 0}
  end

  defp dec_huffman_lookup(224, 11) do
    {:ok, 14, 0}
  end

  defp dec_huffman_lookup(224, 12) do
    {:ok, 15, 0}
  end

  defp dec_huffman_lookup(224, 13) do
    {:ok, 16, 0}
  end

  defp dec_huffman_lookup(224, 14) do
    {:ok, 17, 0}
  end

  defp dec_huffman_lookup(224, 15) do
    {:ok, 18, 0}
  end

  defp dec_huffman_lookup(225, 0) do
    {:more, 254, 2}
  end

  defp dec_huffman_lookup(225, 1) do
    {:more, 254, 9}
  end

  defp dec_huffman_lookup(225, 2) do
    {:more, 254, 23}
  end

  defp dec_huffman_lookup(225, 3) do
    {:ok, 254, 40}
  end

  defp dec_huffman_lookup(225, 4) do
    {:more, 2, 1}
  end

  defp dec_huffman_lookup(225, 5) do
    {:ok, 2, 22}
  end

  defp dec_huffman_lookup(225, 6) do
    {:more, 3, 1}
  end

  defp dec_huffman_lookup(225, 7) do
    {:ok, 3, 22}
  end

  defp dec_huffman_lookup(225, 8) do
    {:more, 4, 1}
  end

  defp dec_huffman_lookup(225, 9) do
    {:ok, 4, 22}
  end

  defp dec_huffman_lookup(225, 10) do
    {:more, 5, 1}
  end

  defp dec_huffman_lookup(225, 11) do
    {:ok, 5, 22}
  end

  defp dec_huffman_lookup(225, 12) do
    {:more, 6, 1}
  end

  defp dec_huffman_lookup(225, 13) do
    {:ok, 6, 22}
  end

  defp dec_huffman_lookup(225, 14) do
    {:more, 7, 1}
  end

  defp dec_huffman_lookup(225, 15) do
    {:ok, 7, 22}
  end

  defp dec_huffman_lookup(226, 0) do
    {:more, 254, 3}
  end

  defp dec_huffman_lookup(226, 1) do
    {:more, 254, 6}
  end

  defp dec_huffman_lookup(226, 2) do
    {:more, 254, 10}
  end

  defp dec_huffman_lookup(226, 3) do
    {:more, 254, 15}
  end

  defp dec_huffman_lookup(226, 4) do
    {:more, 254, 24}
  end

  defp dec_huffman_lookup(226, 5) do
    {:more, 254, 31}
  end

  defp dec_huffman_lookup(226, 6) do
    {:more, 254, 41}
  end

  defp dec_huffman_lookup(226, 7) do
    {:ok, 254, 56}
  end

  defp dec_huffman_lookup(226, 8) do
    {:more, 2, 2}
  end

  defp dec_huffman_lookup(226, 9) do
    {:more, 2, 9}
  end

  defp dec_huffman_lookup(226, 10) do
    {:more, 2, 23}
  end

  defp dec_huffman_lookup(226, 11) do
    {:ok, 2, 40}
  end

  defp dec_huffman_lookup(226, 12) do
    {:more, 3, 2}
  end

  defp dec_huffman_lookup(226, 13) do
    {:more, 3, 9}
  end

  defp dec_huffman_lookup(226, 14) do
    {:more, 3, 23}
  end

  defp dec_huffman_lookup(226, 15) do
    {:ok, 3, 40}
  end

  defp dec_huffman_lookup(227, 0) do
    {:more, 2, 3}
  end

  defp dec_huffman_lookup(227, 1) do
    {:more, 2, 6}
  end

  defp dec_huffman_lookup(227, 2) do
    {:more, 2, 10}
  end

  defp dec_huffman_lookup(227, 3) do
    {:more, 2, 15}
  end

  defp dec_huffman_lookup(227, 4) do
    {:more, 2, 24}
  end

  defp dec_huffman_lookup(227, 5) do
    {:more, 2, 31}
  end

  defp dec_huffman_lookup(227, 6) do
    {:more, 2, 41}
  end

  defp dec_huffman_lookup(227, 7) do
    {:ok, 2, 56}
  end

  defp dec_huffman_lookup(227, 8) do
    {:more, 3, 3}
  end

  defp dec_huffman_lookup(227, 9) do
    {:more, 3, 6}
  end

  defp dec_huffman_lookup(227, 10) do
    {:more, 3, 10}
  end

  defp dec_huffman_lookup(227, 11) do
    {:more, 3, 15}
  end

  defp dec_huffman_lookup(227, 12) do
    {:more, 3, 24}
  end

  defp dec_huffman_lookup(227, 13) do
    {:more, 3, 31}
  end

  defp dec_huffman_lookup(227, 14) do
    {:more, 3, 41}
  end

  defp dec_huffman_lookup(227, 15) do
    {:ok, 3, 56}
  end

  defp dec_huffman_lookup(228, 0) do
    {:more, 4, 2}
  end

  defp dec_huffman_lookup(228, 1) do
    {:more, 4, 9}
  end

  defp dec_huffman_lookup(228, 2) do
    {:more, 4, 23}
  end

  defp dec_huffman_lookup(228, 3) do
    {:ok, 4, 40}
  end

  defp dec_huffman_lookup(228, 4) do
    {:more, 5, 2}
  end

  defp dec_huffman_lookup(228, 5) do
    {:more, 5, 9}
  end

  defp dec_huffman_lookup(228, 6) do
    {:more, 5, 23}
  end

  defp dec_huffman_lookup(228, 7) do
    {:ok, 5, 40}
  end

  defp dec_huffman_lookup(228, 8) do
    {:more, 6, 2}
  end

  defp dec_huffman_lookup(228, 9) do
    {:more, 6, 9}
  end

  defp dec_huffman_lookup(228, 10) do
    {:more, 6, 23}
  end

  defp dec_huffman_lookup(228, 11) do
    {:ok, 6, 40}
  end

  defp dec_huffman_lookup(228, 12) do
    {:more, 7, 2}
  end

  defp dec_huffman_lookup(228, 13) do
    {:more, 7, 9}
  end

  defp dec_huffman_lookup(228, 14) do
    {:more, 7, 23}
  end

  defp dec_huffman_lookup(228, 15) do
    {:ok, 7, 40}
  end

  defp dec_huffman_lookup(229, 0) do
    {:more, 4, 3}
  end

  defp dec_huffman_lookup(229, 1) do
    {:more, 4, 6}
  end

  defp dec_huffman_lookup(229, 2) do
    {:more, 4, 10}
  end

  defp dec_huffman_lookup(229, 3) do
    {:more, 4, 15}
  end

  defp dec_huffman_lookup(229, 4) do
    {:more, 4, 24}
  end

  defp dec_huffman_lookup(229, 5) do
    {:more, 4, 31}
  end

  defp dec_huffman_lookup(229, 6) do
    {:more, 4, 41}
  end

  defp dec_huffman_lookup(229, 7) do
    {:ok, 4, 56}
  end

  defp dec_huffman_lookup(229, 8) do
    {:more, 5, 3}
  end

  defp dec_huffman_lookup(229, 9) do
    {:more, 5, 6}
  end

  defp dec_huffman_lookup(229, 10) do
    {:more, 5, 10}
  end

  defp dec_huffman_lookup(229, 11) do
    {:more, 5, 15}
  end

  defp dec_huffman_lookup(229, 12) do
    {:more, 5, 24}
  end

  defp dec_huffman_lookup(229, 13) do
    {:more, 5, 31}
  end

  defp dec_huffman_lookup(229, 14) do
    {:more, 5, 41}
  end

  defp dec_huffman_lookup(229, 15) do
    {:ok, 5, 56}
  end

  defp dec_huffman_lookup(230, 0) do
    {:more, 6, 3}
  end

  defp dec_huffman_lookup(230, 1) do
    {:more, 6, 6}
  end

  defp dec_huffman_lookup(230, 2) do
    {:more, 6, 10}
  end

  defp dec_huffman_lookup(230, 3) do
    {:more, 6, 15}
  end

  defp dec_huffman_lookup(230, 4) do
    {:more, 6, 24}
  end

  defp dec_huffman_lookup(230, 5) do
    {:more, 6, 31}
  end

  defp dec_huffman_lookup(230, 6) do
    {:more, 6, 41}
  end

  defp dec_huffman_lookup(230, 7) do
    {:ok, 6, 56}
  end

  defp dec_huffman_lookup(230, 8) do
    {:more, 7, 3}
  end

  defp dec_huffman_lookup(230, 9) do
    {:more, 7, 6}
  end

  defp dec_huffman_lookup(230, 10) do
    {:more, 7, 10}
  end

  defp dec_huffman_lookup(230, 11) do
    {:more, 7, 15}
  end

  defp dec_huffman_lookup(230, 12) do
    {:more, 7, 24}
  end

  defp dec_huffman_lookup(230, 13) do
    {:more, 7, 31}
  end

  defp dec_huffman_lookup(230, 14) do
    {:more, 7, 41}
  end

  defp dec_huffman_lookup(230, 15) do
    {:ok, 7, 56}
  end

  defp dec_huffman_lookup(231, 0) do
    {:more, 8, 1}
  end

  defp dec_huffman_lookup(231, 1) do
    {:ok, 8, 22}
  end

  defp dec_huffman_lookup(231, 2) do
    {:more, 11, 1}
  end

  defp dec_huffman_lookup(231, 3) do
    {:ok, 11, 22}
  end

  defp dec_huffman_lookup(231, 4) do
    {:more, 12, 1}
  end

  defp dec_huffman_lookup(231, 5) do
    {:ok, 12, 22}
  end

  defp dec_huffman_lookup(231, 6) do
    {:more, 14, 1}
  end

  defp dec_huffman_lookup(231, 7) do
    {:ok, 14, 22}
  end

  defp dec_huffman_lookup(231, 8) do
    {:more, 15, 1}
  end

  defp dec_huffman_lookup(231, 9) do
    {:ok, 15, 22}
  end

  defp dec_huffman_lookup(231, 10) do
    {:more, 16, 1}
  end

  defp dec_huffman_lookup(231, 11) do
    {:ok, 16, 22}
  end

  defp dec_huffman_lookup(231, 12) do
    {:more, 17, 1}
  end

  defp dec_huffman_lookup(231, 13) do
    {:ok, 17, 22}
  end

  defp dec_huffman_lookup(231, 14) do
    {:more, 18, 1}
  end

  defp dec_huffman_lookup(231, 15) do
    {:ok, 18, 22}
  end

  defp dec_huffman_lookup(232, 0) do
    {:more, 8, 2}
  end

  defp dec_huffman_lookup(232, 1) do
    {:more, 8, 9}
  end

  defp dec_huffman_lookup(232, 2) do
    {:more, 8, 23}
  end

  defp dec_huffman_lookup(232, 3) do
    {:ok, 8, 40}
  end

  defp dec_huffman_lookup(232, 4) do
    {:more, 11, 2}
  end

  defp dec_huffman_lookup(232, 5) do
    {:more, 11, 9}
  end

  defp dec_huffman_lookup(232, 6) do
    {:more, 11, 23}
  end

  defp dec_huffman_lookup(232, 7) do
    {:ok, 11, 40}
  end

  defp dec_huffman_lookup(232, 8) do
    {:more, 12, 2}
  end

  defp dec_huffman_lookup(232, 9) do
    {:more, 12, 9}
  end

  defp dec_huffman_lookup(232, 10) do
    {:more, 12, 23}
  end

  defp dec_huffman_lookup(232, 11) do
    {:ok, 12, 40}
  end

  defp dec_huffman_lookup(232, 12) do
    {:more, 14, 2}
  end

  defp dec_huffman_lookup(232, 13) do
    {:more, 14, 9}
  end

  defp dec_huffman_lookup(232, 14) do
    {:more, 14, 23}
  end

  defp dec_huffman_lookup(232, 15) do
    {:ok, 14, 40}
  end

  defp dec_huffman_lookup(233, 0) do
    {:more, 8, 3}
  end

  defp dec_huffman_lookup(233, 1) do
    {:more, 8, 6}
  end

  defp dec_huffman_lookup(233, 2) do
    {:more, 8, 10}
  end

  defp dec_huffman_lookup(233, 3) do
    {:more, 8, 15}
  end

  defp dec_huffman_lookup(233, 4) do
    {:more, 8, 24}
  end

  defp dec_huffman_lookup(233, 5) do
    {:more, 8, 31}
  end

  defp dec_huffman_lookup(233, 6) do
    {:more, 8, 41}
  end

  defp dec_huffman_lookup(233, 7) do
    {:ok, 8, 56}
  end

  defp dec_huffman_lookup(233, 8) do
    {:more, 11, 3}
  end

  defp dec_huffman_lookup(233, 9) do
    {:more, 11, 6}
  end

  defp dec_huffman_lookup(233, 10) do
    {:more, 11, 10}
  end

  defp dec_huffman_lookup(233, 11) do
    {:more, 11, 15}
  end

  defp dec_huffman_lookup(233, 12) do
    {:more, 11, 24}
  end

  defp dec_huffman_lookup(233, 13) do
    {:more, 11, 31}
  end

  defp dec_huffman_lookup(233, 14) do
    {:more, 11, 41}
  end

  defp dec_huffman_lookup(233, 15) do
    {:ok, 11, 56}
  end

  defp dec_huffman_lookup(234, 0) do
    {:more, 12, 3}
  end

  defp dec_huffman_lookup(234, 1) do
    {:more, 12, 6}
  end

  defp dec_huffman_lookup(234, 2) do
    {:more, 12, 10}
  end

  defp dec_huffman_lookup(234, 3) do
    {:more, 12, 15}
  end

  defp dec_huffman_lookup(234, 4) do
    {:more, 12, 24}
  end

  defp dec_huffman_lookup(234, 5) do
    {:more, 12, 31}
  end

  defp dec_huffman_lookup(234, 6) do
    {:more, 12, 41}
  end

  defp dec_huffman_lookup(234, 7) do
    {:ok, 12, 56}
  end

  defp dec_huffman_lookup(234, 8) do
    {:more, 14, 3}
  end

  defp dec_huffman_lookup(234, 9) do
    {:more, 14, 6}
  end

  defp dec_huffman_lookup(234, 10) do
    {:more, 14, 10}
  end

  defp dec_huffman_lookup(234, 11) do
    {:more, 14, 15}
  end

  defp dec_huffman_lookup(234, 12) do
    {:more, 14, 24}
  end

  defp dec_huffman_lookup(234, 13) do
    {:more, 14, 31}
  end

  defp dec_huffman_lookup(234, 14) do
    {:more, 14, 41}
  end

  defp dec_huffman_lookup(234, 15) do
    {:ok, 14, 56}
  end

  defp dec_huffman_lookup(235, 0) do
    {:more, 15, 2}
  end

  defp dec_huffman_lookup(235, 1) do
    {:more, 15, 9}
  end

  defp dec_huffman_lookup(235, 2) do
    {:more, 15, 23}
  end

  defp dec_huffman_lookup(235, 3) do
    {:ok, 15, 40}
  end

  defp dec_huffman_lookup(235, 4) do
    {:more, 16, 2}
  end

  defp dec_huffman_lookup(235, 5) do
    {:more, 16, 9}
  end

  defp dec_huffman_lookup(235, 6) do
    {:more, 16, 23}
  end

  defp dec_huffman_lookup(235, 7) do
    {:ok, 16, 40}
  end

  defp dec_huffman_lookup(235, 8) do
    {:more, 17, 2}
  end

  defp dec_huffman_lookup(235, 9) do
    {:more, 17, 9}
  end

  defp dec_huffman_lookup(235, 10) do
    {:more, 17, 23}
  end

  defp dec_huffman_lookup(235, 11) do
    {:ok, 17, 40}
  end

  defp dec_huffman_lookup(235, 12) do
    {:more, 18, 2}
  end

  defp dec_huffman_lookup(235, 13) do
    {:more, 18, 9}
  end

  defp dec_huffman_lookup(235, 14) do
    {:more, 18, 23}
  end

  defp dec_huffman_lookup(235, 15) do
    {:ok, 18, 40}
  end

  defp dec_huffman_lookup(236, 0) do
    {:more, 15, 3}
  end

  defp dec_huffman_lookup(236, 1) do
    {:more, 15, 6}
  end

  defp dec_huffman_lookup(236, 2) do
    {:more, 15, 10}
  end

  defp dec_huffman_lookup(236, 3) do
    {:more, 15, 15}
  end

  defp dec_huffman_lookup(236, 4) do
    {:more, 15, 24}
  end

  defp dec_huffman_lookup(236, 5) do
    {:more, 15, 31}
  end

  defp dec_huffman_lookup(236, 6) do
    {:more, 15, 41}
  end

  defp dec_huffman_lookup(236, 7) do
    {:ok, 15, 56}
  end

  defp dec_huffman_lookup(236, 8) do
    {:more, 16, 3}
  end

  defp dec_huffman_lookup(236, 9) do
    {:more, 16, 6}
  end

  defp dec_huffman_lookup(236, 10) do
    {:more, 16, 10}
  end

  defp dec_huffman_lookup(236, 11) do
    {:more, 16, 15}
  end

  defp dec_huffman_lookup(236, 12) do
    {:more, 16, 24}
  end

  defp dec_huffman_lookup(236, 13) do
    {:more, 16, 31}
  end

  defp dec_huffman_lookup(236, 14) do
    {:more, 16, 41}
  end

  defp dec_huffman_lookup(236, 15) do
    {:ok, 16, 56}
  end

  defp dec_huffman_lookup(237, 0) do
    {:more, 17, 3}
  end

  defp dec_huffman_lookup(237, 1) do
    {:more, 17, 6}
  end

  defp dec_huffman_lookup(237, 2) do
    {:more, 17, 10}
  end

  defp dec_huffman_lookup(237, 3) do
    {:more, 17, 15}
  end

  defp dec_huffman_lookup(237, 4) do
    {:more, 17, 24}
  end

  defp dec_huffman_lookup(237, 5) do
    {:more, 17, 31}
  end

  defp dec_huffman_lookup(237, 6) do
    {:more, 17, 41}
  end

  defp dec_huffman_lookup(237, 7) do
    {:ok, 17, 56}
  end

  defp dec_huffman_lookup(237, 8) do
    {:more, 18, 3}
  end

  defp dec_huffman_lookup(237, 9) do
    {:more, 18, 6}
  end

  defp dec_huffman_lookup(237, 10) do
    {:more, 18, 10}
  end

  defp dec_huffman_lookup(237, 11) do
    {:more, 18, 15}
  end

  defp dec_huffman_lookup(237, 12) do
    {:more, 18, 24}
  end

  defp dec_huffman_lookup(237, 13) do
    {:more, 18, 31}
  end

  defp dec_huffman_lookup(237, 14) do
    {:more, 18, 41}
  end

  defp dec_huffman_lookup(237, 15) do
    {:ok, 18, 56}
  end

  defp dec_huffman_lookup(238, 0) do
    {:ok, 19, 0}
  end

  defp dec_huffman_lookup(238, 1) do
    {:ok, 20, 0}
  end

  defp dec_huffman_lookup(238, 2) do
    {:ok, 21, 0}
  end

  defp dec_huffman_lookup(238, 3) do
    {:ok, 23, 0}
  end

  defp dec_huffman_lookup(238, 4) do
    {:ok, 24, 0}
  end

  defp dec_huffman_lookup(238, 5) do
    {:ok, 25, 0}
  end

  defp dec_huffman_lookup(238, 6) do
    {:ok, 26, 0}
  end

  defp dec_huffman_lookup(238, 7) do
    {:ok, 27, 0}
  end

  defp dec_huffman_lookup(238, 8) do
    {:ok, 28, 0}
  end

  defp dec_huffman_lookup(238, 9) do
    {:ok, 29, 0}
  end

  defp dec_huffman_lookup(238, 10) do
    {:ok, 30, 0}
  end

  defp dec_huffman_lookup(238, 11) do
    {:ok, 31, 0}
  end

  defp dec_huffman_lookup(238, 12) do
    {:ok, 127, 0}
  end

  defp dec_huffman_lookup(238, 13) do
    {:ok, 220, 0}
  end

  defp dec_huffman_lookup(238, 14) do
    {:ok, 249, 0}
  end

  defp dec_huffman_lookup(238, 15) do
    {:ok, :undefined, 253}
  end

  defp dec_huffman_lookup(239, 0) do
    {:more, 19, 1}
  end

  defp dec_huffman_lookup(239, 1) do
    {:ok, 19, 22}
  end

  defp dec_huffman_lookup(239, 2) do
    {:more, 20, 1}
  end

  defp dec_huffman_lookup(239, 3) do
    {:ok, 20, 22}
  end

  defp dec_huffman_lookup(239, 4) do
    {:more, 21, 1}
  end

  defp dec_huffman_lookup(239, 5) do
    {:ok, 21, 22}
  end

  defp dec_huffman_lookup(239, 6) do
    {:more, 23, 1}
  end

  defp dec_huffman_lookup(239, 7) do
    {:ok, 23, 22}
  end

  defp dec_huffman_lookup(239, 8) do
    {:more, 24, 1}
  end

  defp dec_huffman_lookup(239, 9) do
    {:ok, 24, 22}
  end

  defp dec_huffman_lookup(239, 10) do
    {:more, 25, 1}
  end

  defp dec_huffman_lookup(239, 11) do
    {:ok, 25, 22}
  end

  defp dec_huffman_lookup(239, 12) do
    {:more, 26, 1}
  end

  defp dec_huffman_lookup(239, 13) do
    {:ok, 26, 22}
  end

  defp dec_huffman_lookup(239, 14) do
    {:more, 27, 1}
  end

  defp dec_huffman_lookup(239, 15) do
    {:ok, 27, 22}
  end

  defp dec_huffman_lookup(240, 0) do
    {:more, 19, 2}
  end

  defp dec_huffman_lookup(240, 1) do
    {:more, 19, 9}
  end

  defp dec_huffman_lookup(240, 2) do
    {:more, 19, 23}
  end

  defp dec_huffman_lookup(240, 3) do
    {:ok, 19, 40}
  end

  defp dec_huffman_lookup(240, 4) do
    {:more, 20, 2}
  end

  defp dec_huffman_lookup(240, 5) do
    {:more, 20, 9}
  end

  defp dec_huffman_lookup(240, 6) do
    {:more, 20, 23}
  end

  defp dec_huffman_lookup(240, 7) do
    {:ok, 20, 40}
  end

  defp dec_huffman_lookup(240, 8) do
    {:more, 21, 2}
  end

  defp dec_huffman_lookup(240, 9) do
    {:more, 21, 9}
  end

  defp dec_huffman_lookup(240, 10) do
    {:more, 21, 23}
  end

  defp dec_huffman_lookup(240, 11) do
    {:ok, 21, 40}
  end

  defp dec_huffman_lookup(240, 12) do
    {:more, 23, 2}
  end

  defp dec_huffman_lookup(240, 13) do
    {:more, 23, 9}
  end

  defp dec_huffman_lookup(240, 14) do
    {:more, 23, 23}
  end

  defp dec_huffman_lookup(240, 15) do
    {:ok, 23, 40}
  end

  defp dec_huffman_lookup(241, 0) do
    {:more, 19, 3}
  end

  defp dec_huffman_lookup(241, 1) do
    {:more, 19, 6}
  end

  defp dec_huffman_lookup(241, 2) do
    {:more, 19, 10}
  end

  defp dec_huffman_lookup(241, 3) do
    {:more, 19, 15}
  end

  defp dec_huffman_lookup(241, 4) do
    {:more, 19, 24}
  end

  defp dec_huffman_lookup(241, 5) do
    {:more, 19, 31}
  end

  defp dec_huffman_lookup(241, 6) do
    {:more, 19, 41}
  end

  defp dec_huffman_lookup(241, 7) do
    {:ok, 19, 56}
  end

  defp dec_huffman_lookup(241, 8) do
    {:more, 20, 3}
  end

  defp dec_huffman_lookup(241, 9) do
    {:more, 20, 6}
  end

  defp dec_huffman_lookup(241, 10) do
    {:more, 20, 10}
  end

  defp dec_huffman_lookup(241, 11) do
    {:more, 20, 15}
  end

  defp dec_huffman_lookup(241, 12) do
    {:more, 20, 24}
  end

  defp dec_huffman_lookup(241, 13) do
    {:more, 20, 31}
  end

  defp dec_huffman_lookup(241, 14) do
    {:more, 20, 41}
  end

  defp dec_huffman_lookup(241, 15) do
    {:ok, 20, 56}
  end

  defp dec_huffman_lookup(242, 0) do
    {:more, 21, 3}
  end

  defp dec_huffman_lookup(242, 1) do
    {:more, 21, 6}
  end

  defp dec_huffman_lookup(242, 2) do
    {:more, 21, 10}
  end

  defp dec_huffman_lookup(242, 3) do
    {:more, 21, 15}
  end

  defp dec_huffman_lookup(242, 4) do
    {:more, 21, 24}
  end

  defp dec_huffman_lookup(242, 5) do
    {:more, 21, 31}
  end

  defp dec_huffman_lookup(242, 6) do
    {:more, 21, 41}
  end

  defp dec_huffman_lookup(242, 7) do
    {:ok, 21, 56}
  end

  defp dec_huffman_lookup(242, 8) do
    {:more, 23, 3}
  end

  defp dec_huffman_lookup(242, 9) do
    {:more, 23, 6}
  end

  defp dec_huffman_lookup(242, 10) do
    {:more, 23, 10}
  end

  defp dec_huffman_lookup(242, 11) do
    {:more, 23, 15}
  end

  defp dec_huffman_lookup(242, 12) do
    {:more, 23, 24}
  end

  defp dec_huffman_lookup(242, 13) do
    {:more, 23, 31}
  end

  defp dec_huffman_lookup(242, 14) do
    {:more, 23, 41}
  end

  defp dec_huffman_lookup(242, 15) do
    {:ok, 23, 56}
  end

  defp dec_huffman_lookup(243, 0) do
    {:more, 24, 2}
  end

  defp dec_huffman_lookup(243, 1) do
    {:more, 24, 9}
  end

  defp dec_huffman_lookup(243, 2) do
    {:more, 24, 23}
  end

  defp dec_huffman_lookup(243, 3) do
    {:ok, 24, 40}
  end

  defp dec_huffman_lookup(243, 4) do
    {:more, 25, 2}
  end

  defp dec_huffman_lookup(243, 5) do
    {:more, 25, 9}
  end

  defp dec_huffman_lookup(243, 6) do
    {:more, 25, 23}
  end

  defp dec_huffman_lookup(243, 7) do
    {:ok, 25, 40}
  end

  defp dec_huffman_lookup(243, 8) do
    {:more, 26, 2}
  end

  defp dec_huffman_lookup(243, 9) do
    {:more, 26, 9}
  end

  defp dec_huffman_lookup(243, 10) do
    {:more, 26, 23}
  end

  defp dec_huffman_lookup(243, 11) do
    {:ok, 26, 40}
  end

  defp dec_huffman_lookup(243, 12) do
    {:more, 27, 2}
  end

  defp dec_huffman_lookup(243, 13) do
    {:more, 27, 9}
  end

  defp dec_huffman_lookup(243, 14) do
    {:more, 27, 23}
  end

  defp dec_huffman_lookup(243, 15) do
    {:ok, 27, 40}
  end

  defp dec_huffman_lookup(244, 0) do
    {:more, 24, 3}
  end

  defp dec_huffman_lookup(244, 1) do
    {:more, 24, 6}
  end

  defp dec_huffman_lookup(244, 2) do
    {:more, 24, 10}
  end

  defp dec_huffman_lookup(244, 3) do
    {:more, 24, 15}
  end

  defp dec_huffman_lookup(244, 4) do
    {:more, 24, 24}
  end

  defp dec_huffman_lookup(244, 5) do
    {:more, 24, 31}
  end

  defp dec_huffman_lookup(244, 6) do
    {:more, 24, 41}
  end

  defp dec_huffman_lookup(244, 7) do
    {:ok, 24, 56}
  end

  defp dec_huffman_lookup(244, 8) do
    {:more, 25, 3}
  end

  defp dec_huffman_lookup(244, 9) do
    {:more, 25, 6}
  end

  defp dec_huffman_lookup(244, 10) do
    {:more, 25, 10}
  end

  defp dec_huffman_lookup(244, 11) do
    {:more, 25, 15}
  end

  defp dec_huffman_lookup(244, 12) do
    {:more, 25, 24}
  end

  defp dec_huffman_lookup(244, 13) do
    {:more, 25, 31}
  end

  defp dec_huffman_lookup(244, 14) do
    {:more, 25, 41}
  end

  defp dec_huffman_lookup(244, 15) do
    {:ok, 25, 56}
  end

  defp dec_huffman_lookup(245, 0) do
    {:more, 26, 3}
  end

  defp dec_huffman_lookup(245, 1) do
    {:more, 26, 6}
  end

  defp dec_huffman_lookup(245, 2) do
    {:more, 26, 10}
  end

  defp dec_huffman_lookup(245, 3) do
    {:more, 26, 15}
  end

  defp dec_huffman_lookup(245, 4) do
    {:more, 26, 24}
  end

  defp dec_huffman_lookup(245, 5) do
    {:more, 26, 31}
  end

  defp dec_huffman_lookup(245, 6) do
    {:more, 26, 41}
  end

  defp dec_huffman_lookup(245, 7) do
    {:ok, 26, 56}
  end

  defp dec_huffman_lookup(245, 8) do
    {:more, 27, 3}
  end

  defp dec_huffman_lookup(245, 9) do
    {:more, 27, 6}
  end

  defp dec_huffman_lookup(245, 10) do
    {:more, 27, 10}
  end

  defp dec_huffman_lookup(245, 11) do
    {:more, 27, 15}
  end

  defp dec_huffman_lookup(245, 12) do
    {:more, 27, 24}
  end

  defp dec_huffman_lookup(245, 13) do
    {:more, 27, 31}
  end

  defp dec_huffman_lookup(245, 14) do
    {:more, 27, 41}
  end

  defp dec_huffman_lookup(245, 15) do
    {:ok, 27, 56}
  end

  defp dec_huffman_lookup(246, 0) do
    {:more, 28, 1}
  end

  defp dec_huffman_lookup(246, 1) do
    {:ok, 28, 22}
  end

  defp dec_huffman_lookup(246, 2) do
    {:more, 29, 1}
  end

  defp dec_huffman_lookup(246, 3) do
    {:ok, 29, 22}
  end

  defp dec_huffman_lookup(246, 4) do
    {:more, 30, 1}
  end

  defp dec_huffman_lookup(246, 5) do
    {:ok, 30, 22}
  end

  defp dec_huffman_lookup(246, 6) do
    {:more, 31, 1}
  end

  defp dec_huffman_lookup(246, 7) do
    {:ok, 31, 22}
  end

  defp dec_huffman_lookup(246, 8) do
    {:more, 127, 1}
  end

  defp dec_huffman_lookup(246, 9) do
    {:ok, 127, 22}
  end

  defp dec_huffman_lookup(246, 10) do
    {:more, 220, 1}
  end

  defp dec_huffman_lookup(246, 11) do
    {:ok, 220, 22}
  end

  defp dec_huffman_lookup(246, 12) do
    {:more, 249, 1}
  end

  defp dec_huffman_lookup(246, 13) do
    {:ok, 249, 22}
  end

  defp dec_huffman_lookup(246, 14) do
    {:more, :undefined, 254}
  end

  defp dec_huffman_lookup(246, 15) do
    {:ok, :undefined, 255}
  end

  defp dec_huffman_lookup(247, 0) do
    {:more, 28, 2}
  end

  defp dec_huffman_lookup(247, 1) do
    {:more, 28, 9}
  end

  defp dec_huffman_lookup(247, 2) do
    {:more, 28, 23}
  end

  defp dec_huffman_lookup(247, 3) do
    {:ok, 28, 40}
  end

  defp dec_huffman_lookup(247, 4) do
    {:more, 29, 2}
  end

  defp dec_huffman_lookup(247, 5) do
    {:more, 29, 9}
  end

  defp dec_huffman_lookup(247, 6) do
    {:more, 29, 23}
  end

  defp dec_huffman_lookup(247, 7) do
    {:ok, 29, 40}
  end

  defp dec_huffman_lookup(247, 8) do
    {:more, 30, 2}
  end

  defp dec_huffman_lookup(247, 9) do
    {:more, 30, 9}
  end

  defp dec_huffman_lookup(247, 10) do
    {:more, 30, 23}
  end

  defp dec_huffman_lookup(247, 11) do
    {:ok, 30, 40}
  end

  defp dec_huffman_lookup(247, 12) do
    {:more, 31, 2}
  end

  defp dec_huffman_lookup(247, 13) do
    {:more, 31, 9}
  end

  defp dec_huffman_lookup(247, 14) do
    {:more, 31, 23}
  end

  defp dec_huffman_lookup(247, 15) do
    {:ok, 31, 40}
  end

  defp dec_huffman_lookup(248, 0) do
    {:more, 28, 3}
  end

  defp dec_huffman_lookup(248, 1) do
    {:more, 28, 6}
  end

  defp dec_huffman_lookup(248, 2) do
    {:more, 28, 10}
  end

  defp dec_huffman_lookup(248, 3) do
    {:more, 28, 15}
  end

  defp dec_huffman_lookup(248, 4) do
    {:more, 28, 24}
  end

  defp dec_huffman_lookup(248, 5) do
    {:more, 28, 31}
  end

  defp dec_huffman_lookup(248, 6) do
    {:more, 28, 41}
  end

  defp dec_huffman_lookup(248, 7) do
    {:ok, 28, 56}
  end

  defp dec_huffman_lookup(248, 8) do
    {:more, 29, 3}
  end

  defp dec_huffman_lookup(248, 9) do
    {:more, 29, 6}
  end

  defp dec_huffman_lookup(248, 10) do
    {:more, 29, 10}
  end

  defp dec_huffman_lookup(248, 11) do
    {:more, 29, 15}
  end

  defp dec_huffman_lookup(248, 12) do
    {:more, 29, 24}
  end

  defp dec_huffman_lookup(248, 13) do
    {:more, 29, 31}
  end

  defp dec_huffman_lookup(248, 14) do
    {:more, 29, 41}
  end

  defp dec_huffman_lookup(248, 15) do
    {:ok, 29, 56}
  end

  defp dec_huffman_lookup(249, 0) do
    {:more, 30, 3}
  end

  defp dec_huffman_lookup(249, 1) do
    {:more, 30, 6}
  end

  defp dec_huffman_lookup(249, 2) do
    {:more, 30, 10}
  end

  defp dec_huffman_lookup(249, 3) do
    {:more, 30, 15}
  end

  defp dec_huffman_lookup(249, 4) do
    {:more, 30, 24}
  end

  defp dec_huffman_lookup(249, 5) do
    {:more, 30, 31}
  end

  defp dec_huffman_lookup(249, 6) do
    {:more, 30, 41}
  end

  defp dec_huffman_lookup(249, 7) do
    {:ok, 30, 56}
  end

  defp dec_huffman_lookup(249, 8) do
    {:more, 31, 3}
  end

  defp dec_huffman_lookup(249, 9) do
    {:more, 31, 6}
  end

  defp dec_huffman_lookup(249, 10) do
    {:more, 31, 10}
  end

  defp dec_huffman_lookup(249, 11) do
    {:more, 31, 15}
  end

  defp dec_huffman_lookup(249, 12) do
    {:more, 31, 24}
  end

  defp dec_huffman_lookup(249, 13) do
    {:more, 31, 31}
  end

  defp dec_huffman_lookup(249, 14) do
    {:more, 31, 41}
  end

  defp dec_huffman_lookup(249, 15) do
    {:ok, 31, 56}
  end

  defp dec_huffman_lookup(250, 0) do
    {:more, 127, 2}
  end

  defp dec_huffman_lookup(250, 1) do
    {:more, 127, 9}
  end

  defp dec_huffman_lookup(250, 2) do
    {:more, 127, 23}
  end

  defp dec_huffman_lookup(250, 3) do
    {:ok, 127, 40}
  end

  defp dec_huffman_lookup(250, 4) do
    {:more, 220, 2}
  end

  defp dec_huffman_lookup(250, 5) do
    {:more, 220, 9}
  end

  defp dec_huffman_lookup(250, 6) do
    {:more, 220, 23}
  end

  defp dec_huffman_lookup(250, 7) do
    {:ok, 220, 40}
  end

  defp dec_huffman_lookup(250, 8) do
    {:more, 249, 2}
  end

  defp dec_huffman_lookup(250, 9) do
    {:more, 249, 9}
  end

  defp dec_huffman_lookup(250, 10) do
    {:more, 249, 23}
  end

  defp dec_huffman_lookup(250, 11) do
    {:ok, 249, 40}
  end

  defp dec_huffman_lookup(250, 12) do
    {:ok, 10, 0}
  end

  defp dec_huffman_lookup(250, 13) do
    {:ok, 13, 0}
  end

  defp dec_huffman_lookup(250, 14) do
    {:ok, 22, 0}
  end

  defp dec_huffman_lookup(250, 15) do
    :error
  end

  defp dec_huffman_lookup(251, 0) do
    {:more, 127, 3}
  end

  defp dec_huffman_lookup(251, 1) do
    {:more, 127, 6}
  end

  defp dec_huffman_lookup(251, 2) do
    {:more, 127, 10}
  end

  defp dec_huffman_lookup(251, 3) do
    {:more, 127, 15}
  end

  defp dec_huffman_lookup(251, 4) do
    {:more, 127, 24}
  end

  defp dec_huffman_lookup(251, 5) do
    {:more, 127, 31}
  end

  defp dec_huffman_lookup(251, 6) do
    {:more, 127, 41}
  end

  defp dec_huffman_lookup(251, 7) do
    {:ok, 127, 56}
  end

  defp dec_huffman_lookup(251, 8) do
    {:more, 220, 3}
  end

  defp dec_huffman_lookup(251, 9) do
    {:more, 220, 6}
  end

  defp dec_huffman_lookup(251, 10) do
    {:more, 220, 10}
  end

  defp dec_huffman_lookup(251, 11) do
    {:more, 220, 15}
  end

  defp dec_huffman_lookup(251, 12) do
    {:more, 220, 24}
  end

  defp dec_huffman_lookup(251, 13) do
    {:more, 220, 31}
  end

  defp dec_huffman_lookup(251, 14) do
    {:more, 220, 41}
  end

  defp dec_huffman_lookup(251, 15) do
    {:ok, 220, 56}
  end

  defp dec_huffman_lookup(252, 0) do
    {:more, 249, 3}
  end

  defp dec_huffman_lookup(252, 1) do
    {:more, 249, 6}
  end

  defp dec_huffman_lookup(252, 2) do
    {:more, 249, 10}
  end

  defp dec_huffman_lookup(252, 3) do
    {:more, 249, 15}
  end

  defp dec_huffman_lookup(252, 4) do
    {:more, 249, 24}
  end

  defp dec_huffman_lookup(252, 5) do
    {:more, 249, 31}
  end

  defp dec_huffman_lookup(252, 6) do
    {:more, 249, 41}
  end

  defp dec_huffman_lookup(252, 7) do
    {:ok, 249, 56}
  end

  defp dec_huffman_lookup(252, 8) do
    {:more, 10, 1}
  end

  defp dec_huffman_lookup(252, 9) do
    {:ok, 10, 22}
  end

  defp dec_huffman_lookup(252, 10) do
    {:more, 13, 1}
  end

  defp dec_huffman_lookup(252, 11) do
    {:ok, 13, 22}
  end

  defp dec_huffman_lookup(252, 12) do
    {:more, 22, 1}
  end

  defp dec_huffman_lookup(252, 13) do
    {:ok, 22, 22}
  end

  defp dec_huffman_lookup(252, 14) do
    :error
  end

  defp dec_huffman_lookup(252, 15) do
    :error
  end

  defp dec_huffman_lookup(253, 0) do
    {:more, 10, 2}
  end

  defp dec_huffman_lookup(253, 1) do
    {:more, 10, 9}
  end

  defp dec_huffman_lookup(253, 2) do
    {:more, 10, 23}
  end

  defp dec_huffman_lookup(253, 3) do
    {:ok, 10, 40}
  end

  defp dec_huffman_lookup(253, 4) do
    {:more, 13, 2}
  end

  defp dec_huffman_lookup(253, 5) do
    {:more, 13, 9}
  end

  defp dec_huffman_lookup(253, 6) do
    {:more, 13, 23}
  end

  defp dec_huffman_lookup(253, 7) do
    {:ok, 13, 40}
  end

  defp dec_huffman_lookup(253, 8) do
    {:more, 22, 2}
  end

  defp dec_huffman_lookup(253, 9) do
    {:more, 22, 9}
  end

  defp dec_huffman_lookup(253, 10) do
    {:more, 22, 23}
  end

  defp dec_huffman_lookup(253, 11) do
    {:ok, 22, 40}
  end

  defp dec_huffman_lookup(253, 12) do
    :error
  end

  defp dec_huffman_lookup(253, 13) do
    :error
  end

  defp dec_huffman_lookup(253, 14) do
    :error
  end

  defp dec_huffman_lookup(253, 15) do
    :error
  end

  defp dec_huffman_lookup(254, 0) do
    {:more, 10, 3}
  end

  defp dec_huffman_lookup(254, 1) do
    {:more, 10, 6}
  end

  defp dec_huffman_lookup(254, 2) do
    {:more, 10, 10}
  end

  defp dec_huffman_lookup(254, 3) do
    {:more, 10, 15}
  end

  defp dec_huffman_lookup(254, 4) do
    {:more, 10, 24}
  end

  defp dec_huffman_lookup(254, 5) do
    {:more, 10, 31}
  end

  defp dec_huffman_lookup(254, 6) do
    {:more, 10, 41}
  end

  defp dec_huffman_lookup(254, 7) do
    {:ok, 10, 56}
  end

  defp dec_huffman_lookup(254, 8) do
    {:more, 13, 3}
  end

  defp dec_huffman_lookup(254, 9) do
    {:more, 13, 6}
  end

  defp dec_huffman_lookup(254, 10) do
    {:more, 13, 10}
  end

  defp dec_huffman_lookup(254, 11) do
    {:more, 13, 15}
  end

  defp dec_huffman_lookup(254, 12) do
    {:more, 13, 24}
  end

  defp dec_huffman_lookup(254, 13) do
    {:more, 13, 31}
  end

  defp dec_huffman_lookup(254, 14) do
    {:more, 13, 41}
  end

  defp dec_huffman_lookup(254, 15) do
    {:ok, 13, 56}
  end

  defp dec_huffman_lookup(255, 0) do
    {:more, 22, 3}
  end

  defp dec_huffman_lookup(255, 1) do
    {:more, 22, 6}
  end

  defp dec_huffman_lookup(255, 2) do
    {:more, 22, 10}
  end

  defp dec_huffman_lookup(255, 3) do
    {:more, 22, 15}
  end

  defp dec_huffman_lookup(255, 4) do
    {:more, 22, 24}
  end

  defp dec_huffman_lookup(255, 5) do
    {:more, 22, 31}
  end

  defp dec_huffman_lookup(255, 6) do
    {:more, 22, 41}
  end

  defp dec_huffman_lookup(255, 7) do
    {:ok, 22, 56}
  end

  defp dec_huffman_lookup(255, 8) do
    :error
  end

  defp dec_huffman_lookup(255, 9) do
    :error
  end

  defp dec_huffman_lookup(255, 10) do
    :error
  end

  defp dec_huffman_lookup(255, 11) do
    :error
  end

  defp dec_huffman_lookup(255, 12) do
    :error
  end

  defp dec_huffman_lookup(255, 13) do
    :error
  end

  defp dec_huffman_lookup(255, 14) do
    :error
  end

  defp dec_huffman_lookup(255, 15) do
    :error
  end

  def encode(headers) do
    encode(headers, init(), :huffman, [])
  end

  def encode(headers,
           state = r_state(max_size: maxSize,
                       configured_max_size: maxSize)) do
    encode(headers, state, :huffman, [])
  end

  def encode(headers,
           state0 = r_state(configured_max_size: maxSize)) do
    state1 = table_update_size(maxSize, state0)
    {data, state} = encode(headers, state1, :huffman, [])
    {[enc_int5(maxSize, 1) | data], state}
  end

  def encode(headers,
           state = r_state(max_size: maxSize,
                       configured_max_size: maxSize),
           opts) do
    encode(headers, state, huffman_opt(opts), [])
  end

  def encode(headers,
           state0 = r_state(configured_max_size: maxSize), opts) do
    state1 = table_update_size(maxSize, state0)
    {data, state} = encode(headers, state1,
                             huffman_opt(opts), [])
    {[enc_int5(maxSize, 1) | data], state}
  end

  defp huffman_opt(%{huffman: false}) do
    :no_huffman
  end

  defp huffman_opt(_) do
    :huffman
  end

  defp encode([], state, _, acc) do
    {:lists.reverse(acc), state}
  end

  defp encode([{name, value0} | tail], state, huffmanOpt,
            acc) do
    value = (cond do
               is_binary(value0) ->
                 value0
               true ->
                 :erlang.iolist_to_binary(value0)
             end)
    header = {name, value}
    case (table_find(header, state)) do
      {:field, index} ->
        encode(tail, state, huffmanOpt,
                 [enc_int7(index, 1) | acc])
      {:name, index} ->
        state2 = table_insert(header, state)
        encode(tail, state2, huffmanOpt,
                 [[enc_int6(index, 1) | enc_str(value, huffmanOpt)] |
                      acc])
      :not_found ->
        state2 = table_insert(header, state)
        encode(tail, state2, huffmanOpt,
                 [[<<0 :: size(1), 1 :: size(1), 0 :: size(6)>>,
                       enc_str(name, huffmanOpt) | enc_str(value,
                                                             huffmanOpt)] |
                      acc])
    end
  end

  defp enc_int5(int, prefix) when int < 31 do
    <<prefix :: size(3), int :: size(5)>>
  end

  defp enc_int5(int, prefix) do
    enc_big_int(int - 31,
                  <<prefix :: size(3), 31 :: size(5)>>)
  end

  defp enc_int6(int, prefix) when int < 63 do
    <<prefix :: size(2), int :: size(6)>>
  end

  defp enc_int6(int, prefix) do
    enc_big_int(int - 63,
                  <<prefix :: size(2), 63 :: size(6)>>)
  end

  defp enc_int7(int, prefix) when int < 127 do
    <<prefix :: size(1), int :: size(7)>>
  end

  defp enc_int7(int, prefix) do
    enc_big_int(int - 127,
                  <<prefix :: size(1), 127 :: size(7)>>)
  end

  defp enc_big_int(int, acc) when int < 128 do
    <<acc :: binary, int :: size(8)>>
  end

  defp enc_big_int(int, acc) do
    enc_big_int(int >>> 7,
                  <<acc :: binary, 1 :: size(1), int :: size(7)>>)
  end

  defp enc_str(str, :huffman) do
    str2 = enc_huffman(str, <<>>)
    [enc_int7(byte_size(str2), 1) | str2]
  end

  defp enc_str(str, :no_huffman) do
    [enc_int7(byte_size(str), 0) | str]
  end

  defp enc_huffman(<<>>, acc) do
    case (rem(bit_size(acc), 8)) do
      1 ->
        <<acc :: bits, 127 :: size(7)>>
      2 ->
        <<acc :: bits, 63 :: size(6)>>
      3 ->
        <<acc :: bits, 31 :: size(5)>>
      4 ->
        <<acc :: bits, 15 :: size(4)>>
      5 ->
        <<acc :: bits, 7 :: size(3)>>
      6 ->
        <<acc :: bits, 3 :: size(2)>>
      7 ->
        <<acc :: bits, 1 :: size(1)>>
      0 ->
        acc
    end
  end

  defp enc_huffman(<<0, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 8184 :: size(13)>>)
  end

  defp enc_huffman(<<1, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 8388568 :: size(23)>>)
  end

  defp enc_huffman(<<2, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 268435426 :: size(28)>>)
  end

  defp enc_huffman(<<3, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 268435427 :: size(28)>>)
  end

  defp enc_huffman(<<4, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 268435428 :: size(28)>>)
  end

  defp enc_huffman(<<5, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 268435429 :: size(28)>>)
  end

  defp enc_huffman(<<6, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 268435430 :: size(28)>>)
  end

  defp enc_huffman(<<7, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 268435431 :: size(28)>>)
  end

  defp enc_huffman(<<8, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 268435432 :: size(28)>>)
  end

  defp enc_huffman(<<9, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 16777194 :: size(24)>>)
  end

  defp enc_huffman(<<10, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 1073741820 :: size(30)>>)
  end

  defp enc_huffman(<<11, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 268435433 :: size(28)>>)
  end

  defp enc_huffman(<<12, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 268435434 :: size(28)>>)
  end

  defp enc_huffman(<<13, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 1073741821 :: size(30)>>)
  end

  defp enc_huffman(<<14, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 268435435 :: size(28)>>)
  end

  defp enc_huffman(<<15, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 268435436 :: size(28)>>)
  end

  defp enc_huffman(<<16, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 268435437 :: size(28)>>)
  end

  defp enc_huffman(<<17, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 268435438 :: size(28)>>)
  end

  defp enc_huffman(<<18, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 268435439 :: size(28)>>)
  end

  defp enc_huffman(<<19, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 268435440 :: size(28)>>)
  end

  defp enc_huffman(<<20, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 268435441 :: size(28)>>)
  end

  defp enc_huffman(<<21, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 268435442 :: size(28)>>)
  end

  defp enc_huffman(<<22, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 1073741822 :: size(30)>>)
  end

  defp enc_huffman(<<23, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 268435443 :: size(28)>>)
  end

  defp enc_huffman(<<24, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 268435444 :: size(28)>>)
  end

  defp enc_huffman(<<25, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 268435445 :: size(28)>>)
  end

  defp enc_huffman(<<26, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 268435446 :: size(28)>>)
  end

  defp enc_huffman(<<27, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 268435447 :: size(28)>>)
  end

  defp enc_huffman(<<28, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 268435448 :: size(28)>>)
  end

  defp enc_huffman(<<29, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 268435449 :: size(28)>>)
  end

  defp enc_huffman(<<30, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 268435450 :: size(28)>>)
  end

  defp enc_huffman(<<31, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 268435451 :: size(28)>>)
  end

  defp enc_huffman(<<32, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 20 :: size(6)>>)
  end

  defp enc_huffman(<<33, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 1016 :: size(10)>>)
  end

  defp enc_huffman(<<34, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 1017 :: size(10)>>)
  end

  defp enc_huffman(<<35, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 4090 :: size(12)>>)
  end

  defp enc_huffman(<<36, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 8185 :: size(13)>>)
  end

  defp enc_huffman(<<37, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 21 :: size(6)>>)
  end

  defp enc_huffman(<<38, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 248 :: size(8)>>)
  end

  defp enc_huffman(<<39, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 2042 :: size(11)>>)
  end

  defp enc_huffman(<<40, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 1018 :: size(10)>>)
  end

  defp enc_huffman(<<41, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 1019 :: size(10)>>)
  end

  defp enc_huffman(<<42, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 249 :: size(8)>>)
  end

  defp enc_huffman(<<43, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 2043 :: size(11)>>)
  end

  defp enc_huffman(<<44, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 250 :: size(8)>>)
  end

  defp enc_huffman(<<45, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 22 :: size(6)>>)
  end

  defp enc_huffman(<<46, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 23 :: size(6)>>)
  end

  defp enc_huffman(<<47, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 24 :: size(6)>>)
  end

  defp enc_huffman(<<48, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 0 :: size(5)>>)
  end

  defp enc_huffman(<<49, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 1 :: size(5)>>)
  end

  defp enc_huffman(<<50, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 2 :: size(5)>>)
  end

  defp enc_huffman(<<51, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 25 :: size(6)>>)
  end

  defp enc_huffman(<<52, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 26 :: size(6)>>)
  end

  defp enc_huffman(<<53, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 27 :: size(6)>>)
  end

  defp enc_huffman(<<54, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 28 :: size(6)>>)
  end

  defp enc_huffman(<<55, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 29 :: size(6)>>)
  end

  defp enc_huffman(<<56, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 30 :: size(6)>>)
  end

  defp enc_huffman(<<57, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 31 :: size(6)>>)
  end

  defp enc_huffman(<<58, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 92 :: size(7)>>)
  end

  defp enc_huffman(<<59, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 251 :: size(8)>>)
  end

  defp enc_huffman(<<60, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 32764 :: size(15)>>)
  end

  defp enc_huffman(<<61, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 32 :: size(6)>>)
  end

  defp enc_huffman(<<62, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 4091 :: size(12)>>)
  end

  defp enc_huffman(<<63, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 1020 :: size(10)>>)
  end

  defp enc_huffman(<<64, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 8186 :: size(13)>>)
  end

  defp enc_huffman(<<65, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 33 :: size(6)>>)
  end

  defp enc_huffman(<<66, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 93 :: size(7)>>)
  end

  defp enc_huffman(<<67, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 94 :: size(7)>>)
  end

  defp enc_huffman(<<68, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 95 :: size(7)>>)
  end

  defp enc_huffman(<<69, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 96 :: size(7)>>)
  end

  defp enc_huffman(<<70, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 97 :: size(7)>>)
  end

  defp enc_huffman(<<71, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 98 :: size(7)>>)
  end

  defp enc_huffman(<<72, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 99 :: size(7)>>)
  end

  defp enc_huffman(<<73, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 100 :: size(7)>>)
  end

  defp enc_huffman(<<74, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 101 :: size(7)>>)
  end

  defp enc_huffman(<<75, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 102 :: size(7)>>)
  end

  defp enc_huffman(<<76, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 103 :: size(7)>>)
  end

  defp enc_huffman(<<77, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 104 :: size(7)>>)
  end

  defp enc_huffman(<<78, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 105 :: size(7)>>)
  end

  defp enc_huffman(<<79, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 106 :: size(7)>>)
  end

  defp enc_huffman(<<80, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 107 :: size(7)>>)
  end

  defp enc_huffman(<<81, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 108 :: size(7)>>)
  end

  defp enc_huffman(<<82, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 109 :: size(7)>>)
  end

  defp enc_huffman(<<83, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 110 :: size(7)>>)
  end

  defp enc_huffman(<<84, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 111 :: size(7)>>)
  end

  defp enc_huffman(<<85, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 112 :: size(7)>>)
  end

  defp enc_huffman(<<86, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 113 :: size(7)>>)
  end

  defp enc_huffman(<<87, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 114 :: size(7)>>)
  end

  defp enc_huffman(<<88, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 252 :: size(8)>>)
  end

  defp enc_huffman(<<89, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 115 :: size(7)>>)
  end

  defp enc_huffman(<<90, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 253 :: size(8)>>)
  end

  defp enc_huffman(<<91, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 8187 :: size(13)>>)
  end

  defp enc_huffman(<<92, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 524272 :: size(19)>>)
  end

  defp enc_huffman(<<93, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 8188 :: size(13)>>)
  end

  defp enc_huffman(<<94, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 16380 :: size(14)>>)
  end

  defp enc_huffman(<<95, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 34 :: size(6)>>)
  end

  defp enc_huffman(<<96, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 32765 :: size(15)>>)
  end

  defp enc_huffman(<<97, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 3 :: size(5)>>)
  end

  defp enc_huffman(<<98, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 35 :: size(6)>>)
  end

  defp enc_huffman(<<99, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 4 :: size(5)>>)
  end

  defp enc_huffman(<<100, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 36 :: size(6)>>)
  end

  defp enc_huffman(<<101, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 5 :: size(5)>>)
  end

  defp enc_huffman(<<102, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 37 :: size(6)>>)
  end

  defp enc_huffman(<<103, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 38 :: size(6)>>)
  end

  defp enc_huffman(<<104, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 39 :: size(6)>>)
  end

  defp enc_huffman(<<105, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 6 :: size(5)>>)
  end

  defp enc_huffman(<<106, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 116 :: size(7)>>)
  end

  defp enc_huffman(<<107, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 117 :: size(7)>>)
  end

  defp enc_huffman(<<108, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 40 :: size(6)>>)
  end

  defp enc_huffman(<<109, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 41 :: size(6)>>)
  end

  defp enc_huffman(<<110, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 42 :: size(6)>>)
  end

  defp enc_huffman(<<111, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 7 :: size(5)>>)
  end

  defp enc_huffman(<<112, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 43 :: size(6)>>)
  end

  defp enc_huffman(<<113, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 118 :: size(7)>>)
  end

  defp enc_huffman(<<114, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 44 :: size(6)>>)
  end

  defp enc_huffman(<<115, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 8 :: size(5)>>)
  end

  defp enc_huffman(<<116, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 9 :: size(5)>>)
  end

  defp enc_huffman(<<117, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 45 :: size(6)>>)
  end

  defp enc_huffman(<<118, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 119 :: size(7)>>)
  end

  defp enc_huffman(<<119, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 120 :: size(7)>>)
  end

  defp enc_huffman(<<120, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 121 :: size(7)>>)
  end

  defp enc_huffman(<<121, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 122 :: size(7)>>)
  end

  defp enc_huffman(<<122, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 123 :: size(7)>>)
  end

  defp enc_huffman(<<123, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 32766 :: size(15)>>)
  end

  defp enc_huffman(<<124, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 2044 :: size(11)>>)
  end

  defp enc_huffman(<<125, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 16381 :: size(14)>>)
  end

  defp enc_huffman(<<126, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 8189 :: size(13)>>)
  end

  defp enc_huffman(<<127, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 268435452 :: size(28)>>)
  end

  defp enc_huffman(<<128, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 1048550 :: size(20)>>)
  end

  defp enc_huffman(<<129, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 4194258 :: size(22)>>)
  end

  defp enc_huffman(<<130, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 1048551 :: size(20)>>)
  end

  defp enc_huffman(<<131, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 1048552 :: size(20)>>)
  end

  defp enc_huffman(<<132, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 4194259 :: size(22)>>)
  end

  defp enc_huffman(<<133, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 4194260 :: size(22)>>)
  end

  defp enc_huffman(<<134, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 4194261 :: size(22)>>)
  end

  defp enc_huffman(<<135, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 8388569 :: size(23)>>)
  end

  defp enc_huffman(<<136, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 4194262 :: size(22)>>)
  end

  defp enc_huffman(<<137, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 8388570 :: size(23)>>)
  end

  defp enc_huffman(<<138, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 8388571 :: size(23)>>)
  end

  defp enc_huffman(<<139, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 8388572 :: size(23)>>)
  end

  defp enc_huffman(<<140, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 8388573 :: size(23)>>)
  end

  defp enc_huffman(<<141, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 8388574 :: size(23)>>)
  end

  defp enc_huffman(<<142, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 16777195 :: size(24)>>)
  end

  defp enc_huffman(<<143, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 8388575 :: size(23)>>)
  end

  defp enc_huffman(<<144, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 16777196 :: size(24)>>)
  end

  defp enc_huffman(<<145, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 16777197 :: size(24)>>)
  end

  defp enc_huffman(<<146, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 4194263 :: size(22)>>)
  end

  defp enc_huffman(<<147, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 8388576 :: size(23)>>)
  end

  defp enc_huffman(<<148, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 16777198 :: size(24)>>)
  end

  defp enc_huffman(<<149, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 8388577 :: size(23)>>)
  end

  defp enc_huffman(<<150, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 8388578 :: size(23)>>)
  end

  defp enc_huffman(<<151, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 8388579 :: size(23)>>)
  end

  defp enc_huffman(<<152, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 8388580 :: size(23)>>)
  end

  defp enc_huffman(<<153, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 2097116 :: size(21)>>)
  end

  defp enc_huffman(<<154, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 4194264 :: size(22)>>)
  end

  defp enc_huffman(<<155, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 8388581 :: size(23)>>)
  end

  defp enc_huffman(<<156, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 4194265 :: size(22)>>)
  end

  defp enc_huffman(<<157, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 8388582 :: size(23)>>)
  end

  defp enc_huffman(<<158, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 8388583 :: size(23)>>)
  end

  defp enc_huffman(<<159, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 16777199 :: size(24)>>)
  end

  defp enc_huffman(<<160, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 4194266 :: size(22)>>)
  end

  defp enc_huffman(<<161, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 2097117 :: size(21)>>)
  end

  defp enc_huffman(<<162, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 1048553 :: size(20)>>)
  end

  defp enc_huffman(<<163, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 4194267 :: size(22)>>)
  end

  defp enc_huffman(<<164, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 4194268 :: size(22)>>)
  end

  defp enc_huffman(<<165, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 8388584 :: size(23)>>)
  end

  defp enc_huffman(<<166, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 8388585 :: size(23)>>)
  end

  defp enc_huffman(<<167, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 2097118 :: size(21)>>)
  end

  defp enc_huffman(<<168, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 8388586 :: size(23)>>)
  end

  defp enc_huffman(<<169, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 4194269 :: size(22)>>)
  end

  defp enc_huffman(<<170, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 4194270 :: size(22)>>)
  end

  defp enc_huffman(<<171, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 16777200 :: size(24)>>)
  end

  defp enc_huffman(<<172, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 2097119 :: size(21)>>)
  end

  defp enc_huffman(<<173, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 4194271 :: size(22)>>)
  end

  defp enc_huffman(<<174, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 8388587 :: size(23)>>)
  end

  defp enc_huffman(<<175, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 8388588 :: size(23)>>)
  end

  defp enc_huffman(<<176, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 2097120 :: size(21)>>)
  end

  defp enc_huffman(<<177, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 2097121 :: size(21)>>)
  end

  defp enc_huffman(<<178, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 4194272 :: size(22)>>)
  end

  defp enc_huffman(<<179, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 2097122 :: size(21)>>)
  end

  defp enc_huffman(<<180, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 8388589 :: size(23)>>)
  end

  defp enc_huffman(<<181, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 4194273 :: size(22)>>)
  end

  defp enc_huffman(<<182, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 8388590 :: size(23)>>)
  end

  defp enc_huffman(<<183, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 8388591 :: size(23)>>)
  end

  defp enc_huffman(<<184, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 1048554 :: size(20)>>)
  end

  defp enc_huffman(<<185, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 4194274 :: size(22)>>)
  end

  defp enc_huffman(<<186, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 4194275 :: size(22)>>)
  end

  defp enc_huffman(<<187, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 4194276 :: size(22)>>)
  end

  defp enc_huffman(<<188, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 8388592 :: size(23)>>)
  end

  defp enc_huffman(<<189, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 4194277 :: size(22)>>)
  end

  defp enc_huffman(<<190, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 4194278 :: size(22)>>)
  end

  defp enc_huffman(<<191, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 8388593 :: size(23)>>)
  end

  defp enc_huffman(<<192, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 67108832 :: size(26)>>)
  end

  defp enc_huffman(<<193, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 67108833 :: size(26)>>)
  end

  defp enc_huffman(<<194, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 1048555 :: size(20)>>)
  end

  defp enc_huffman(<<195, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 524273 :: size(19)>>)
  end

  defp enc_huffman(<<196, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 4194279 :: size(22)>>)
  end

  defp enc_huffman(<<197, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 8388594 :: size(23)>>)
  end

  defp enc_huffman(<<198, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 4194280 :: size(22)>>)
  end

  defp enc_huffman(<<199, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 33554412 :: size(25)>>)
  end

  defp enc_huffman(<<200, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 67108834 :: size(26)>>)
  end

  defp enc_huffman(<<201, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 67108835 :: size(26)>>)
  end

  defp enc_huffman(<<202, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 67108836 :: size(26)>>)
  end

  defp enc_huffman(<<203, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 134217694 :: size(27)>>)
  end

  defp enc_huffman(<<204, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 134217695 :: size(27)>>)
  end

  defp enc_huffman(<<205, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 67108837 :: size(26)>>)
  end

  defp enc_huffman(<<206, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 16777201 :: size(24)>>)
  end

  defp enc_huffman(<<207, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 33554413 :: size(25)>>)
  end

  defp enc_huffman(<<208, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 524274 :: size(19)>>)
  end

  defp enc_huffman(<<209, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 2097123 :: size(21)>>)
  end

  defp enc_huffman(<<210, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 67108838 :: size(26)>>)
  end

  defp enc_huffman(<<211, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 134217696 :: size(27)>>)
  end

  defp enc_huffman(<<212, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 134217697 :: size(27)>>)
  end

  defp enc_huffman(<<213, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 67108839 :: size(26)>>)
  end

  defp enc_huffman(<<214, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 134217698 :: size(27)>>)
  end

  defp enc_huffman(<<215, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 16777202 :: size(24)>>)
  end

  defp enc_huffman(<<216, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 2097124 :: size(21)>>)
  end

  defp enc_huffman(<<217, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 2097125 :: size(21)>>)
  end

  defp enc_huffman(<<218, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 67108840 :: size(26)>>)
  end

  defp enc_huffman(<<219, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 67108841 :: size(26)>>)
  end

  defp enc_huffman(<<220, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 268435453 :: size(28)>>)
  end

  defp enc_huffman(<<221, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 134217699 :: size(27)>>)
  end

  defp enc_huffman(<<222, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 134217700 :: size(27)>>)
  end

  defp enc_huffman(<<223, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 134217701 :: size(27)>>)
  end

  defp enc_huffman(<<224, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 1048556 :: size(20)>>)
  end

  defp enc_huffman(<<225, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 16777203 :: size(24)>>)
  end

  defp enc_huffman(<<226, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 1048557 :: size(20)>>)
  end

  defp enc_huffman(<<227, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 2097126 :: size(21)>>)
  end

  defp enc_huffman(<<228, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 4194281 :: size(22)>>)
  end

  defp enc_huffman(<<229, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 2097127 :: size(21)>>)
  end

  defp enc_huffman(<<230, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 2097128 :: size(21)>>)
  end

  defp enc_huffman(<<231, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 8388595 :: size(23)>>)
  end

  defp enc_huffman(<<232, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 4194282 :: size(22)>>)
  end

  defp enc_huffman(<<233, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 4194283 :: size(22)>>)
  end

  defp enc_huffman(<<234, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 33554414 :: size(25)>>)
  end

  defp enc_huffman(<<235, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 33554415 :: size(25)>>)
  end

  defp enc_huffman(<<236, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 16777204 :: size(24)>>)
  end

  defp enc_huffman(<<237, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 16777205 :: size(24)>>)
  end

  defp enc_huffman(<<238, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 67108842 :: size(26)>>)
  end

  defp enc_huffman(<<239, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 8388596 :: size(23)>>)
  end

  defp enc_huffman(<<240, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 67108843 :: size(26)>>)
  end

  defp enc_huffman(<<241, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 134217702 :: size(27)>>)
  end

  defp enc_huffman(<<242, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 67108844 :: size(26)>>)
  end

  defp enc_huffman(<<243, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 67108845 :: size(26)>>)
  end

  defp enc_huffman(<<244, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 134217703 :: size(27)>>)
  end

  defp enc_huffman(<<245, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 134217704 :: size(27)>>)
  end

  defp enc_huffman(<<246, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 134217705 :: size(27)>>)
  end

  defp enc_huffman(<<247, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 134217706 :: size(27)>>)
  end

  defp enc_huffman(<<248, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 134217707 :: size(27)>>)
  end

  defp enc_huffman(<<249, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 268435454 :: size(28)>>)
  end

  defp enc_huffman(<<250, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 134217708 :: size(27)>>)
  end

  defp enc_huffman(<<251, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 134217709 :: size(27)>>)
  end

  defp enc_huffman(<<252, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 134217710 :: size(27)>>)
  end

  defp enc_huffman(<<253, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 134217711 :: size(27)>>)
  end

  defp enc_huffman(<<254, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 134217712 :: size(27)>>)
  end

  defp enc_huffman(<<255, r :: bits>>, a) do
    enc_huffman(r, <<a :: bits, 67108846 :: size(26)>>)
  end

  defp table_find(header = {name, _}, state) do
    case (table_find_field(header, state)) do
      :not_found ->
        case (table_find_name(name, state)) do
          notFound = :not_found ->
            notFound
          found ->
            {:name, found}
        end
      found ->
        {:field, found}
    end
  end

  defp table_find_field({":authority", <<>>}, _) do
    1
  end

  defp table_find_field({":method", "GET"}, _) do
    2
  end

  defp table_find_field({":method", "POST"}, _) do
    3
  end

  defp table_find_field({":path", "/"}, _) do
    4
  end

  defp table_find_field({":path", "/index.html"}, _) do
    5
  end

  defp table_find_field({":scheme", "http"}, _) do
    6
  end

  defp table_find_field({":scheme", "https"}, _) do
    7
  end

  defp table_find_field({":status", "200"}, _) do
    8
  end

  defp table_find_field({":status", "204"}, _) do
    9
  end

  defp table_find_field({":status", "206"}, _) do
    10
  end

  defp table_find_field({":status", "304"}, _) do
    11
  end

  defp table_find_field({":status", "400"}, _) do
    12
  end

  defp table_find_field({":status", "404"}, _) do
    13
  end

  defp table_find_field({":status", "500"}, _) do
    14
  end

  defp table_find_field({"accept-charset", <<>>}, _) do
    15
  end

  defp table_find_field({"accept-encoding", "gzip, deflate"}, _) do
    16
  end

  defp table_find_field({"accept-language", <<>>}, _) do
    17
  end

  defp table_find_field({"accept-ranges", <<>>}, _) do
    18
  end

  defp table_find_field({"accept", <<>>}, _) do
    19
  end

  defp table_find_field({"access-control-allow-origin", <<>>}, _) do
    20
  end

  defp table_find_field({"age", <<>>}, _) do
    21
  end

  defp table_find_field({"allow", <<>>}, _) do
    22
  end

  defp table_find_field({"authorization", <<>>}, _) do
    23
  end

  defp table_find_field({"cache-control", <<>>}, _) do
    24
  end

  defp table_find_field({"content-disposition", <<>>}, _) do
    25
  end

  defp table_find_field({"content-encoding", <<>>}, _) do
    26
  end

  defp table_find_field({"content-language", <<>>}, _) do
    27
  end

  defp table_find_field({"content-length", <<>>}, _) do
    28
  end

  defp table_find_field({"content-location", <<>>}, _) do
    29
  end

  defp table_find_field({"content-range", <<>>}, _) do
    30
  end

  defp table_find_field({"content-type", <<>>}, _) do
    31
  end

  defp table_find_field({"cookie", <<>>}, _) do
    32
  end

  defp table_find_field({"date", <<>>}, _) do
    33
  end

  defp table_find_field({"etag", <<>>}, _) do
    34
  end

  defp table_find_field({"expect", <<>>}, _) do
    35
  end

  defp table_find_field({"expires", <<>>}, _) do
    36
  end

  defp table_find_field({"from", <<>>}, _) do
    37
  end

  defp table_find_field({"host", <<>>}, _) do
    38
  end

  defp table_find_field({"if-match", <<>>}, _) do
    39
  end

  defp table_find_field({"if-modified-since", <<>>}, _) do
    40
  end

  defp table_find_field({"if-none-match", <<>>}, _) do
    41
  end

  defp table_find_field({"if-range", <<>>}, _) do
    42
  end

  defp table_find_field({"if-unmodified-since", <<>>}, _) do
    43
  end

  defp table_find_field({"last-modified", <<>>}, _) do
    44
  end

  defp table_find_field({"link", <<>>}, _) do
    45
  end

  defp table_find_field({"location", <<>>}, _) do
    46
  end

  defp table_find_field({"max-forwards", <<>>}, _) do
    47
  end

  defp table_find_field({"proxy-authenticate", <<>>}, _) do
    48
  end

  defp table_find_field({"proxy-authorization", <<>>}, _) do
    49
  end

  defp table_find_field({"range", <<>>}, _) do
    50
  end

  defp table_find_field({"referer", <<>>}, _) do
    51
  end

  defp table_find_field({"refresh", <<>>}, _) do
    52
  end

  defp table_find_field({"retry-after", <<>>}, _) do
    53
  end

  defp table_find_field({"server", <<>>}, _) do
    54
  end

  defp table_find_field({"set-cookie", <<>>}, _) do
    55
  end

  defp table_find_field({"strict-transport-security", <<>>}, _) do
    56
  end

  defp table_find_field({"transfer-encoding", <<>>}, _) do
    57
  end

  defp table_find_field({"user-agent", <<>>}, _) do
    58
  end

  defp table_find_field({"vary", <<>>}, _) do
    59
  end

  defp table_find_field({"via", <<>>}, _) do
    60
  end

  defp table_find_field({"www-authenticate", <<>>}, _) do
    61
  end

  defp table_find_field(header, r_state(dyn_table: dynamicTable)) do
    table_find_field_dyn(header, dynamicTable, 62)
  end

  defp table_find_field_dyn(_, [], _) do
    :not_found
  end

  defp table_find_field_dyn(header, [{_, header} | _], index) do
    index
  end

  defp table_find_field_dyn(header, [_ | tail], index) do
    table_find_field_dyn(header, tail, index + 1)
  end

  defp table_find_name(":authority", _) do
    1
  end

  defp table_find_name(":method", _) do
    2
  end

  defp table_find_name(":path", _) do
    4
  end

  defp table_find_name(":scheme", _) do
    6
  end

  defp table_find_name(":status", _) do
    8
  end

  defp table_find_name("accept-charset", _) do
    15
  end

  defp table_find_name("accept-encoding", _) do
    16
  end

  defp table_find_name("accept-language", _) do
    17
  end

  defp table_find_name("accept-ranges", _) do
    18
  end

  defp table_find_name("accept", _) do
    19
  end

  defp table_find_name("access-control-allow-origin", _) do
    20
  end

  defp table_find_name("age", _) do
    21
  end

  defp table_find_name("allow", _) do
    22
  end

  defp table_find_name("authorization", _) do
    23
  end

  defp table_find_name("cache-control", _) do
    24
  end

  defp table_find_name("content-disposition", _) do
    25
  end

  defp table_find_name("content-encoding", _) do
    26
  end

  defp table_find_name("content-language", _) do
    27
  end

  defp table_find_name("content-length", _) do
    28
  end

  defp table_find_name("content-location", _) do
    29
  end

  defp table_find_name("content-range", _) do
    30
  end

  defp table_find_name("content-type", _) do
    31
  end

  defp table_find_name("cookie", _) do
    32
  end

  defp table_find_name("date", _) do
    33
  end

  defp table_find_name("etag", _) do
    34
  end

  defp table_find_name("expect", _) do
    35
  end

  defp table_find_name("expires", _) do
    36
  end

  defp table_find_name("from", _) do
    37
  end

  defp table_find_name("host", _) do
    38
  end

  defp table_find_name("if-match", _) do
    39
  end

  defp table_find_name("if-modified-since", _) do
    40
  end

  defp table_find_name("if-none-match", _) do
    41
  end

  defp table_find_name("if-range", _) do
    42
  end

  defp table_find_name("if-unmodified-since", _) do
    43
  end

  defp table_find_name("last-modified", _) do
    44
  end

  defp table_find_name("link", _) do
    45
  end

  defp table_find_name("location", _) do
    46
  end

  defp table_find_name("max-forwards", _) do
    47
  end

  defp table_find_name("proxy-authenticate", _) do
    48
  end

  defp table_find_name("proxy-authorization", _) do
    49
  end

  defp table_find_name("range", _) do
    50
  end

  defp table_find_name("referer", _) do
    51
  end

  defp table_find_name("refresh", _) do
    52
  end

  defp table_find_name("retry-after", _) do
    53
  end

  defp table_find_name("server", _) do
    54
  end

  defp table_find_name("set-cookie", _) do
    55
  end

  defp table_find_name("strict-transport-security", _) do
    56
  end

  defp table_find_name("transfer-encoding", _) do
    57
  end

  defp table_find_name("user-agent", _) do
    58
  end

  defp table_find_name("vary", _) do
    59
  end

  defp table_find_name("via", _) do
    60
  end

  defp table_find_name("www-authenticate", _) do
    61
  end

  defp table_find_name(name, r_state(dyn_table: dynamicTable)) do
    table_find_name_dyn(name, dynamicTable, 62)
  end

  defp table_find_name_dyn(_, [], _) do
    :not_found
  end

  defp table_find_name_dyn(name, [{name, _} | _], index) do
    index
  end

  defp table_find_name_dyn(name, [_ | tail], index) do
    table_find_name_dyn(name, tail, index + 1)
  end

  defp table_get(1, _) do
    {":authority", <<>>}
  end

  defp table_get(2, _) do
    {":method", "GET"}
  end

  defp table_get(3, _) do
    {":method", "POST"}
  end

  defp table_get(4, _) do
    {":path", "/"}
  end

  defp table_get(5, _) do
    {":path", "/index.html"}
  end

  defp table_get(6, _) do
    {":scheme", "http"}
  end

  defp table_get(7, _) do
    {":scheme", "https"}
  end

  defp table_get(8, _) do
    {":status", "200"}
  end

  defp table_get(9, _) do
    {":status", "204"}
  end

  defp table_get(10, _) do
    {":status", "206"}
  end

  defp table_get(11, _) do
    {":status", "304"}
  end

  defp table_get(12, _) do
    {":status", "400"}
  end

  defp table_get(13, _) do
    {":status", "404"}
  end

  defp table_get(14, _) do
    {":status", "500"}
  end

  defp table_get(15, _) do
    {"accept-charset", <<>>}
  end

  defp table_get(16, _) do
    {"accept-encoding", "gzip, deflate"}
  end

  defp table_get(17, _) do
    {"accept-language", <<>>}
  end

  defp table_get(18, _) do
    {"accept-ranges", <<>>}
  end

  defp table_get(19, _) do
    {"accept", <<>>}
  end

  defp table_get(20, _) do
    {"access-control-allow-origin", <<>>}
  end

  defp table_get(21, _) do
    {"age", <<>>}
  end

  defp table_get(22, _) do
    {"allow", <<>>}
  end

  defp table_get(23, _) do
    {"authorization", <<>>}
  end

  defp table_get(24, _) do
    {"cache-control", <<>>}
  end

  defp table_get(25, _) do
    {"content-disposition", <<>>}
  end

  defp table_get(26, _) do
    {"content-encoding", <<>>}
  end

  defp table_get(27, _) do
    {"content-language", <<>>}
  end

  defp table_get(28, _) do
    {"content-length", <<>>}
  end

  defp table_get(29, _) do
    {"content-location", <<>>}
  end

  defp table_get(30, _) do
    {"content-range", <<>>}
  end

  defp table_get(31, _) do
    {"content-type", <<>>}
  end

  defp table_get(32, _) do
    {"cookie", <<>>}
  end

  defp table_get(33, _) do
    {"date", <<>>}
  end

  defp table_get(34, _) do
    {"etag", <<>>}
  end

  defp table_get(35, _) do
    {"expect", <<>>}
  end

  defp table_get(36, _) do
    {"expires", <<>>}
  end

  defp table_get(37, _) do
    {"from", <<>>}
  end

  defp table_get(38, _) do
    {"host", <<>>}
  end

  defp table_get(39, _) do
    {"if-match", <<>>}
  end

  defp table_get(40, _) do
    {"if-modified-since", <<>>}
  end

  defp table_get(41, _) do
    {"if-none-match", <<>>}
  end

  defp table_get(42, _) do
    {"if-range", <<>>}
  end

  defp table_get(43, _) do
    {"if-unmodified-since", <<>>}
  end

  defp table_get(44, _) do
    {"last-modified", <<>>}
  end

  defp table_get(45, _) do
    {"link", <<>>}
  end

  defp table_get(46, _) do
    {"location", <<>>}
  end

  defp table_get(47, _) do
    {"max-forwards", <<>>}
  end

  defp table_get(48, _) do
    {"proxy-authenticate", <<>>}
  end

  defp table_get(49, _) do
    {"proxy-authorization", <<>>}
  end

  defp table_get(50, _) do
    {"range", <<>>}
  end

  defp table_get(51, _) do
    {"referer", <<>>}
  end

  defp table_get(52, _) do
    {"refresh", <<>>}
  end

  defp table_get(53, _) do
    {"retry-after", <<>>}
  end

  defp table_get(54, _) do
    {"server", <<>>}
  end

  defp table_get(55, _) do
    {"set-cookie", <<>>}
  end

  defp table_get(56, _) do
    {"strict-transport-security", <<>>}
  end

  defp table_get(57, _) do
    {"transfer-encoding", <<>>}
  end

  defp table_get(58, _) do
    {"user-agent", <<>>}
  end

  defp table_get(59, _) do
    {"vary", <<>>}
  end

  defp table_get(60, _) do
    {"via", <<>>}
  end

  defp table_get(61, _) do
    {"www-authenticate", <<>>}
  end

  defp table_get(index, r_state(dyn_table: dynamicTable)) do
    {_, header} = :lists.nth(index - 61, dynamicTable)
    header
  end

  defp table_get_name(1, _) do
    ":authority"
  end

  defp table_get_name(2, _) do
    ":method"
  end

  defp table_get_name(3, _) do
    ":method"
  end

  defp table_get_name(4, _) do
    ":path"
  end

  defp table_get_name(5, _) do
    ":path"
  end

  defp table_get_name(6, _) do
    ":scheme"
  end

  defp table_get_name(7, _) do
    ":scheme"
  end

  defp table_get_name(8, _) do
    ":status"
  end

  defp table_get_name(9, _) do
    ":status"
  end

  defp table_get_name(10, _) do
    ":status"
  end

  defp table_get_name(11, _) do
    ":status"
  end

  defp table_get_name(12, _) do
    ":status"
  end

  defp table_get_name(13, _) do
    ":status"
  end

  defp table_get_name(14, _) do
    ":status"
  end

  defp table_get_name(15, _) do
    "accept-charset"
  end

  defp table_get_name(16, _) do
    "accept-encoding"
  end

  defp table_get_name(17, _) do
    "accept-language"
  end

  defp table_get_name(18, _) do
    "accept-ranges"
  end

  defp table_get_name(19, _) do
    "accept"
  end

  defp table_get_name(20, _) do
    "access-control-allow-origin"
  end

  defp table_get_name(21, _) do
    "age"
  end

  defp table_get_name(22, _) do
    "allow"
  end

  defp table_get_name(23, _) do
    "authorization"
  end

  defp table_get_name(24, _) do
    "cache-control"
  end

  defp table_get_name(25, _) do
    "content-disposition"
  end

  defp table_get_name(26, _) do
    "content-encoding"
  end

  defp table_get_name(27, _) do
    "content-language"
  end

  defp table_get_name(28, _) do
    "content-length"
  end

  defp table_get_name(29, _) do
    "content-location"
  end

  defp table_get_name(30, _) do
    "content-range"
  end

  defp table_get_name(31, _) do
    "content-type"
  end

  defp table_get_name(32, _) do
    "cookie"
  end

  defp table_get_name(33, _) do
    "date"
  end

  defp table_get_name(34, _) do
    "etag"
  end

  defp table_get_name(35, _) do
    "expect"
  end

  defp table_get_name(36, _) do
    "expires"
  end

  defp table_get_name(37, _) do
    "from"
  end

  defp table_get_name(38, _) do
    "host"
  end

  defp table_get_name(39, _) do
    "if-match"
  end

  defp table_get_name(40, _) do
    "if-modified-since"
  end

  defp table_get_name(41, _) do
    "if-none-match"
  end

  defp table_get_name(42, _) do
    "if-range"
  end

  defp table_get_name(43, _) do
    "if-unmodified-since"
  end

  defp table_get_name(44, _) do
    "last-modified"
  end

  defp table_get_name(45, _) do
    "link"
  end

  defp table_get_name(46, _) do
    "location"
  end

  defp table_get_name(47, _) do
    "max-forwards"
  end

  defp table_get_name(48, _) do
    "proxy-authenticate"
  end

  defp table_get_name(49, _) do
    "proxy-authorization"
  end

  defp table_get_name(50, _) do
    "range"
  end

  defp table_get_name(51, _) do
    "referer"
  end

  defp table_get_name(52, _) do
    "refresh"
  end

  defp table_get_name(53, _) do
    "retry-after"
  end

  defp table_get_name(54, _) do
    "server"
  end

  defp table_get_name(55, _) do
    "set-cookie"
  end

  defp table_get_name(56, _) do
    "strict-transport-security"
  end

  defp table_get_name(57, _) do
    "transfer-encoding"
  end

  defp table_get_name(58, _) do
    "user-agent"
  end

  defp table_get_name(59, _) do
    "vary"
  end

  defp table_get_name(60, _) do
    "via"
  end

  defp table_get_name(61, _) do
    "www-authenticate"
  end

  defp table_get_name(index, r_state(dyn_table: dynamicTable)) do
    {_, {name, _}} = :lists.nth(index - 61, dynamicTable)
    name
  end

  defp table_insert(entry = {name, value},
            state = r_state(size: size, max_size: maxSize,
                        dyn_table: dynamicTable)) do
    entrySize = byte_size(name) + byte_size(value) + 32
    cond do
      entrySize + size <= maxSize ->
        r_state(state, size: size + entrySize, 
                   dyn_table: [{entrySize, entry} | dynamicTable])
      entrySize <= maxSize ->
        {dynamicTable2, size2} = table_resize(dynamicTable,
                                                maxSize - entrySize, 0, [])
        r_state(state, size: size2 + entrySize, 
                   dyn_table: [{entrySize, entry} | dynamicTable2])
      entrySize > maxSize ->
        r_state(state, size: 0,  dyn_table: [])
    end
  end

  defp table_resize([], _, size, acc) do
    {:lists.reverse(acc), size}
  end

  defp table_resize([{entrySize, _} | _], maxSize, size, acc)
      when size + entrySize > maxSize do
    {:lists.reverse(acc), size}
  end

  defp table_resize([entry = {entrySize, _} | tail], maxSize, size,
            acc) do
    table_resize(tail, maxSize, size + entrySize,
                   [entry | acc])
  end

  defp table_update_size(0, state) do
    r_state(state, size: 0,  max_size: 0,  dyn_table: [])
  end

  defp table_update_size(maxSize, state = r_state(size: currentSize))
      when currentSize <= maxSize do
    r_state(state, max_size: maxSize)
  end

  defp table_update_size(maxSize, state = r_state(dyn_table: dynTable)) do
    {dynTable2, size} = table_resize(dynTable, maxSize, 0,
                                       [])
    r_state(state, size: size,  max_size: maxSize, 
               dyn_table: dynTable2)
  end

end