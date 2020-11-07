defmodule :m_asn1rtt_check do
  use Bitwise

  def check_fail(_) do
    throw(false)
  end

  def check_int(value, value, _) when is_integer(value) do
    true
  end

  def check_int(value, defValue, nNL) when is_atom(value) do
    case :lists.keyfind(value, 1, nNL) do
      {_, ^defValue} ->
        true

      _ ->
        throw(false)
    end
  end

  def check_int(_, _, _) do
    throw(false)
  end

  def check_legacy_bitstring(value, default) do
    check_bitstring(default, value)
  end

  defp check_bitstring(defVal, {unused, binary}) do
    sz = bit_size(binary) - unused
    <<val::size(sz)-bitstring, _::size(unused)>> = binary
    check_bitstring(defVal, val)
  end

  defp check_bitstring(defVal, val) when is_bitstring(val) do
    case val === defVal do
      false ->
        throw(false)

      true ->
        true
    end
  end

  defp check_bitstring(def__, val) when is_list(val) do
    check_bitstring_list(def__, val)
  end

  defp check_bitstring(def__, val) when is_integer(val) do
    check_bitstring_integer(def__, val)
  end

  defp check_bitstring_list(<<h::size(1), t1::bitstring>>, [h | t2]) do
    check_bitstring_list(t1, t2)
  end

  defp check_bitstring_list(<<>>, []) do
    true
  end

  defp check_bitstring_list(_, _) do
    throw(false)
  end

  defp check_bitstring_integer(<<h::size(1), t1::bitstring>>, int)
       when h === int &&& 1 do
    check_bitstring_integer(t1, int >>> 1)
  end

  defp check_bitstring_integer(<<>>, 0) do
    true
  end

  defp check_bitstring_integer(_, _) do
    throw(false)
  end

  def check_legacy_named_bitstring([int | _] = val, bs, bsSize)
      when is_integer(int) do
    check_named_bitstring(
      for b <- val, into: <<>> do
        <<b::size(1)>>
      end,
      bs,
      bsSize
    )
  end

  def check_legacy_named_bitstring({unused, val0}, bs, bsSize) do
    sz = bit_size(val0) - unused
    <<val::size(sz)-bits, _::bits>> = val0
    check_named_bitstring(val, bs, bsSize)
  end

  def check_legacy_named_bitstring(val, bs, bsSize) when is_integer(val) do
    l = legacy_int_to_bitlist(val)

    check_named_bitstring(
      for b <- l, into: <<>> do
        <<b::size(1)>>
      end,
      bs,
      bsSize
    )
  end

  def check_legacy_named_bitstring(val, bs, bsSize) do
    check_named_bitstring(val, bs, bsSize)
  end

  def check_legacy_named_bitstring([int | _] = val, names, bs, bsSize)
      when is_integer(int) do
    check_named_bitstring(
      for b <- val, into: <<>> do
        <<b::size(1)>>
      end,
      names,
      bs,
      bsSize
    )
  end

  def check_legacy_named_bitstring({unused, val0}, names, bs, bsSize) do
    sz = bit_size(val0) - unused
    <<val::size(sz)-bits, _::bits>> = val0
    check_named_bitstring(val, names, bs, bsSize)
  end

  def check_legacy_named_bitstring(val, names, bs, bsSize) when is_integer(val) do
    l = legacy_int_to_bitlist(val)

    check_named_bitstring(
      for b <- l, into: <<>> do
        <<b::size(1)>>
      end,
      names,
      bs,
      bsSize
    )
  end

  def check_legacy_named_bitstring(val, names, bs, bsSize) do
    check_named_bitstring(val, names, bs, bsSize)
  end

  defp legacy_int_to_bitlist(0) do
    []
  end

  defp legacy_int_to_bitlist(int) do
    [int &&& 1 | legacy_int_to_bitlist(int >>> 1)]
  end

  def check_named_bitstring(bs, bs, _) do
    true
  end

  def check_named_bitstring(val, bs, bsSize) do
    rest = bit_size(val) - bsSize

    case val do
      <<^bs::size(bsSize)-bits, 0::size(rest)>> ->
        true

      _ ->
        throw(false)
    end
  end

  def check_named_bitstring([_ | _] = val, names, _, _) do
    case :lists.sort(val) do
      ^names ->
        true

      _ ->
        throw(false)
    end
  end

  def check_named_bitstring(bs, _, bs, _) do
    true
  end

  def check_named_bitstring(val, _, bs, bsSize) do
    rest = bit_size(val) - bsSize

    case val do
      <<^bs::size(bsSize)-bits, 0::size(rest)>> ->
        true

      _ ->
        throw(false)
    end
  end

  def check_octetstring(v, v) do
    true
  end

  def check_octetstring(v, def__) when is_list(v) do
    case :erlang.list_to_binary(v) do
      ^def__ ->
        true

      _ ->
        throw(false)
    end
  end

  def check_octetstring(_, _) do
    throw(false)
  end

  def check_objectidentifier(value, {prefix, tail}) when is_tuple(value) do
    check_oid(:erlang.tuple_to_list(value), prefix, tail)
  end

  def check_objectidentifier(_, _) do
    throw(false)
  end

  defp check_oid([h | t], [k | ks], tail) do
    case :lists.member(h, k) do
      false ->
        throw(false)

      true ->
        check_oid(t, ks, tail)
    end
  end

  defp check_oid(tail, [], tail) do
    true
  end

  defp check_oid(_, _, _) do
    throw(false)
  end

  def check_objectdescriptor(_, :asn1_DEFAULT) do
    true
  end

  def check_objectdescriptor(oD, oD) do
    true
  end

  def check_objectdescriptor(oD, oD) do
    throw({:error, {:not_implemented_yet, :check_objectdescriptor}})
  end

  def check_real(_, :asn1_DEFAULT) do
    true
  end

  def check_real(r, r) do
    true
  end

  def check_real(_, _) do
    throw({:error, {:not_implemented_yet, :check_real}})
  end

  def check_restrictedstring(val, val) do
    true
  end

  def check_restrictedstring([v | rest1], [v | rest2]) do
    check_restrictedstring(rest1, rest2)
  end

  def check_restrictedstring([v1 | rest1], [v2 | rest2]) do
    check_restrictedstring(v1, v2)
    check_restrictedstring(rest1, rest2)
  end

  def check_restrictedstring({v1, v2}, [v1, v2]) do
    true
  end

  def check_restrictedstring([v1, v2], {v1, v2}) do
    true
  end

  def check_restrictedstring({v1, v2, v3, v4}, [v1, v2, v3, v4]) do
    true
  end

  def check_restrictedstring([v1, v2, v3, v4], {v1, v2, v3, v4}) do
    true
  end

  def check_restrictedstring(v1, v2) when is_tuple(v1) do
    check_restrictedstring(:erlang.tuple_to_list(v1), v2)
  end

  def check_restrictedstring(_, _) do
    throw(false)
  end

  def check_literal_sof(value, default) do
    case :lists.sort(value) do
      ^default ->
        true

      _ ->
        throw(false)
    end
  end
end
