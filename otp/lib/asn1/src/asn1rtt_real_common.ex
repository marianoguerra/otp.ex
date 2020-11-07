defmodule :m_asn1rtt_real_common do
  use Bitwise

  def ber_encode_real(0) do
    {[], 0}
  end

  def ber_encode_real(:"PLUS-INFINITY") do
    {[64], 1}
  end

  def ber_encode_real(:"MINUS-INFINITY") do
    {[65], 1}
  end

  def ber_encode_real(val) when is_tuple(val) or is_list(val) do
    encode_real(val)
  end

  def encode_real(real) do
    encode_real([], real)
  end

  defp encode_real(_C, {mantissa, base, exponent})
       when base === 2 do
    {man, expAdd} = truncate_zeros(mantissa)
    exp = exponent + expAdd

    octExp =
      cond do
        exp >= 0 ->
          :erlang.list_to_binary(encode_pos_integer(exp, []))

        true ->
          :erlang.list_to_binary(encode_neg_integer(exp, []))
      end

    signBit =
      cond do
        man > 0 ->
          0

        true ->
          1
      end

    sFactor = 0
    octExpLen = byte_size(octExp)

    cond do
      octExpLen > 255 ->
        exit({:error, {:asn1, {:to_big_exp_in_encode_real, octExpLen}}})

      true ->
        true
    end

    {lenCode, eOctets} =
      case octExpLen do
        1 ->
          {0, octExp}

        2 ->
          {1, octExp}

        3 ->
          {2, octExp}

        _ ->
          {3, <<octExpLen, octExp::binary>>}
      end

    bB = 0
    firstOctet = <<1::size(1), signBit::size(1), bB::size(2), sFactor::size(2), lenCode::size(2)>>

    octMantissa =
      cond do
        man > 0 ->
          :erlang.list_to_binary(real_mininum_octets(man))

        true ->
          :erlang.list_to_binary(real_mininum_octets(-man))
      end

    <<firstOctet::binary, eOctets::binary, octMantissa::binary>>
  end

  defp encode_real(c, {mantissa, base, exponent})
       when base === 10 and is_integer(mantissa) and
              is_integer(exponent) do
    manStr = :erlang.integer_to_list(mantissa)
    encode_real_as_string(c, manStr, exponent)
  end

  defp encode_real(_C, {_, base, _}) do
    exit({:error, {:asn1, {:encode_real_non_supported_encoding, base}}})
  end

  defp encode_real(c, real) when is_list(real) do
    {mantissa, exponent} =
      case :string.lexemes(
             real,
             'Ee'
           ) do
        [nR2] ->
          {nR2, 0}

        [nR3MB, nR3E] ->
          {nR3MB, :erlang.list_to_integer(nR3E)}
      end

    zeroDecimal = fn
      '0' ->
        ''

      l ->
        l
    end

    {newMantissa, lenDecimal} =
      case mantissa do
        [?. | dec] ->
          newMan = remove_trailing_zeros(dec)
          {newMan, length(zeroDecimal.(newMan))}

        _ ->
          case :string.lexemes(mantissa, ',.') do
            [num] ->
              {:erlang.integer_to_list(:erlang.list_to_integer(num)), 0}

            [num, dec] ->
              newDec = zeroDecimal.(remove_trailing_zeros(dec))
              newMan = :erlang.integer_to_list(:erlang.list_to_integer(num)) ++ newDec
              {:erlang.integer_to_list(:erlang.list_to_integer(newMan)), length(newDec)}
          end
      end

    encode_real_as_string(c, newMantissa, exponent - lenDecimal)
  end

  defp encode_real_as_string(_C, mantissa, exponent)
       when is_list(mantissa) and is_integer(exponent) do
    truncMant = remove_trailing_zeros(mantissa)
    expIncr = length(mantissa) - length(truncMant)
    expStr = :erlang.integer_to_list(exponent + expIncr)

    expBin =
      case expStr do
        '0' ->
          "E+0"

        _ ->
          expB = :erlang.list_to_binary(expStr)
          <<?E, expB::binary>>
      end

    manBin = :erlang.list_to_binary(truncMant)
    nR3 = 3
    <<nR3, manBin::binary, ?., expBin::binary>>
  end

  defp remove_trailing_zeros(intStr) do
    case :lists.dropwhile(
           fn
             ?0 ->
               true

             _ ->
               false
           end,
           :lists.reverse(intStr)
         ) do
      [] ->
        '0'

      reversedIntStr ->
        :lists.reverse(reversedIntStr)
    end
  end

  defp truncate_zeros(num) do
    truncate_zeros(num, 0)
  end

  defp truncate_zeros(0, sum) do
    {0, sum}
  end

  defp truncate_zeros(m, sum) do
    case m &&& 15 === m &&& 14 do
      true ->
        truncate_zeros(m >>> 1, sum + 1)

      _ ->
        {m, sum}
    end
  end

  def decode_real(buffer) do
    sz = byte_size(buffer)
    {realVal, <<>>, ^sz} = decode_real2(buffer, [], sz, 0)
    realVal
  end

  defp decode_real2(buffer, _C, 0, _RemBytes) do
    {0, buffer}
  end

  defp decode_real2(buffer0, _C, len, remBytes1) do
    <<first, buffer2::binary>> = buffer0

    cond do
      first === 64 ->
        {:"PLUS-INFINITY", buffer2}

      first === 65 ->
        {:"MINUS-INFINITY", buffer2}

      first === 1 or first === 2 or first === 3 ->
        {nRx, rest} = :erlang.split_binary(buffer2, len - 1)
        {:erlang.binary_to_list(nRx), rest, len}

      true ->
        <<_B7::size(1), sign::size(1), bB::size(2), _FF::size(2), eE::size(2)>> = <<first>>

        base =
          case bB do
            0 ->
              2

            _ ->
              exit({:error, {:asn1, {:non_supported_base, bB}}})
          end

        {firstLen, {exp, buffer3, _Rb2}, remBytes2} =
          case eE do
            0 ->
              {2, decode_integer2(1, buffer2, remBytes1), remBytes1 + 1}

            1 ->
              {3, decode_integer2(2, buffer2, remBytes1), remBytes1 + 2}

            2 ->
              {4, decode_integer2(3, buffer2, remBytes1), remBytes1 + 3}

            3 ->
              <<expLen1, restBuffer::binary>> = buffer2
              {expLen1 + 2, decode_integer2(expLen1, restBuffer, remBytes1), remBytes1 + expLen1}
          end

        length = len - firstLen
        <<longInt::size(length)-unit(8), restBuff::binary>> = buffer3

        {{mantissa, buffer4}, remBytes3} =
          cond do
            sign === 0 ->
              {{longInt, restBuff}, 1 + length}

            true ->
              {{-longInt, restBuff}, 1 + length}
          end

        {{mantissa, base, exp}, buffer4, remBytes2 + remBytes3}
    end
  end

  defp encode_pos_integer(0, [b | _Acc] = l) when b < 128 do
    l
  end

  defp encode_pos_integer(n, acc) do
    encode_pos_integer(n >>> 8, [n &&& 255 | acc])
  end

  defp encode_neg_integer(-1, [b1 | _T] = l) when b1 > 127 do
    l
  end

  defp encode_neg_integer(n, acc) do
    encode_neg_integer(n >>> 8, [n &&& 255 | acc])
  end

  defp real_mininum_octets(val) do
    real_mininum_octets(val, [])
  end

  defp real_mininum_octets(0, acc) do
    acc
  end

  defp real_mininum_octets(val, acc) do
    real_mininum_octets(val >>> 8, [val &&& 255 | acc])
  end

  defp decode_integer2(
         len,
         <<0::size(1), _::size(7), _Bs::binary>> = bin,
         removedBytes
       ) do
    <<int::size(len)-unit(8), buffer2::binary>> = bin
    {int, buffer2, removedBytes}
  end

  defp decode_integer2(
         len,
         <<1::size(1), b2::size(7), bs::binary>>,
         removedBytes
       ) do
    <<n::size(len)-unit(8), buffer2::binary>> = <<b2, bs::binary>>
    int = n - (1 <<< (8 * len - 1))
    {int, buffer2, removedBytes}
  end
end
