defmodule :m_asn1rtt_per do
  use Bitwise

  def skipextensions(bytes0, nr, extensionBitstr)
      when is_bitstring(extensionBitstr) do
    prev = nr - 1

    case extensionBitstr do
      <<_::size(prev), 1::size(1), _::bitstring>> ->
        {len, bytes1} = decode_length(bytes0)
        <<_::size(len)-binary, bytes2::bitstring>> = bytes1
        skipextensions(bytes2, nr + 1, extensionBitstr)

      <<_::size(prev), 0::size(1), _::bitstring>> ->
        skipextensions(bytes0, nr + 1, extensionBitstr)

      _ ->
        bytes0
    end
  end

  defp align(bin) when is_binary(bin) do
    bin
  end

  defp align(bitStr) when is_bitstring(bitStr) do
    alignBits = rem(bit_size(bitStr), 8)
    <<_::size(alignBits), rest::binary>> = bitStr
    rest
  end

  defp decode_length(buffer) do
    case align(buffer) do
      <<0::size(1), oct::size(7), rest::binary>> ->
        {oct, rest}

      <<2::size(2), val::size(14), rest::binary>> ->
        {val, rest}

      <<3::size(2), _Val::size(14), _Rest::binary>> ->
        exit({:error, {:asn1, {:decode_length, {:nyi, :above_16k}}}})
    end
  end

  def complete(l0) do
    l = complete(l0, [])

    case :erlang.list_to_bitstring(l) do
      <<>> ->
        <<0>>

      bin ->
        bin
    end
  end

  defp complete([], []) do
    []
  end

  defp complete([], [h | more]) do
    complete(h, more)
  end

  defp complete([:align | t], more) do
    complete(t, more)
  end

  defp complete([[] | t], more) do
    complete(t, more)
  end

  defp complete([[_ | _] = h], more) do
    complete(h, more)
  end

  defp complete([[_ | _] = h | t], more) do
    complete(h, [t | more])
  end

  defp complete([h | t], more)
       when is_integer(h) or
              is_binary(h) do
    [h | complete(t, more)]
  end

  defp complete([h | t], more) do
    [h | complete(t, bit_size(h), more)]
  end

  defp complete(bin, more) when is_binary(bin) do
    [bin | complete([], more)]
  end

  defp complete(bin, more) do
    [bin | complete([], bit_size(bin), more)]
  end

  defp complete([], bits, []) do
    case bits &&& 7 do
      0 ->
        []

      n ->
        [<<0::size(8 - n)>>]
    end
  end

  defp complete([], bits, [h | more]) do
    complete(h, bits, more)
  end

  defp complete([:align | t], bits, more) do
    case bits &&& 7 do
      0 ->
        complete(t, more)

      1 ->
        [<<0::size(7)>> | complete(t, more)]

      2 ->
        [<<0::size(6)>> | complete(t, more)]

      3 ->
        [<<0::size(5)>> | complete(t, more)]

      4 ->
        [<<0::size(4)>> | complete(t, more)]

      5 ->
        [<<0::size(3)>> | complete(t, more)]

      6 ->
        [<<0::size(2)>> | complete(t, more)]

      7 ->
        [<<0::size(1)>> | complete(t, more)]
    end
  end

  defp complete([[] | t], bits, more) do
    complete(t, bits, more)
  end

  defp complete([[_ | _] = h], bits, more) do
    complete(h, bits, more)
  end

  defp complete([[_ | _] = h | t], bits, more) do
    complete(h, bits, [t | more])
  end

  defp complete([h | t], bits, more)
       when is_integer(h) or
              is_binary(h) do
    [h | complete(t, bits, more)]
  end

  defp complete([h | t], bits, more) do
    [h | complete(t, bits + bit_size(h), more)]
  end

  defp complete(bin, bits, more) when is_binary(bin) do
    [bin | complete([], bits, more)]
  end

  defp complete(bin, bits, more) do
    [bin | complete([], bits + bit_size(bin), more)]
  end
end
