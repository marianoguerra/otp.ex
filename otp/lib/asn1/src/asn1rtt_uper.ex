defmodule :m_asn1rtt_uper do
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

  defp decode_length(<<0::size(1), oct::size(7), rest::bitstring>>) do
    {oct, rest}
  end

  defp decode_length(<<2::size(2), val::size(14), rest::bitstring>>) do
    {val, rest}
  end

  defp decode_length(<<3::size(2), _::size(14), _Rest::bitstring>>) do
    exit({:error, {:asn1, {:decode_length, {:nyi, :above_16k}}}})
  end

  def complete(inList) when is_list(inList) do
    case :erlang.list_to_bitstring(inList) do
      <<>> ->
        <<0>>

      res ->
        sz = bit_size(res)

        case sz &&& 7 do
          0 ->
            res

          bits ->
            <<res::size(sz)-bitstring, 0::size(8 - bits)>>
        end
    end
  end

  def complete(bin) when is_binary(bin) do
    case bin do
      <<>> ->
        <<0>>

      _ ->
        bin
    end
  end

  def complete(inList) when is_bitstring(inList) do
    sz = bit_size(inList)
    padLen = 8 - sz &&& 7
    <<inList::size(sz)-bitstring, 0::size(padLen)>>
  end

  def complete_NFP(inList) when is_list(inList) do
    :erlang.list_to_bitstring(inList)
  end

  def complete_NFP(inList) when is_bitstring(inList) do
    inList
  end
end
