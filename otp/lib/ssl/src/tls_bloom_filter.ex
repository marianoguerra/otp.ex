defmodule :m_tls_bloom_filter do
  use Bitwise

  def new(k, m) do
    size = round(:math.ceil(m / 8))
    bitField = :binary.copy(<<0>>, size)
    %{k: k, m: m, current: bitField, old: bitField}
  end

  def add_elem(
        %{k: k, m: m, current: bitField0} = bloomFilter,
        elem
      ) do
    hash = hash(elem, k, m)
    bitField = set_bits(bitField0, hash)
    Map.put(bloomFilter, :current, bitField)
  end

  def contains(
        %{k: k, m: m, current: bFCurrent, old: bFOld},
        elem
      ) do
    hash = hash(elem, k, m)

    :lists.all(
      fn pos ->
        bit_is_set(bFCurrent, pos)
      end,
      hash
    ) or
      :lists.all(
        fn pos ->
          bit_is_set(bFOld, pos)
        end,
        hash
      )
  end

  def rotate(%{m: m, current: bFCurrent} = bloomFilter) do
    size = round(:math.ceil(m / 8))
    bFNew = :binary.copy(<<0>>, size)
    %{bloomFilter | current: bFNew, old: bFCurrent}
  end

  defp bit_is_set(<<1::size(1), _::bitstring>>, 0) do
    true
  end

  defp bit_is_set(bitField, n) do
    case bitField do
      <<_::size(n), 1::size(1), _::bitstring>> ->
        true

      _ ->
        false
    end
  end

  defp set_bits(bitField, []) do
    bitField
  end

  defp set_bits(bitField, [h | t]) do
    set_bits(set_bit(bitField, h), t)
  end

  defp set_bit(bitField, 0) do
    <<_::size(1), rest::bitstring>> = bitField
    <<1::size(1), rest::bitstring>>
  end

  defp set_bit(bitField, b) do
    <<front::size(b), _::size(1), rest::bitstring>> = bitField
    <<front::size(b), 1::size(1), rest::bitstring>>
  end

  defp hash(elem, k, m) do
    hash(elem, k, m, [])
  end

  defp hash(_, 0, _, acc) do
    acc
  end

  defp hash(elem, k, m, acc) do
    h =
      rem(
        :erlang.phash2(
          {elem, 0},
          m
        ) + (k - 1) * :erlang.phash2({elem, 1}, m),
        m
      )

    hash(elem, k - 1, m, [h | acc])
  end
end
