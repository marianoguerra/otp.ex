defmodule :m_base64 do
  use Bitwise

  def encode_to_string(bin) when is_binary(bin) do
    encode_to_string(:erlang.binary_to_list(bin))
  end

  def encode_to_string(list) when is_list(list) do
    encode_list_to_string(list)
  end

  def encode(bin) when is_binary(bin) do
    encode_binary(bin, <<>>)
  end

  def encode(list) when is_list(list) do
    encode_list(list, <<>>)
  end

  defp encode_list_to_string([]) do
    []
  end

  defp encode_list_to_string([b1]) do
    [b64e(b1 >>> 2), b64e(b1 &&& 3 <<< 4), ?=, ?=]
  end

  defp encode_list_to_string([b1, b2]) do
    [b64e(b1 >>> 2), b64e((b1 &&& 3 <<< 4) ||| b2 >>> 4), b64e(b2 &&& 15 <<< 2), ?=]
  end

  defp encode_list_to_string([b1, b2, b3 | ls]) do
    bB = b1 <<< 16 ||| b2 <<< 8 ||| b3

    [
      b64e(bB >>> 18),
      b64e(bB >>> 12 &&& 63),
      b64e(bB >>> 6 &&& 63),
      b64e(bB &&& 63)
      | encode_list_to_string(ls)
    ]
  end

  defp encode_binary(<<>>, a) do
    a
  end

  defp encode_binary(<<b1::size(8)>>, a) do
    <<a::bits, b64e(b1 >>> 2)::size(8), b64e(b1 &&& 3 <<< 4)::size(8), ?=::size(8), ?=::size(8)>>
  end

  defp encode_binary(<<b1::size(8), b2::size(8)>>, a) do
    <<a::bits, b64e(b1 >>> 2)::size(8), b64e((b1 &&& 3 <<< 4) ||| b2 >>> 4)::size(8),
      b64e(b2 &&& 15 <<< 2)::size(8), ?=::size(8)>>
  end

  defp encode_binary(
         <<b1::size(8), b2::size(8), b3::size(8), ls::bits>>,
         a
       ) do
    bB = b1 <<< 16 ||| b2 <<< 8 ||| b3

    encode_binary(
      ls,
      <<a::bits, b64e(bB >>> 18)::size(8), b64e(bB >>> 12 &&& 63)::size(8),
        b64e(bB >>> 6 &&& 63)::size(8), b64e(bB &&& 63)::size(8)>>
    )
  end

  defp encode_list([], a) do
    a
  end

  defp encode_list([b1], a) do
    <<a::bits, b64e(b1 >>> 2)::size(8), b64e(b1 &&& 3 <<< 4)::size(8), ?=::size(8), ?=::size(8)>>
  end

  defp encode_list([b1, b2], a) do
    <<a::bits, b64e(b1 >>> 2)::size(8), b64e((b1 &&& 3 <<< 4) ||| b2 >>> 4)::size(8),
      b64e(b2 &&& 15 <<< 2)::size(8), ?=::size(8)>>
  end

  defp encode_list([b1, b2, b3 | ls], a) do
    bB = b1 <<< 16 ||| b2 <<< 8 ||| b3

    encode_list(
      ls,
      <<a::bits, b64e(bB >>> 18)::size(8), b64e(bB >>> 12 &&& 63)::size(8),
        b64e(bB >>> 6 &&& 63)::size(8), b64e(bB &&& 63)::size(8)>>
    )
  end

  def decode(bin) when is_binary(bin) do
    decode_binary(bin, <<>>)
  end

  def decode(list) when is_list(list) do
    decode_list(list, <<>>)
  end

  def mime_decode(bin) when is_binary(bin) do
    mime_decode_binary(bin, <<>>)
  end

  def mime_decode(list) when is_list(list) do
    mime_decode_list(list, <<>>)
  end

  def decode_to_string(bin) when is_binary(bin) do
    decode_to_string(:erlang.binary_to_list(bin))
  end

  def decode_to_string(list) when is_list(list) do
    decode_list_to_string(list)
  end

  def mime_decode_to_string(bin) when is_binary(bin) do
    mime_decode_to_string(:erlang.binary_to_list(bin))
  end

  def mime_decode_to_string(list) when is_list(list) do
    mime_decode_list_to_string(list)
  end

  defp mime_decode_list([0 | cs], a) do
    mime_decode_list(cs, a)
  end

  defp mime_decode_list([c1 | cs], a) do
    case b64d(c1) do
      b1 when is_integer(b1) ->
        mime_decode_list(cs, a, b1)

      _ ->
        mime_decode_list(cs, a)
    end
  end

  defp mime_decode_list([], a) do
    a
  end

  defp mime_decode_list([0 | cs], a, b1) do
    mime_decode_list(cs, a, b1)
  end

  defp mime_decode_list([c2 | cs], a, b1) do
    case b64d(c2) do
      b2 when is_integer(b2) ->
        mime_decode_list(cs, a, b1, b2)

      _ ->
        mime_decode_list(cs, a, b1)
    end
  end

  defp mime_decode_list([0 | cs], a, b1, b2) do
    mime_decode_list(cs, a, b1, b2)
  end

  defp mime_decode_list([c3 | cs], a, b1, b2) do
    case b64d(c3) do
      b3 when is_integer(b3) ->
        mime_decode_list(cs, a, b1, b2, b3)

      :eq = b3 ->
        mime_decode_list_after_eq(cs, a, b1, b2, b3)

      _ ->
        mime_decode_list(cs, a, b1, b2)
    end
  end

  defp mime_decode_list([0 | cs], a, b1, b2, b3) do
    mime_decode_list(cs, a, b1, b2, b3)
  end

  defp mime_decode_list([c4 | cs], a, b1, b2, b3) do
    case b64d(c4) do
      b4 when is_integer(b4) ->
        mime_decode_list(
          cs,
          <<a::bits, b1::size(6), b2::size(6), b3::size(6), b4::size(6)>>
        )

      :eq ->
        mime_decode_list_after_eq(cs, a, b1, b2, b3)

      _ ->
        mime_decode_list(cs, a, b1, b2, b3)
    end
  end

  defp mime_decode_list_after_eq([0 | cs], a, b1, b2, b3) do
    mime_decode_list_after_eq(cs, a, b1, b2, b3)
  end

  defp mime_decode_list_after_eq([c | cs], a, b1, b2, b3) do
    case b64d(c) do
      b when is_integer(b) ->
        case b3 do
          :eq ->
            mime_decode_list(cs, a, b1, b2, b)

          _ ->
            mime_decode_list(
              cs,
              <<a::bits, b1::size(6), b2::size(6), b3::size(6), b::size(6)>>
            )
        end

      _ ->
        mime_decode_list_after_eq(cs, a, b1, b2, b3)
    end
  end

  defp mime_decode_list_after_eq([], a, b1, b2, :eq) do
    <<a::bits, b1::size(6), b2 >>> 4::size(2)>>
  end

  defp mime_decode_list_after_eq([], a, b1, b2, b3) do
    <<a::bits, b1::size(6), b2::size(6), b3 >>> 2::size(4)>>
  end

  defp mime_decode_binary(<<0::size(8), cs::bits>>, a) do
    mime_decode_binary(cs, a)
  end

  defp mime_decode_binary(<<c1::size(8), cs::bits>>, a) do
    case b64d(c1) do
      b1 when is_integer(b1) ->
        mime_decode_binary(cs, a, b1)

      _ ->
        mime_decode_binary(cs, a)
    end
  end

  defp mime_decode_binary(<<>>, a) do
    a
  end

  defp mime_decode_binary(<<0::size(8), cs::bits>>, a, b1) do
    mime_decode_binary(cs, a, b1)
  end

  defp mime_decode_binary(<<c2::size(8), cs::bits>>, a, b1) do
    case b64d(c2) do
      b2 when is_integer(b2) ->
        mime_decode_binary(cs, a, b1, b2)

      _ ->
        mime_decode_binary(cs, a, b1)
    end
  end

  defp mime_decode_binary(<<0::size(8), cs::bits>>, a, b1, b2) do
    mime_decode_binary(cs, a, b1, b2)
  end

  defp mime_decode_binary(<<c3::size(8), cs::bits>>, a, b1, b2) do
    case b64d(c3) do
      b3 when is_integer(b3) ->
        mime_decode_binary(cs, a, b1, b2, b3)

      :eq = b3 ->
        mime_decode_binary_after_eq(cs, a, b1, b2, b3)

      _ ->
        mime_decode_binary(cs, a, b1, b2)
    end
  end

  defp mime_decode_binary(<<0::size(8), cs::bits>>, a, b1, b2, b3) do
    mime_decode_binary(cs, a, b1, b2, b3)
  end

  defp mime_decode_binary(<<c4::size(8), cs::bits>>, a, b1, b2, b3) do
    case b64d(c4) do
      b4 when is_integer(b4) ->
        mime_decode_binary(
          cs,
          <<a::bits, b1::size(6), b2::size(6), b3::size(6), b4::size(6)>>
        )

      :eq ->
        mime_decode_binary_after_eq(cs, a, b1, b2, b3)

      _ ->
        mime_decode_binary(cs, a, b1, b2, b3)
    end
  end

  defp mime_decode_binary_after_eq(<<0::size(8), cs::bits>>, a, b1, b2, b3) do
    mime_decode_binary_after_eq(cs, a, b1, b2, b3)
  end

  defp mime_decode_binary_after_eq(<<c::size(8), cs::bits>>, a, b1, b2, b3) do
    case b64d(c) do
      b when is_integer(b) ->
        case b3 do
          :eq ->
            mime_decode_binary(cs, a, b1, b2, b)

          _ ->
            mime_decode_binary(
              cs,
              <<a::bits, b1::size(6), b2::size(6), b3::size(6), b::size(6)>>
            )
        end

      _ ->
        mime_decode_binary_after_eq(cs, a, b1, b2, b3)
    end
  end

  defp mime_decode_binary_after_eq(<<>>, a, b1, b2, :eq) do
    <<a::bits, b1::size(6), b2 >>> 4::size(2)>>
  end

  defp mime_decode_binary_after_eq(<<>>, a, b1, b2, b3) do
    <<a::bits, b1::size(6), b2::size(6), b3 >>> 2::size(4)>>
  end

  defp mime_decode_list_to_string([0 | cs]) do
    mime_decode_list_to_string(cs)
  end

  defp mime_decode_list_to_string([c1 | cs]) do
    case b64d(c1) do
      b1 when is_integer(b1) ->
        mime_decode_list_to_string(cs, b1)

      _ ->
        mime_decode_list_to_string(cs)
    end
  end

  defp mime_decode_list_to_string([]) do
    []
  end

  defp mime_decode_list_to_string([0 | cs], b1) do
    mime_decode_list_to_string(cs, b1)
  end

  defp mime_decode_list_to_string([c2 | cs], b1) do
    case b64d(c2) do
      b2 when is_integer(b2) ->
        mime_decode_list_to_string(cs, b1, b2)

      _ ->
        mime_decode_list_to_string(cs, b1)
    end
  end

  defp mime_decode_list_to_string([0 | cs], b1, b2) do
    mime_decode_list_to_string(cs, b1, b2)
  end

  defp mime_decode_list_to_string([c3 | cs], b1, b2) do
    case b64d(c3) do
      b3 when is_integer(b3) ->
        mime_decode_list_to_string(cs, b1, b2, b3)

      :eq = b3 ->
        mime_decode_list_to_string_after_eq(cs, b1, b2, b3)

      _ ->
        mime_decode_list_to_string(cs, b1, b2)
    end
  end

  defp mime_decode_list_to_string([0 | cs], b1, b2, b3) do
    mime_decode_list_to_string(cs, b1, b2, b3)
  end

  defp mime_decode_list_to_string([c4 | cs], b1, b2, b3) do
    case b64d(c4) do
      b4 when is_integer(b4) ->
        bits4x6 = b1 <<< 18 ||| b2 <<< 12 ||| b3 <<< 6 ||| b4
        octet1 = bits4x6 >>> 16
        octet2 = bits4x6 >>> 8 &&& 255
        octet3 = bits4x6 &&& 255

        [
          octet1,
          octet2,
          octet3
          | mime_decode_list_to_string(cs)
        ]

      :eq ->
        mime_decode_list_to_string_after_eq(cs, b1, b2, b3)

      _ ->
        mime_decode_list_to_string(cs, b1, b2, b3)
    end
  end

  defp mime_decode_list_to_string_after_eq([0 | cs], b1, b2, b3) do
    mime_decode_list_to_string_after_eq(cs, b1, b2, b3)
  end

  defp mime_decode_list_to_string_after_eq([c | cs], b1, b2, b3) do
    case b64d(c) do
      b when is_integer(b) ->
        case b3 do
          :eq ->
            mime_decode_list_to_string(cs, b1, b2, b)

          _ ->
            bits4x6 = b1 <<< 18 ||| b2 <<< 12 ||| b3 <<< 6 ||| b
            octet1 = bits4x6 >>> 16
            octet2 = bits4x6 >>> 8 &&& 255
            octet3 = bits4x6 &&& 255

            [
              octet1,
              octet2,
              octet3
              | mime_decode_list_to_string(cs)
            ]
        end

      _ ->
        mime_decode_list_to_string_after_eq(cs, b1, b2, b3)
    end
  end

  defp mime_decode_list_to_string_after_eq([], b1, b2, :eq) do
    :erlang.binary_to_list(<<b1::size(6), b2 >>> 4::size(2)>>)
  end

  defp mime_decode_list_to_string_after_eq([], b1, b2, b3) do
    :erlang.binary_to_list(<<b1::size(6), b2::size(6), b3 >>> 2::size(4)>>)
  end

  defp decode_list([c1 | cs], a) do
    case b64d(c1) do
      :ws ->
        decode_list(cs, a)

      b1 ->
        decode_list(cs, a, b1)
    end
  end

  defp decode_list([], a) do
    a
  end

  defp decode_list([c2 | cs], a, b1) do
    case b64d(c2) do
      :ws ->
        decode_list(cs, a, b1)

      b2 ->
        decode_list(cs, a, b1, b2)
    end
  end

  defp decode_list([c3 | cs], a, b1, b2) do
    case b64d(c3) do
      :ws ->
        decode_list(cs, a, b1, b2)

      b3 ->
        decode_list(cs, a, b1, b2, b3)
    end
  end

  defp decode_list([c4 | cs], a, b1, b2, b3) do
    case b64d(c4) do
      :ws ->
        decode_list(cs, a, b1, b2, b3)

      :eq when b3 === :eq ->
        only_ws(
          cs,
          <<a::bits, b1::size(6), b2 >>> 4::size(2)>>
        )

      :eq ->
        only_ws(
          cs,
          <<a::bits, b1::size(6), b2::size(6), b3 >>> 2::size(4)>>
        )

      b4 ->
        decode_list(
          cs,
          <<a::bits, b1::size(6), b2::size(6), b3::size(6), b4::size(6)>>
        )
    end
  end

  defp decode_binary(<<c1::size(8), cs::bits>>, a) do
    case b64d(c1) do
      :ws ->
        decode_binary(cs, a)

      b1 ->
        decode_binary(cs, a, b1)
    end
  end

  defp decode_binary(<<>>, a) do
    a
  end

  defp decode_binary(<<c2::size(8), cs::bits>>, a, b1) do
    case b64d(c2) do
      :ws ->
        decode_binary(cs, a, b1)

      b2 ->
        decode_binary(cs, a, b1, b2)
    end
  end

  defp decode_binary(<<c3::size(8), cs::bits>>, a, b1, b2) do
    case b64d(c3) do
      :ws ->
        decode_binary(cs, a, b1, b2)

      b3 ->
        decode_binary(cs, a, b1, b2, b3)
    end
  end

  defp decode_binary(<<c4::size(8), cs::bits>>, a, b1, b2, b3) do
    case b64d(c4) do
      :ws ->
        decode_binary(cs, a, b1, b2, b3)

      :eq when b3 === :eq ->
        only_ws_binary(
          cs,
          <<a::bits, b1::size(6), b2 >>> 4::size(2)>>
        )

      :eq ->
        only_ws_binary(
          cs,
          <<a::bits, b1::size(6), b2::size(6), b3 >>> 2::size(4)>>
        )

      b4 ->
        decode_binary(
          cs,
          <<a::bits, b1::size(6), b2::size(6), b3::size(6), b4::size(6)>>
        )
    end
  end

  defp only_ws_binary(<<>>, a) do
    a
  end

  defp only_ws_binary(<<c::size(8), cs::bits>>, a) do
    case b64d(c) do
      :ws ->
        only_ws_binary(cs, a)
    end
  end

  defp decode_list_to_string([c1 | cs]) do
    case b64d(c1) do
      :ws ->
        decode_list_to_string(cs)

      b1 ->
        decode_list_to_string(cs, b1)
    end
  end

  defp decode_list_to_string([]) do
    []
  end

  defp decode_list_to_string([c2 | cs], b1) do
    case b64d(c2) do
      :ws ->
        decode_list_to_string(cs, b1)

      b2 ->
        decode_list_to_string(cs, b1, b2)
    end
  end

  defp decode_list_to_string([c3 | cs], b1, b2) do
    case b64d(c3) do
      :ws ->
        decode_list_to_string(cs, b1, b2)

      b3 ->
        decode_list_to_string(cs, b1, b2, b3)
    end
  end

  defp decode_list_to_string([c4 | cs], b1, b2, b3) do
    case b64d(c4) do
      :ws ->
        decode_list_to_string(cs, b1, b2, b3)

      :eq when b3 === :eq ->
        only_ws(
          cs,
          :erlang.binary_to_list(<<b1::size(6), b2 >>> 4::size(2)>>)
        )

      :eq ->
        only_ws(
          cs,
          :erlang.binary_to_list(<<b1::size(6), b2::size(6), b3 >>> 2::size(4)>>)
        )

      b4 ->
        bits4x6 = b1 <<< 18 ||| b2 <<< 12 ||| b3 <<< 6 ||| b4
        octet1 = bits4x6 >>> 16
        octet2 = bits4x6 >>> 8 &&& 255
        octet3 = bits4x6 &&& 255
        [octet1, octet2, octet3 | decode_list_to_string(cs)]
    end
  end

  defp only_ws([], a) do
    a
  end

  defp only_ws([c | cs], a) do
    case b64d(c) do
      :ws ->
        only_ws(cs, a)
    end
  end

  defp b64d(x) do
    :erlang.element(
      x,
      {:bad, :bad, :bad, :bad, :bad, :bad, :bad, :bad, :ws, :ws, :bad, :bad, :ws, :bad, :bad,
       :bad, :bad, :bad, :bad, :bad, :bad, :bad, :bad, :bad, :bad, :bad, :bad, :bad, :bad, :bad,
       :bad, :ws, :bad, :bad, :bad, :bad, :bad, :bad, :bad, :bad, :bad, :bad, 62, :bad, :bad,
       :bad, 63, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, :bad, :bad, :bad, :eq, :bad, :bad, :bad,
       0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
       25, :bad, :bad, :bad, :bad, :bad, :bad, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38,
       39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, :bad, :bad, :bad, :bad, :bad, :bad,
       :bad, :bad, :bad, :bad, :bad, :bad, :bad, :bad, :bad, :bad, :bad, :bad, :bad, :bad, :bad,
       :bad, :bad, :bad, :bad, :bad, :bad, :bad, :bad, :bad, :bad, :bad, :bad, :bad, :bad, :bad,
       :bad, :bad, :bad, :bad, :bad, :bad, :bad, :bad, :bad, :bad, :bad, :bad, :bad, :bad, :bad,
       :bad, :bad, :bad, :bad, :bad, :bad, :bad, :bad, :bad, :bad, :bad, :bad, :bad, :bad, :bad,
       :bad, :bad, :bad, :bad, :bad, :bad, :bad, :bad, :bad, :bad, :bad, :bad, :bad, :bad, :bad,
       :bad, :bad, :bad, :bad, :bad, :bad, :bad, :bad, :bad, :bad, :bad, :bad, :bad, :bad, :bad,
       :bad, :bad, :bad, :bad, :bad, :bad, :bad, :bad, :bad, :bad, :bad, :bad, :bad, :bad, :bad,
       :bad, :bad, :bad, :bad, :bad, :bad, :bad, :bad, :bad, :bad, :bad, :bad, :bad, :bad, :bad,
       :bad, :bad, :bad, :bad, :bad, :bad, :bad}
    )
  end

  defp b64e(x) do
    :erlang.element(
      x + 1,
      {?A, ?B, ?C, ?D, ?E, ?F, ?G, ?H, ?I, ?J, ?K, ?L, ?M, ?N, ?O, ?P, ?Q, ?R, ?S, ?T, ?U, ?V, ?W,
       ?X, ?Y, ?Z, ?a, ?b, ?c, ?d, ?e, ?f, ?g, ?h, ?i, ?j, ?k, ?l, ?m, ?n, ?o, ?p, ?q, ?r, ?s, ?t,
       ?u, ?v, ?w, ?x, ?y, ?z, ?0, ?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9, ?+, ?/}
    )
  end
end
