defmodule :m_asn1rtt_ber do
  use Bitwise

  def ber_encode([tlv]) do
    ber_encode(tlv)
  end

  def ber_encode(tlv) when is_binary(tlv) do
    tlv
  end

  def ber_encode(tlv) do
    :asn1rt_nif.encode_ber_tlv(tlv)
  end

  def ber_decode_nif(b) do
    :asn1rt_nif.decode_ber_tlv(b)
  end

  def ber_decode_erlang(b) when is_binary(b) do
    decode_primitive(b)
  end

  def ber_decode_erlang(tlv) do
    {tlv, <<>>}
  end

  defp decode_primitive(bin) do
    {form, tagNo, v, rest} = decode_tag_and_length(bin)

    case form do
      1 ->
        {{tagNo, decode_constructed(v)}, rest}

      0 ->
        {{tagNo, v}, rest}

      2 ->
        {vlist, rest2} = decode_constructed_indefinite(v, [])
        {{tagNo, vlist}, rest2}
    end
  end

  defp decode_constructed(bin) when byte_size(bin) === 0 do
    []
  end

  defp decode_constructed(bin) do
    {tlv, rest} = decode_primitive(bin)
    [tlv | decode_constructed(rest)]
  end

  defp decode_constructed_indefinite(<<0, 0, rest::binary>>, acc) do
    {:lists.reverse(acc), rest}
  end

  defp decode_constructed_indefinite(bin, acc) do
    {tlv, rest} = decode_primitive(bin)
    decode_constructed_indefinite(rest, [tlv | acc])
  end

  def decode_primitive_incomplete([[:default, tagNo]], bin) do
    case decode_tag_and_length(bin) do
      {form, ^tagNo, v, rest} ->
        decode_incomplete2(form, tagNo, v, [], rest)

      _ ->
        :asn1_NOVALUE
    end
  end

  def decode_primitive_incomplete([[:default, tagNo, directives]], bin) do
    case decode_tag_and_length(bin) do
      {form, ^tagNo, v, rest} ->
        decode_incomplete2(form, tagNo, v, directives, rest)

      _ ->
        :asn1_NOVALUE
    end
  end

  def decode_primitive_incomplete([[:opt, tagNo]], bin) do
    case decode_tag_and_length(bin) do
      {form, ^tagNo, v, rest} ->
        decode_incomplete2(form, tagNo, v, [], rest)

      _ ->
        :asn1_NOVALUE
    end
  end

  def decode_primitive_incomplete([[:opt, tagNo, directives]], bin) do
    case decode_tag_and_length(bin) do
      {form, ^tagNo, v, rest} ->
        decode_incomplete2(form, tagNo, v, directives, rest)

      _ ->
        :asn1_NOVALUE
    end
  end

  def decode_primitive_incomplete([[:opt_undec, tag]], bin) do
    case decode_tag_and_length(bin) do
      {_, ^tag, _, _} ->
        decode_incomplete_bin(bin)

      _ ->
        :asn1_NOVALUE
    end
  end

  def decode_primitive_incomplete([[:alt_undec, tagNo] | restAlts], bin) do
    case decode_tag_and_length(bin) do
      {_, ^tagNo, _, _} ->
        decode_incomplete_bin(bin)

      _ ->
        decode_primitive_incomplete(restAlts, bin)
    end
  end

  def decode_primitive_incomplete([[:alt, tagNo] | restAlts], bin) do
    case decode_tag_and_length(bin) do
      {_Form, ^tagNo, v, rest} ->
        {{tagNo, v}, rest}

      _ ->
        decode_primitive_incomplete(restAlts, bin)
    end
  end

  def decode_primitive_incomplete([[:alt, tagNo, directives] | restAlts], bin) do
    case decode_tag_and_length(bin) do
      {form, ^tagNo, v, rest} ->
        decode_incomplete2(form, tagNo, v, directives, rest)

      _ ->
        decode_primitive_incomplete(restAlts, bin)
    end
  end

  def decode_primitive_incomplete([[:alt_parts, tagNo]], bin) do
    case decode_tag_and_length(bin) do
      {_Form, ^tagNo, v, rest} ->
        {{tagNo, v}, rest}

      _ ->
        :asn1_NOVALUE
    end
  end

  def decode_primitive_incomplete([[:alt_parts, tagNo] | restAlts], bin) do
    case decode_tag_and_length(bin) do
      {_Form, ^tagNo, v, rest} ->
        {{tagNo, decode_parts_incomplete(v)}, rest}

      _ ->
        decode_primitive_incomplete(restAlts, bin)
    end
  end

  def decode_primitive_incomplete([[:undec, _TagNo] | _RestTag], bin) do
    decode_incomplete_bin(bin)
  end

  def decode_primitive_incomplete([[:parts, tagNo] | _RestTag], bin) do
    case decode_tag_and_length(bin) do
      {_Form, ^tagNo, v, rest} ->
        {{tagNo, decode_parts_incomplete(v)}, rest}

      err ->
        {:error, {:asn1, 'tag failure', tagNo, err}}
    end
  end

  def decode_primitive_incomplete([:mandatory | restTag], bin) do
    {form, tagNo, v, rest} = decode_tag_and_length(bin)
    decode_incomplete2(form, tagNo, v, restTag, rest)
  end

  def decode_primitive_incomplete([[:mandatory | directives]], bin) do
    {form, tagNo, v, rest} = decode_tag_and_length(bin)
    decode_incomplete2(form, tagNo, v, directives, rest)
  end

  def decode_primitive_incomplete([], bin) do
    decode_primitive(bin)
  end

  defp decode_parts_incomplete(<<>>) do
    []
  end

  defp decode_parts_incomplete(bin) do
    {:ok, rest} = skip_tag(bin)
    {:ok, rest2} = skip_length_and_value(rest)
    lenPart = byte_size(bin) - byte_size(rest2)
    <<part::size(lenPart)-binary, restBin::binary>> = bin
    [part | decode_parts_incomplete(restBin)]
  end

  defp decode_incomplete2(_Form = 2, tagNo, v, tagMatch, _) do
    {vlist, rest2} = decode_constr_indef_incomplete(tagMatch, v, [])
    {{tagNo, vlist}, rest2}
  end

  defp decode_incomplete2(1, tagNo, v, [tagMatch], rest)
       when is_list(tagMatch) do
    {{tagNo, decode_constructed_incomplete(tagMatch, v)}, rest}
  end

  defp decode_incomplete2(1, tagNo, v, tagMatch, rest) do
    {{tagNo, decode_constructed_incomplete(tagMatch, v)}, rest}
  end

  defp decode_incomplete2(0, tagNo, v, _TagMatch, rest) do
    {{tagNo, v}, rest}
  end

  defp decode_constructed_incomplete([tags = [ts]], bin) when is_list(ts) do
    decode_constructed_incomplete(tags, bin)
  end

  defp decode_constructed_incomplete(_TagMatch, <<>>) do
    []
  end

  defp decode_constructed_incomplete([:mandatory | restTag], bin) do
    {tlv, rest} = decode_primitive(bin)
    [tlv | decode_constructed_incomplete(restTag, rest)]
  end

  defp decode_constructed_incomplete(directives = [[alt, _] | _], bin)
       when alt === :alt_undec or alt === :alt or
              alt === :alt_parts do
    {_Form, tagNo, v, rest} = decode_tag_and_length(bin)

    case incomplete_choice_alt(tagNo, directives) do
      {:alt_undec, _} ->
        lenA = byte_size(bin) - byte_size(rest)
        <<a::size(lenA)-binary, ^rest::binary>> = bin
        a

      {:alt, innerDirectives} ->
        {tlv, ^rest} = decode_primitive_incomplete(innerDirectives, v)
        {tagNo, tlv}

      {:alt_parts, _} ->
        [{tagNo, decode_parts_incomplete(v)}]

      :no_match ->
        {tlv, _} = decode_primitive(bin)
        tlv
    end
  end

  defp decode_constructed_incomplete([tagNo | restTag], bin) do
    case decode_primitive_incomplete([tagNo], bin) do
      {tlv, rest} ->
        [tlv | decode_constructed_incomplete(restTag, rest)]

      :asn1_NOVALUE ->
        decode_constructed_incomplete(restTag, bin)
    end
  end

  defp decode_constructed_incomplete([], bin) do
    {tlv, rest} = decode_primitive(bin)
    [tlv | decode_constructed_incomplete([], rest)]
  end

  defp decode_constr_indef_incomplete(_TagMatch, <<0, 0, rest::binary>>, acc) do
    {:lists.reverse(acc), rest}
  end

  defp decode_constr_indef_incomplete([tag | restTags], bin, acc) do
    case decode_primitive_incomplete([tag], bin) do
      {tlv, rest} ->
        decode_constr_indef_incomplete(restTags, rest, [tlv | acc])

      :asn1_NOVALUE ->
        decode_constr_indef_incomplete(restTags, bin, acc)
    end
  end

  defp decode_incomplete_bin(bin) do
    {:ok, rest} = skip_tag(bin)
    {:ok, rest2} = skip_length_and_value(rest)
    incLen = byte_size(bin) - byte_size(rest2)
    <<incBin::size(incLen)-binary, ret::binary>> = bin
    {incBin, ret}
  end

  defp incomplete_choice_alt(tagNo, [[alt, tagNo] | directives]) do
    {alt, directives}
  end

  defp incomplete_choice_alt(tagNo, [d]) when is_list(d) do
    incomplete_choice_alt(tagNo, d)
  end

  defp incomplete_choice_alt(tagNo, [_H | directives]) do
    incomplete_choice_alt(tagNo, directives)
  end

  defp incomplete_choice_alt(_, []) do
    :no_match
  end

  def decode_selective([], binary) do
    {:ok, binary}
  end

  def decode_selective([:skip | restPattern], binary) do
    {:ok, restBinary} = skip_tag(binary)
    {:ok, restBinary2} = skip_length_and_value(restBinary)
    decode_selective(restPattern, restBinary2)
  end

  def decode_selective([[:skip_optional, tag] | restPattern], binary) do
    case skip_optional_tag(tag, binary) do
      {:ok, restBinary} ->
        {:ok, restBinary2} = skip_length_and_value(restBinary)
        decode_selective(restPattern, restBinary2)

      :missing ->
        decode_selective(restPattern, binary)
    end
  end

  def decode_selective([[:choosen, tag]], binary) do
    return_value(tag, binary)
  end

  def decode_selective([[:choosen, tag] | restPattern], binary) do
    case skip_optional_tag(tag, binary) do
      {:ok, restBinary} ->
        {:ok, value} = get_value(restBinary)
        decode_selective(restPattern, value)

      :missing ->
        {:ok, <<>>}
    end
  end

  def decode_selective(p, _) do
    {:error, {:asn1, {:partial_decode, 'bad pattern', p}}}
  end

  defp return_value(tag, binary) do
    {:ok, {^tag, restBinary}} = get_tag(binary)
    {:ok, {lenVal, _RestBinary2}} = get_length_and_value(restBinary)
    {:ok, <<tag::binary, lenVal::binary>>}
  end

  defp skip_tag(<<_::size(3), 31::size(5), rest::binary>>) do
    skip_long_tag(rest)
  end

  defp skip_tag(<<_::size(3), _Tag::size(5), rest::binary>>) do
    {:ok, rest}
  end

  defp skip_long_tag(<<1::size(1), _::size(7), rest::binary>>) do
    skip_long_tag(rest)
  end

  defp skip_long_tag(<<0::size(1), _::size(7), rest::binary>>) do
    {:ok, rest}
  end

  defp skip_optional_tag(<<>>, binary) do
    {:ok, binary}
  end

  defp skip_optional_tag(
         <<tag, restTag::binary>>,
         <<tag, rest::binary>>
       ) do
    skip_optional_tag(restTag, rest)
  end

  defp skip_optional_tag(_, _) do
    :missing
  end

  defp skip_length_and_value(binary) do
    case decode_length(binary) do
      {:indefinite, restBinary} ->
        skip_indefinite_value(restBinary)

      {length, restBinary} ->
        <<_::size(length)-unit(8), rest::binary>> = restBinary
        {:ok, rest}
    end
  end

  defp skip_indefinite_value(<<0, 0, rest::binary>>) do
    {:ok, rest}
  end

  defp skip_indefinite_value(binary) do
    {:ok, restBinary} = skip_tag(binary)
    {:ok, restBinary2} = skip_length_and_value(restBinary)
    skip_indefinite_value(restBinary2)
  end

  defp get_value(binary) do
    case decode_length(binary) do
      {:indefinite, restBinary} ->
        get_indefinite_value(restBinary, [])

      {length, restBinary} ->
        <<value::size(length)-binary, _Rest::binary>> = restBinary
        {:ok, value}
    end
  end

  defp get_indefinite_value(<<0, 0, _Rest::binary>>, acc) do
    {:ok, :erlang.list_to_binary(:lists.reverse(acc))}
  end

  defp get_indefinite_value(binary, acc) do
    {:ok, {tag, restBinary}} = get_tag(binary)
    {:ok, {lenVal, restBinary2}} = get_length_and_value(restBinary)
    get_indefinite_value(restBinary2, [lenVal, tag | acc])
  end

  defp get_tag(<<h::size(1)-binary, rest::binary>>) do
    case h do
      <<_::size(3), 31::size(5)>> ->
        get_long_tag(rest, [h])

      _ ->
        {:ok, {h, rest}}
    end
  end

  defp get_long_tag(
         <<h::size(1)-binary, rest::binary>>,
         acc
       ) do
    case h do
      <<0::size(1), _::size(7)>> ->
        {:ok, {:erlang.list_to_binary(:lists.reverse([h | acc])), rest}}

      _ ->
        get_long_tag(rest, [h | acc])
    end
  end

  defp get_length_and_value(bin = <<0::size(1), length::size(7), _T::binary>>) do
    <<len, val::size(length)-binary, rest::binary>> = bin
    {:ok, {<<len, val::binary>>, rest}}
  end

  defp get_length_and_value(bin = <<1::size(1), 0::size(7), _T::binary>>) do
    get_indefinite_length_and_value(bin)
  end

  defp get_length_and_value(<<1::size(1), lL::size(7), t::binary>>) do
    <<length::size(lL)-unit(8), rest::binary>> = t
    <<value::size(length)-binary, rest2::binary>> = rest
    {:ok, {<<1::size(1), lL::size(7), length::size(lL)-unit(8), value::binary>>, rest2}}
  end

  defp get_indefinite_length_and_value(<<h, t::binary>>) do
    get_indefinite_length_and_value(t, [h])
  end

  defp get_indefinite_length_and_value(<<0, 0, rest::binary>>, acc) do
    {:ok, {:erlang.list_to_binary(:lists.reverse(acc)), rest}}
  end

  defp get_indefinite_length_and_value(binary, acc) do
    {:ok, {tag, restBinary}} = get_tag(binary)
    {:ok, {lenVal, restBinary2}} = get_length_and_value(restBinary)

    get_indefinite_length_and_value(
      restBinary2,
      [lenVal, tag | acc]
    )
  end

  def match_tags({t, v}, [t]) do
    v
  end

  def match_tags({t, v}, [t | tt]) do
    match_tags(v, tt)
  end

  def match_tags([{t, v}], [t | tt]) do
    match_tags(v, tt)
  end

  def match_tags([{t, _V} | _] = vlist, [t]) do
    vlist
  end

  def match_tags(tlv, []) do
    tlv
  end

  def match_tags({tag, _V} = tlv, [t | _Tt]) do
    exit({:error, {:asn1, {:wrong_tag, {{:expected, t}, {:got, tag, tlv}}}}})
  end

  def skip_ExtensionAdditions([], _Tags) do
    []
  end

  def skip_ExtensionAdditions([{tag, _} | rest] = tLV, tags) do
    case (for x = t <- tags, t === tag do
            x
          end) do
      [] ->
        skip_ExtensionAdditions(rest, tags)

      _ ->
        tLV
    end
  end

  defp decode_tag_and_length(
         <<class::size(2), form::size(1), tagNo::size(5), 0::size(1), length::size(7),
           v::size(length)-binary, restBuffer::binary>>
       )
       when tagNo < 31 do
    {form, class <<< 16 ||| tagNo, v, restBuffer}
  end

  defp decode_tag_and_length(
         <<class::size(2), 1::size(1), tagNo::size(5), 1::size(1), 0::size(7), t::binary>>
       )
       when tagNo < 31 do
    {2, class <<< (16 + tagNo), t, <<>>}
  end

  defp decode_tag_and_length(
         <<class::size(2), form::size(1), tagNo::size(5), 1::size(1), lL::size(7),
           length::size(lL)-unit(8), v::size(length)-binary, t::binary>>
       )
       when tagNo < 31 do
    {form, class <<< 16 ||| tagNo, v, t}
  end

  defp decode_tag_and_length(
         <<class::size(2), form::size(1), 31::size(5), 0::size(1), tagNo::size(7), 0::size(1),
           length::size(7), v::size(length)-binary, restBuffer::binary>>
       ) do
    {form, class <<< 16 ||| tagNo, v, restBuffer}
  end

  defp decode_tag_and_length(
         <<class::size(2), 1::size(1), 31::size(5), 0::size(1), tagNo::size(7), 1::size(1),
           0::size(7), t::binary>>
       ) do
    {2, class <<< 16 ||| tagNo, t, <<>>}
  end

  defp decode_tag_and_length(
         <<class::size(2), form::size(1), 31::size(5), 0::size(1), tagNo::size(7), 1::size(1),
           lL::size(7), length::size(lL)-unit(8), v::size(length)-binary, t::binary>>
       ) do
    {form, class <<< 16 ||| tagNo, v, t}
  end

  defp decode_tag_and_length(
         <<class::size(2), form::size(1), 31::size(5), 1::size(1), tagPart1::size(7), 0::size(1),
           tagPartLast, buffer::binary>>
       ) do
    tagNo = tagPart1 <<< 7 ||| tagPartLast
    {length, restBuffer} = decode_length(buffer)
    <<v::size(length)-binary, restBuffer2::binary>> = restBuffer
    {form, class <<< 16 ||| tagNo, v, restBuffer2}
  end

  defp decode_tag_and_length(<<class::size(2), form::size(1), 31::size(5), buffer::binary>>) do
    {tagNo, buffer1} = decode_tag(buffer, 0)
    {length, restBuffer} = decode_length(buffer1)
    <<v::size(length)-binary, restBuffer2::binary>> = restBuffer
    {form, class <<< 16 ||| tagNo, v, restBuffer2}
  end

  defp decode_tag(
         <<0::size(1), partialTag::size(7), buffer::binary>>,
         tagAck
       ) do
    tagNo = tagAck <<< 7 ||| partialTag
    {tagNo, buffer}
  end

  defp decode_tag(
         <<_::size(1), partialTag::size(7), buffer::binary>>,
         tagAck
       ) do
    tagAck1 = tagAck <<< 7 ||| partialTag
    decode_tag(buffer, tagAck1)
  end

  def encode_tags([tag | trest], bytesSoFar, lenSoFar) do
    {bytes2, l2} = encode_length(lenSoFar)
    encode_tags(trest, [tag, bytes2 | bytesSoFar], lenSoFar + byte_size(tag) + l2)
  end

  def encode_tags([], bytesSoFar, lenSoFar) do
    {bytesSoFar, lenSoFar}
  end

  defp encode_tags(tagIn, {bytesSoFar, lenSoFar}) do
    encode_tags(tagIn, bytesSoFar, lenSoFar)
  end

  def encode_open_type(val, t) when is_list(val) do
    encode_open_type(:erlang.list_to_binary(val), t)
  end

  def encode_open_type(val, tag) do
    encode_tags(tag, val, byte_size(val))
  end

  def decode_open_type(tlv, tagIn) do
    case match_tags(tlv, tagIn) do
      bin when is_binary(bin) ->
        {innerTlv, _} = ber_decode_nif(bin)
        innerTlv

      tlvBytes ->
        tlvBytes
    end
  end

  def decode_open_type_as_binary(tlv, tagIn) do
    ber_encode(match_tags(tlv, tagIn))
  end

  def encode_boolean(true, tagIn) do
    encode_tags(tagIn, [255], 1)
  end

  def encode_boolean(false, tagIn) do
    encode_tags(tagIn, [0], 1)
  end

  def encode_boolean(x, _) do
    exit({:error, {:asn1, {:encode_boolean, x}}})
  end

  def decode_boolean(tlv, tagIn) do
    val = match_tags(tlv, tagIn)

    case val do
      <<0::size(8)>> ->
        false

      <<_::size(8)>> ->
        true

      _ ->
        exit({:error, {:asn1, {:decode_boolean, val}}})
    end
  end

  def encode_integer(val, tag) when is_integer(val) do
    encode_tags(tag, encode_integer(val))
  end

  def encode_integer(val, _Tag) do
    exit({:error, {:asn1, {:encode_integer, val}}})
  end

  def encode_integer(val, namedNumberList, tag) when is_atom(val) do
    case :lists.keyfind(val, 1, namedNumberList) do
      {_, newVal} ->
        encode_tags(tag, encode_integer(newVal))

      _ ->
        exit({:error, {:asn1, {:encode_integer_namednumber, val}}})
    end
  end

  def encode_integer(val, _NamedNumberList, tag) do
    encode_tags(tag, encode_integer(val))
  end

  defp encode_integer(val) do
    bytes =
      cond do
        val >= 0 ->
          encode_integer_pos(val, [])

        true ->
          encode_integer_neg(val, [])
      end

    {bytes, length(bytes)}
  end

  defp encode_integer_pos(0, [b | _Acc] = l) when b < 128 do
    l
  end

  defp encode_integer_pos(n, acc) do
    encode_integer_pos(n >>> 8, [n &&& 255 | acc])
  end

  defp encode_integer_neg(-1, [b1 | _T] = l) when b1 > 127 do
    l
  end

  defp encode_integer_neg(n, acc) do
    encode_integer_neg(n >>> 8, [n &&& 255 | acc])
  end

  def decode_integer(tlv, tagIn) do
    bin = match_tags(tlv, tagIn)
    len = byte_size(bin)
    <<int::size(len)-signed-unit(8)>> = bin
    int
  end

  def number2name(int, namedNumberList) do
    case :lists.keyfind(int, 2, namedNumberList) do
      {namedVal, _} ->
        namedVal

      _ ->
        int
    end
  end

  def encode_unnamed_bit_string(bits, tagIn) do
    unused = 8 - bit_size(bits) &&& 7 &&& 7
    bin = <<unused, bits::bitstring, 0::size(unused)>>
    encode_tags(tagIn, bin, byte_size(bin))
  end

  def encode_unnamed_bit_string(maxBits, bits, tagIn) do
    numBits = bit_size(bits)
    unused = 8 - numBits &&& 7 &&& 7
    bin = <<unused, bits::bitstring, 0::size(unused)>>

    cond do
      numBits > maxBits ->
        exit({:error, {:asn1, {:bitstring_length, {{:was, numBits}, {:maximum, maxBits}}}}})

      true ->
        encode_tags(tagIn, bin, byte_size(bin))
    end
  end

  def encode_named_bit_string([h | _] = bits, namedBitList, tagIn)
      when is_atom(h) do
    do_encode_named_bit_string(bits, namedBitList, tagIn)
  end

  def encode_named_bit_string([{:bit, _} | _] = bits, namedBitList, tagIn) do
    do_encode_named_bit_string(bits, namedBitList, tagIn)
  end

  def encode_named_bit_string([], _NamedBitList, tagIn) do
    encode_unnamed_bit_string(<<>>, tagIn)
  end

  def encode_named_bit_string(bits, _NamedBitList, tagIn)
      when is_bitstring(bits) do
    encode_unnamed_bit_string(bits, tagIn)
  end

  def encode_named_bit_string(c, [h | _] = bits, namedBitList, tagIn)
      when is_atom(h) do
    do_encode_named_bit_string(c, bits, namedBitList, tagIn)
  end

  def encode_named_bit_string(c, [{:bit, _} | _] = bits, namedBitList, tagIn) do
    do_encode_named_bit_string(c, bits, namedBitList, tagIn)
  end

  def encode_named_bit_string(c, [], _NamedBitList, tagIn) do
    encode_unnamed_bit_string(c, <<>>, tagIn)
  end

  def encode_named_bit_string(c, bits, _NamedBitList, tagIn)
      when is_bitstring(bits) do
    encode_unnamed_bit_string(c, bits, tagIn)
  end

  defp do_encode_named_bit_string([firstVal | restVal], namedBitList, tagIn) do
    toSetPos = get_all_bitposes([firstVal | restVal], namedBitList, [])
    size = :lists.max(toSetPos) + 1
    bitList = make_and_set_list(size, toSetPos, 0)
    {len, unused, octetList} = encode_bitstring(bitList)
    encode_tags(tagIn, [unused | octetList], len + 1)
  end

  defp do_encode_named_bit_string(size, [firstVal | restVal], namedBitList, tagIn) do
    toSetPos = get_all_bitposes([firstVal | restVal], namedBitList, [])
    bitList = make_and_set_list(size, toSetPos, 0)
    {len, unused, octetList} = encode_bitstring(bitList)
    encode_tags(tagIn, [unused | octetList], len + 1)
  end

  def encode_bit_string(c, bits, namedBitList, tagIn)
      when is_bitstring(bits) do
    padLen = 8 - bit_size(bits) &&& 7 &&& 7
    compact = {padLen, <<bits::bitstring, 0::size(padLen)>>}
    encode_bin_bit_string(c, compact, namedBitList, tagIn)
  end

  def encode_bit_string(c, bin = {unused, binBits}, namedBitList, tagIn)
      when is_integer(unused) and is_binary(binBits) do
    encode_bin_bit_string(c, bin, namedBitList, tagIn)
  end

  def encode_bit_string(c, [firstVal | restVal], namedBitList, tagIn)
      when is_atom(firstVal) do
    encode_bit_string_named(c, [firstVal | restVal], namedBitList, tagIn)
  end

  def encode_bit_string(c, [{:bit, x} | restVal], namedBitList, tagIn) do
    encode_bit_string_named(c, [{:bit, x} | restVal], namedBitList, tagIn)
  end

  def encode_bit_string(c, [firstVal | restVal], namedBitList, tagIn)
      when is_integer(firstVal) do
    encode_bit_string_bits(c, [firstVal | restVal], namedBitList, tagIn)
  end

  def encode_bit_string(_C, 0, _NamedBitList, tagIn) do
    encode_tags(tagIn, <<0>>, 1)
  end

  def encode_bit_string(_C, [], _NamedBitList, tagIn) do
    encode_tags(tagIn, <<0>>, 1)
  end

  def encode_bit_string(c, integerVal, namedBitList, tagIn)
      when is_integer(integerVal) do
    bitListVal = int_to_bitlist(integerVal)
    encode_bit_string_bits(c, bitListVal, namedBitList, tagIn)
  end

  defp int_to_bitlist(0) do
    []
  end

  defp int_to_bitlist(int) when is_integer(int) and int >= 0 do
    [int &&& 1 | int_to_bitlist(int >>> 1)]
  end

  defp encode_bin_bit_string(c, {unused, binBits}, _NamedBitList, tagIn) do
    case c do
      [] ->
        remove_unused_then_dotag(tagIn, unused, binBits)

      {_Min, max} ->
        bBLen = byte_size(binBits) * 8 - unused

        cond do
          bBLen > max ->
            exit({:error, {:asn1, {:bitstring_length, {{:was, bBLen}, {:maximum, max}}}}})

          true ->
            remove_unused_then_dotag(tagIn, unused, binBits)
        end

      size ->
        case byte_size(binBits) * 8 - unused do
          bBSize when bBSize <= size ->
            remove_unused_then_dotag(tagIn, unused, binBits)

          bBSize ->
            exit({:error, {:asn1, {:bitstring_length, {{:was, bBSize}, {:should_be, size}}}}})
        end
    end
  end

  defp remove_unused_then_dotag(tagIn, unused, binBits) do
    case unused do
      0 when byte_size(binBits) === 0 ->
        encode_tags(tagIn, <<0>>, 1)

      0 ->
        bin = <<unused, binBits::binary>>
        encode_tags(tagIn, bin, byte_size(bin))

      num ->
        n = byte_size(binBits) - 1
        <<bBits::size(n)-binary, lastByte>> = binBits

        encode_tags(
          tagIn,
          [unused, :erlang.binary_to_list(bBits) ++ [lastByte >>> num <<< num]],
          1 + byte_size(binBits)
        )
    end
  end

  defp encode_bit_string_named(c, [firstVal | restVal], namedBitList, tagIn) do
    toSetPos = get_all_bitposes([firstVal | restVal], namedBitList, [])

    size =
      case c do
        [] ->
          :lists.max(toSetPos) + 1

        {_Min, max} ->
          max

        tSize ->
          tSize
      end

    bitList = make_and_set_list(size, toSetPos, 0)
    {len, unused, octetList} = encode_bitstring(bitList)
    encode_tags(tagIn, [unused | octetList], len + 1)
  end

  defp get_all_bitposes([{:bit, valPos} | rest], namedBitList, ack) do
    get_all_bitposes(rest, namedBitList, [valPos | ack])
  end

  defp get_all_bitposes([val | rest], namedBitList, ack)
       when is_atom(val) do
    case :lists.keyfind(val, 1, namedBitList) do
      {_ValName, valPos} ->
        get_all_bitposes(rest, namedBitList, [valPos | ack])

      _ ->
        exit({:error, {:asn1, {:bitstring_namedbit, val}}})
    end
  end

  defp get_all_bitposes([], _NamedBitList, ack) do
    :lists.sort(ack)
  end

  defp make_and_set_list(0, [], _) do
    []
  end

  defp make_and_set_list(0, _, _) do
    exit({:error, {:asn1, :bitstring_sizeconstraint}})
  end

  defp make_and_set_list(len, [xPos | setPos], xPos) do
    [1 | make_and_set_list(len - 1, setPos, xPos + 1)]
  end

  defp make_and_set_list(len, [pos | setPos], xPos) do
    [0 | make_and_set_list(len - 1, [pos | setPos], xPos + 1)]
  end

  defp make_and_set_list(len, [], xPos) do
    [0 | make_and_set_list(len - 1, [], xPos + 1)]
  end

  defp encode_bit_string_bits(c, bitListVal, _NamedBitList, tagIn)
       when is_list(bitListVal) do
    case c do
      [] ->
        {len, unused, octetList} = encode_bitstring(bitListVal)
        encode_tags(tagIn, [unused | octetList], len + 1)

      constr = {min, _Max} when is_integer(min) ->
        encode_constr_bit_str_bits(constr, bitListVal, tagIn)

      {constr = {_, _}, []} ->
        encode_constr_bit_str_bits(constr, bitListVal, tagIn)

      constr = {{_, _}, {_, _}} ->
        encode_constr_bit_str_bits(constr, bitListVal, tagIn)

      size when is_integer(size) ->
        case length(bitListVal) do
          bitSize when bitSize == size ->
            {len, unused, octetList} = encode_bitstring(bitListVal)
            encode_tags(tagIn, [unused | octetList], len + 1)

          bitSize when bitSize < size ->
            paddedList = pad_bit_list(size - bitSize, bitListVal)
            {len, unused, octetList} = encode_bitstring(paddedList)
            encode_tags(tagIn, [unused | octetList], len + 1)

          bitSize ->
            exit({:error, {:asn1, {:bitstring_length, {{:was, bitSize}, {:should_be, size}}}}})
        end
    end
  end

  defp encode_constr_bit_str_bits({{_Min1, max1}, {min2, max2}}, bitListVal, tagIn) do
    bitLen = length(bitListVal)

    case bitLen do
      len when len > max2 ->
        exit({:error, {:asn1, {:bitstring_length, {{:was, bitLen}, {:maximum, max2}}}}})

      len when len > max1 and len < min2 ->
        exit(
          {:error,
           {:asn1, {:bitstring_length, {{:was, bitLen}, {:not_allowed_interval, max1, min2}}}}}
        )

      _ ->
        {len, unused, octetList} = encode_bitstring(bitListVal)
        encode_tags(tagIn, [unused, octetList], len + 1)
    end
  end

  defp encode_constr_bit_str_bits({min, max}, bitListVal, tagIn) do
    bitLen = length(bitListVal)

    cond do
      bitLen > max ->
        exit({:error, {:asn1, {:bitstring_length, {{:was, bitLen}, {:maximum, max}}}}})

      bitLen < min ->
        exit({:error, {:asn1, {:bitstring_length, {{:was, bitLen}, {:minimum, max}}}}})

      true ->
        {len, unused, octetList} = encode_bitstring(bitListVal)
        encode_tags(tagIn, [unused, octetList], len + 1)
    end
  end

  defp pad_bit_list(size, bitListVal) do
    tail = :lists.duplicate(size, 0)
    :lists.append(bitListVal, tail)
  end

  defp encode_bitstring([b8, b7, b6, b5, b4, b3, b2, b1 | rest]) do
    val =
      b8 <<< 7 ||| b7 <<< 6 ||| b6 <<< 5 ||| b5 <<< 4 ||| b4 <<< 3 ||| b3 <<< 2 ||| b2 <<< 1 |||
        b1

    encode_bitstring(rest, [val], 1)
  end

  defp encode_bitstring(val) do
    {unused, octet} = unused_bitlist(val, 7, 0)
    {1, unused, [octet]}
  end

  defp encode_bitstring([b8, b7, b6, b5, b4, b3, b2, b1 | rest], ack, len) do
    val =
      b8 <<< 7 ||| b7 <<< 6 ||| b6 <<< 5 ||| b5 <<< 4 ||| b4 <<< 3 ||| b3 <<< 2 ||| b2 <<< 1 |||
        b1

    encode_bitstring(rest, [ack, val], len + 1)
  end

  defp encode_bitstring([], ack, len) do
    {len, 0, ack}
  end

  defp encode_bitstring(rest, ack, len) do
    {unused, val} = unused_bitlist(rest, 7, 0)
    {len + 1, unused, [ack, val]}
  end

  defp unused_bitlist([], trail, ack) do
    {trail + 1, ack}
  end

  defp unused_bitlist([bit | rest], trail, ack) do
    unused_bitlist(rest, trail - 1, bit <<< trail ||| ack)
  end

  def decode_compact_bit_string(buffer, tags) do
    case match_and_collect(buffer, tags) do
      <<0>> ->
        {0, <<>>}

      <<unused, bits::binary>> ->
        {unused, bits}
    end
  end

  def compact_bit_string_size({unused, bits}) do
    bit_size(bits) - unused
  end

  def decode_native_bit_string(buffer, tags) do
    case match_and_collect(buffer, tags) do
      <<0>> ->
        <<>>

      <<unused, bits::binary>> ->
        size = bit_size(bits) - unused
        <<val::size(size)-bitstring, _::size(unused)-bitstring>> = bits
        val
    end
  end

  def decode_named_bit_string(buffer, namedNumberList, tags) do
    case match_and_collect(buffer, tags) do
      <<0>> ->
        []

      <<unused, bits::binary>> ->
        bitString = decode_bitstring2(byte_size(bits), unused, bits)
        decode_bitstring_NNL(bitString, namedNumberList)
    end
  end

  defp decode_bitstring2(
         1,
         unused,
         <<b7::size(1), b6::size(1), b5::size(1), b4::size(1), b3::size(1), b2::size(1),
           b1::size(1), b0::size(1), _::binary>>
       ) do
    :lists.sublist(
      [b7, b6, b5, b4, b3, b2, b1, b0],
      8 - unused
    )
  end

  defp decode_bitstring2(
         len,
         unused,
         <<b7::size(1), b6::size(1), b5::size(1), b4::size(1), b3::size(1), b2::size(1),
           b1::size(1), b0::size(1), buffer::binary>>
       ) do
    [
      b7,
      b6,
      b5,
      b4,
      b3,
      b2,
      b1,
      b0
      | decode_bitstring2(len - 1, unused, buffer)
    ]
  end

  def native_to_legacy_bit_string(bits) do
    for <<(<<b::size(1)>> <- bits)>> do
      b
    end
  end

  defp decode_bitstring_NNL(bitList, namedNumberList) do
    decode_bitstring_NNL(bitList, namedNumberList, 0, [])
  end

  defp decode_bitstring_NNL([], _, _No, result) do
    :lists.reverse(result)
  end

  defp decode_bitstring_NNL([b | bitList], [{name, no} | namedNumberList], no, result) do
    cond do
      b === 0 ->
        decode_bitstring_NNL(bitList, namedNumberList, no + 1, result)

      true ->
        decode_bitstring_NNL(bitList, namedNumberList, no + 1, [name | result])
    end
  end

  defp decode_bitstring_NNL([1 | bitList], namedNumberList, no, result) do
    decode_bitstring_NNL(bitList, namedNumberList, no + 1, [{:bit, no} | result])
  end

  defp decode_bitstring_NNL([0 | bitList], namedNumberList, no, result) do
    decode_bitstring_NNL(bitList, namedNumberList, no + 1, result)
  end

  def encode_null(_Val, tagIn) do
    encode_tags(tagIn, [], 0)
  end

  def decode_null(tlv, tags) do
    val = match_tags(tlv, tags)

    case val do
      <<>> ->
        :NULL

      _ ->
        exit({:error, {:asn1, {:decode_null, val}}})
    end
  end

  def encode_object_identifier(val, tagIn) do
    encode_tags(tagIn, e_object_identifier(val))
  end

  defp e_object_identifier({:"OBJECT IDENTIFIER", v}) do
    e_object_identifier(v)
  end

  defp e_object_identifier(v) when is_tuple(v) do
    e_object_identifier(:erlang.tuple_to_list(v))
  end

  defp e_object_identifier([e1, e2 | tail]) do
    head = 40 * e1 + e2
    {h, lh} = mk_object_val(head)
    {r, lr} = :lists.mapfoldl(&enc_obj_id_tail/2, 0, tail)
    {[h | r], lh + lr}
  end

  defp enc_obj_id_tail(h, len) do
    {b, l} = mk_object_val(h)
    {b, len + l}
  end

  defp mk_object_val(val) when val <= 127 do
    {[255 &&& val], 1}
  end

  defp mk_object_val(val) do
    mk_object_val(val >>> 7, [val &&& 127], 1)
  end

  defp mk_object_val(0, ack, len) do
    {ack, len}
  end

  defp mk_object_val(val, ack, len) do
    mk_object_val(val >>> 7, [(val &&& 127) ||| 128 | ack], len + 1)
  end

  def decode_object_identifier(tlv, tags) do
    val = match_tags(tlv, tags)
    [addedObjVal | objVals] = dec_subidentifiers(val, 0, [])

    {val1, val2} =
      cond do
        addedObjVal < 40 ->
          {0, addedObjVal}

        addedObjVal < 80 ->
          {1, addedObjVal - 40}

        true ->
          {2, addedObjVal - 80}
      end

    :erlang.list_to_tuple([val1, val2 | objVals])
  end

  defp dec_subidentifiers(<<>>, _Av, al) do
    :lists.reverse(al)
  end

  defp dec_subidentifiers(<<1::size(1), h::size(7), t::binary>>, av, al) do
    dec_subidentifiers(t, av <<< (7 + h), al)
  end

  defp dec_subidentifiers(<<h, t::binary>>, av, al) do
    dec_subidentifiers(t, 0, [av <<< (7 + h) | al])
  end

  def encode_relative_oid(val, tagIn) when is_tuple(val) do
    encode_relative_oid(:erlang.tuple_to_list(val), tagIn)
  end

  def encode_relative_oid(val, tagIn) do
    encode_tags(tagIn, enc_relative_oid(val))
  end

  defp enc_relative_oid(tuple) when is_tuple(tuple) do
    enc_relative_oid(:erlang.tuple_to_list(tuple))
  end

  defp enc_relative_oid(val) do
    :lists.mapfoldl(
      fn x, accIn ->
        {sO, l} = mk_object_val(x)
        {sO, l + accIn}
      end,
      0,
      val
    )
  end

  def decode_relative_oid(tlv, tags) do
    val = match_tags(tlv, tags)
    objVals = dec_subidentifiers(val, 0, [])
    :erlang.list_to_tuple(objVals)
  end

  def encode_restricted_string(octetList, tagIn) when is_binary(octetList) do
    encode_tags(tagIn, octetList, byte_size(octetList))
  end

  def encode_restricted_string(octetList, tagIn) when is_list(octetList) do
    encode_tags(tagIn, octetList, length(octetList))
  end

  def decode_octet_string(tlv, tagsIn) do
    bin = match_and_collect(tlv, tagsIn)
    :binary.copy(bin)
  end

  def decode_restricted_string(tlv, tagsIn) do
    match_and_collect(tlv, tagsIn)
  end

  def encode_universal_string(universal, tagIn) do
    octetList = mk_uni_list(universal)
    encode_tags(tagIn, octetList, length(octetList))
  end

  defp mk_uni_list(in__) do
    mk_uni_list(in__, [])
  end

  defp mk_uni_list([], list) do
    :lists.reverse(list)
  end

  defp mk_uni_list([{a, b, c, d} | t], list) do
    mk_uni_list(t, [d, c, b, a | list])
  end

  defp mk_uni_list([h | t], list) do
    mk_uni_list(t, [h, 0, 0, 0 | list])
  end

  def decode_universal_string(buffer, tags) do
    bin = match_and_collect(buffer, tags)
    mk_universal_string(:erlang.binary_to_list(bin))
  end

  defp mk_universal_string(in__) do
    mk_universal_string(in__, [])
  end

  defp mk_universal_string([], acc) do
    :lists.reverse(acc)
  end

  defp mk_universal_string([0, 0, 0, d | t], acc) do
    mk_universal_string(t, [d | acc])
  end

  defp mk_universal_string([a, b, c, d | t], acc) do
    mk_universal_string(t, [{a, b, c, d} | acc])
  end

  def encode_UTF8_string(uTF8String, tagIn) when is_binary(uTF8String) do
    encode_tags(tagIn, uTF8String, byte_size(uTF8String))
  end

  def encode_UTF8_string(uTF8String, tagIn) do
    encode_tags(tagIn, uTF8String, length(uTF8String))
  end

  def decode_UTF8_string(tlv, tagsIn) do
    val = match_tags(tlv, tagsIn)

    case val do
      [_ | _] = partList ->
        collect_parts(partList)

      bin ->
        bin
    end
  end

  def encode_BMP_string(bMPString, tagIn) do
    octetList = mk_BMP_list(bMPString)
    encode_tags(tagIn, octetList, length(octetList))
  end

  defp mk_BMP_list(in__) do
    mk_BMP_list(in__, [])
  end

  defp mk_BMP_list([], list) do
    :lists.reverse(list)
  end

  defp mk_BMP_list([{0, 0, c, d} | t], list) do
    mk_BMP_list(t, [d, c | list])
  end

  defp mk_BMP_list([h | t], list) do
    mk_BMP_list(t, [h, 0 | list])
  end

  def decode_BMP_string(buffer, tags) do
    bin = match_and_collect(buffer, tags)
    mk_BMP_string(:erlang.binary_to_list(bin))
  end

  defp mk_BMP_string(in__) do
    mk_BMP_string(in__, [])
  end

  defp mk_BMP_string([], uS) do
    :lists.reverse(uS)
  end

  defp mk_BMP_string([0, b | t], uS) do
    mk_BMP_string(t, [b | uS])
  end

  defp mk_BMP_string([c, d | t], uS) do
    mk_BMP_string(t, [{0, 0, c, d} | uS])
  end

  defp encode_length(l) when l <= 127 do
    {[l], 1}
  end

  defp encode_length(l) do
    oct = minimum_octets(l)
    len = length(oct)

    cond do
      len <= 126 ->
        {[128 ||| len | oct], len + 1}

      true ->
        exit({:error, {:asn1, :too_long_length_oct, len}})
    end
  end

  defp minimum_octets(val) do
    minimum_octets(val, [])
  end

  defp minimum_octets(0, acc) do
    acc
  end

  defp minimum_octets(val, acc) do
    minimum_octets(val >>> 8, [val &&& 255 | acc])
  end

  defp decode_length(<<1::size(1), 0::size(7), t::binary>>) do
    {:indefinite, t}
  end

  defp decode_length(<<0::size(1), length::size(7), t::binary>>) do
    {length, t}
  end

  defp decode_length(<<1::size(1), lL::size(7), length::size(lL)-unit(8), t::binary>>) do
    {length, t}
  end

  def dynamicsort_SET_components(listOfEncCs) do
    tagBinL =
      for l <- listOfEncCs do
        bin = :erlang.list_to_binary(l)
        {dynsort_decode_tag(bin), bin}
      end

    for {_, e} <- :lists.keysort(1, tagBinL) do
      e
    end
  end

  def dynamicsort_SETOF(listOfEncVal) do
    binL =
      :lists.map(
        fn
          l when is_list(l) ->
            :erlang.list_to_binary(l)

          b ->
            b
        end,
        listOfEncVal
      )

    :lists.sort(binL)
  end

  defp dynsort_decode_tag(<<class::size(2), _Form::size(1), 31::size(5), buffer::binary>>) do
    tagNum = dynsort_decode_tag(buffer, 0)
    {class, tagNum}
  end

  defp dynsort_decode_tag(<<class::size(2), _Form::size(1), tagNum::size(5), _::binary>>) do
    {class, tagNum}
  end

  defp dynsort_decode_tag(
         <<0::size(1), partialTag::size(7), _::binary>>,
         tagAcc
       ) do
    tagAcc <<< 7 ||| partialTag
  end

  defp dynsort_decode_tag(
         <<_::size(1), partialTag::size(7), buffer::binary>>,
         tagAcc0
       ) do
    tagAcc = tagAcc0 <<< 7 ||| partialTag
    dynsort_decode_tag(buffer, tagAcc)
  end

  defp match_and_collect(tlv, tagsIn) do
    val = match_tags(tlv, tagsIn)

    case val do
      [_ | _] = partList ->
        collect_parts(partList)

      bin when is_binary(bin) ->
        bin
    end
  end

  defp collect_parts(tlvList) do
    collect_parts(tlvList, [])
  end

  defp collect_parts([{_, l} | rest], acc) when is_list(l) do
    collect_parts(rest, [collect_parts(l) | acc])
  end

  defp collect_parts(
         [{3, <<unused, bits::binary>>} | rest],
         _Acc
       ) do
    collect_parts_bit(rest, [bits], unused)
  end

  defp collect_parts([{_T, v} | rest], acc) do
    collect_parts(rest, [v | acc])
  end

  defp collect_parts([], acc) do
    :erlang.list_to_binary(:lists.reverse(acc))
  end

  defp collect_parts_bit([{3, <<unused, bits::binary>>} | rest], acc, uacc) do
    collect_parts_bit(rest, [bits | acc], unused + uacc)
  end

  defp collect_parts_bit([], acc, uacc) do
    :erlang.list_to_binary([uacc | :lists.reverse(acc)])
  end
end
