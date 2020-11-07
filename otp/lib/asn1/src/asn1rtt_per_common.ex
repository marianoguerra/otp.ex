defmodule :m_asn1rtt_per_common do
  use Bitwise
  require Record

  Record.defrecord(:r_module, :module,
    pos: :undefined,
    name: :undefined,
    defid: :undefined,
    tagdefault: :EXPLICIT,
    exports: {:exports, []},
    imports: {:imports, []},
    extensiondefault: :empty,
    typeorval: :undefined
  )

  Record.defrecord(:r_ExtensionAdditionGroup, :ExtensionAdditionGroup, number: :undefined)

  Record.defrecord(:r_SEQUENCE, :SEQUENCE,
    pname: false,
    tablecinf: false,
    extaddgroup: :undefined,
    components: []
  )

  Record.defrecord(:r_SET, :SET, pname: false, sorted: false, tablecinf: false, components: [])

  Record.defrecord(:r_ComponentType, :ComponentType,
    pos: :undefined,
    name: :undefined,
    typespec: :undefined,
    prop: :undefined,
    tags: :undefined,
    textual_order: :undefined
  )

  Record.defrecord(:r_ObjectClassFieldType, :ObjectClassFieldType,
    classname: :undefined,
    class: :undefined,
    fieldname: :undefined,
    type: :undefined
  )

  Record.defrecord(:r_typedef, :typedef,
    checked: false,
    pos: :undefined,
    name: :undefined,
    typespec: :undefined
  )

  Record.defrecord(:r_classdef, :classdef,
    checked: false,
    pos: :undefined,
    name: :undefined,
    module: :undefined,
    typespec: :undefined
  )

  Record.defrecord(:r_valuedef, :valuedef,
    checked: false,
    pos: :undefined,
    name: :undefined,
    type: :undefined,
    value: :undefined,
    module: :undefined
  )

  Record.defrecord(:r_ptypedef, :ptypedef,
    checked: false,
    pos: :undefined,
    name: :undefined,
    args: :undefined,
    typespec: :undefined
  )

  Record.defrecord(:r_pvaluedef, :pvaluedef,
    checked: false,
    pos: :undefined,
    name: :undefined,
    args: :undefined,
    type: :undefined,
    value: :undefined
  )

  Record.defrecord(:r_pvaluesetdef, :pvaluesetdef,
    checked: false,
    pos: :undefined,
    name: :undefined,
    args: :undefined,
    type: :undefined,
    valueset: :undefined
  )

  Record.defrecord(:r_pobjectdef, :pobjectdef,
    checked: false,
    pos: :undefined,
    name: :undefined,
    args: :undefined,
    class: :undefined,
    def: :undefined
  )

  Record.defrecord(:r_pobjectsetdef, :pobjectsetdef,
    checked: false,
    pos: :undefined,
    name: :undefined,
    args: :undefined,
    class: :undefined,
    def: :undefined
  )

  Record.defrecord(:r_Constraint, :Constraint,
    SingleValue: :no,
    SizeConstraint: :no,
    ValueRange: :no,
    PermittedAlphabet: :no,
    ContainedSubtype: :no,
    TypeConstraint: :no,
    InnerSubtyping: :no,
    e: :no,
    Other: :no
  )

  Record.defrecord(:r_simpletableattributes, :simpletableattributes,
    objectsetname: :undefined,
    c_name: :undefined,
    c_index: :undefined,
    usedclassfield: :undefined,
    uniqueclassfield: :undefined,
    valueindex: :undefined
  )

  Record.defrecord(:r_type, :type,
    tag: [],
    def: :undefined,
    constraint: [],
    tablecinf: [],
    inlined: :no
  )

  Record.defrecord(:r_objectclass, :objectclass,
    fields: [],
    syntax: :undefined
  )

  Record.defrecord(:r_Object, :Object, classname: :undefined, gen: true, def: :undefined)

  Record.defrecord(:r_ObjectSet, :ObjectSet,
    class: :undefined,
    gen: true,
    uniquefname: :undefined,
    set: :undefined
  )

  Record.defrecord(:r_tag, :tag, class: :undefined, number: :undefined, type: :undefined, form: 32)

  Record.defrecord(:r_cmap, :cmap,
    single_value: :no,
    contained_subtype: :no,
    value_range: :no,
    size: :no,
    permitted_alphabet: :no,
    type_constraint: :no,
    inner_subtyping: :no
  )

  Record.defrecord(:r_EXTENSIONMARK, :EXTENSIONMARK,
    pos: :undefined,
    val: :undefined
  )

  Record.defrecord(:r_SymbolsFromModule, :SymbolsFromModule,
    symbols: :undefined,
    module: :undefined,
    objid: :undefined
  )

  Record.defrecord(:r_Externaltypereference, :Externaltypereference,
    pos: :undefined,
    module: :undefined,
    type: :undefined
  )

  Record.defrecord(:r_Externalvaluereference, :Externalvaluereference,
    pos: :undefined,
    module: :undefined,
    value: :undefined
  )

  Record.defrecord(:r_seqtag, :seqtag, pos: :undefined, module: :undefined, val: :undefined)

  Record.defrecord(:r_state, :state,
    module: :undefined,
    mname: :undefined,
    tname: :undefined,
    erule: :undefined,
    parameters: [],
    inputmodules: [],
    abscomppath: [],
    recordtopname: [],
    options: :undefined,
    sourcedir: :undefined,
    error_context: :undefined
  )

  Record.defrecord(:r_gen, :gen,
    erule: :ber,
    der: false,
    jer: false,
    aligned: false,
    rec_prefix: '',
    macro_prefix: '',
    pack: :record,
    options: []
  )

  Record.defrecord(:r_abst, :abst,
    name: :undefined,
    types: :undefined,
    values: :undefined,
    ptypes: :undefined,
    classes: :undefined,
    objects: :undefined,
    objsets: :undefined
  )

  Record.defrecord(:r_gen_state, :gen_state,
    active: false,
    prefix: :undefined,
    inc_tag_pattern: :undefined,
    tag_pattern: :undefined,
    inc_type_pattern: :undefined,
    type_pattern: :undefined,
    func_name: :undefined,
    namelist: :undefined,
    tobe_refed_funcs: [],
    gen_refed_funcs: [],
    generated_functions: [],
    suffix_index: 1,
    current_suffix_index: :undefined
  )

  def decode_fragmented(segSz0, buf0, unit) do
    segSz = segSz0 * unit * 16384
    <<res::size(segSz)-bitstring, buf::bitstring>> = buf0
    decode_fragmented_1(buf, unit, res)
  end

  defp decode_fragmented_1(<<0::size(1), n::size(7), buf0::bitstring>>, unit, res) do
    sz = n * unit
    <<s::size(sz)-bitstring, buf::bitstring>> = buf0
    {<<res::bitstring, s::bitstring>>, buf}
  end

  defp decode_fragmented_1(<<1::size(1), 0::size(1), n::size(14), buf0::bitstring>>, unit, res) do
    sz = n * unit
    <<s::size(sz)-bitstring, buf::bitstring>> = buf0
    {<<res::bitstring, s::bitstring>>, buf}
  end

  defp decode_fragmented_1(
         <<1::size(1), 1::size(1), segSz0::size(6), buf0::bitstring>>,
         unit,
         res0
       ) do
    segSz = segSz0 * unit * 16384
    <<frag::size(segSz)-bitstring, buf::bitstring>> = buf0
    res = <<res0::bitstring, frag::bitstring>>
    decode_fragmented_1(buf, unit, res)
  end

  def decode_named_bit_string(val, nNL) do
    bits =
      for <<(<<b::size(1)>> <- val)>> do
        b
      end

    decode_named_bit_string_1(0, bits, nNL, [])
  end

  def decode_legacy_bit_string(val) do
    for <<(<<b::size(1)>> <- val)>> do
      b
    end
  end

  def decode_compact_bit_string(val) do
    padLen = 8 - bit_size(val) &&& 7 &&& 7
    {padLen, <<val::bitstring, 0::size(padLen)>>}
  end

  def decode_chars(val, n) do
    for <<(<<c::size(n)>> <- val)>> do
      c
    end
  end

  def decode_chars(val, n, chars) do
    for <<(<<c::size(n)>> <- val)>> do
      :erlang.element(c + 1, chars)
    end
  end

  def decode_chars_16bit(val) do
    cs =
      for <<(<<c::size(16)>> <- val)>> do
        c
      end

    decode_chars_16bit_1(cs)
  end

  def decode_big_chars(val, n) do
    decode_big_chars_1(decode_chars(val, n))
  end

  def decode_oid(octets) do
    [first | rest] = dec_subidentifiers(octets, 0, [])

    idlist =
      cond do
        first < 40 ->
          [[0, first] | rest]

        first < 80 ->
          [[1, first - 40] | rest]

        true ->
          [[2, first - 80] | rest]
      end

    :erlang.list_to_tuple(idlist)
  end

  def decode_relative_oid(octets) do
    :erlang.list_to_tuple(dec_subidentifiers(octets, 0, []))
  end

  def encode_chars(val, numBits) do
    for c <- val, into: <<>> do
      <<c::size(numBits)>>
    end
  end

  def encode_chars(val, numBits, {lb, tab}) do
    for c <- val, into: <<>> do
      <<enc_char(c, lb, tab)::size(numBits)>>
    end
  end

  def encode_chars_compact_map(val, numBits, {lb, limit}) do
    for c <- val, into: <<>> do
      <<enc_char_cm(c, lb, limit)::size(numBits)>>
    end
  end

  def encode_chars_16bit(val) do
    l =
      for c <- val do
        case c do
          {0, 0, a, b} ->
            [a, b]

          ^c when is_integer(c) ->
            [0, c]
        end
      end

    :erlang.iolist_to_binary(l)
  end

  def encode_big_chars(val) do
    l =
      for c <- val do
        case c do
          {_, _, _, _} ->
            :erlang.tuple_to_list(c)

          ^c when is_integer(c) ->
            [<<0, 0, 0>>, c]
        end
      end

    :erlang.iolist_to_binary(l)
  end

  def encode_fragmented(bin, unit) do
    encode_fragmented_1(bin, unit, 4)
  end

  def encode_oid(val) when is_tuple(val) do
    encode_oid(:erlang.tuple_to_list(val))
  end

  def encode_oid(val) do
    :erlang.iolist_to_binary(e_object_identifier(val))
  end

  def encode_relative_oid(val) when is_tuple(val) do
    encode_relative_oid(:erlang.tuple_to_list(val))
  end

  def encode_relative_oid(val) when is_list(val) do
    :erlang.list_to_binary(
      for x <- val do
        e_object_element(x)
      end
    )
  end

  def encode_unconstrained_number(val) when not is_integer(val) do
    exit({:error, {:asn1, {:illegal_integer, val}}})
  end

  def encode_unconstrained_number(val) when val >= 0 do
    cond do
      val < 128 ->
        [1, val]

      val < 256 ->
        [<<2, 0>>, val]

      true ->
        case :binary.encode_unsigned(val) do
          <<0::size(1), _::bitstring>> = bin ->
            case byte_size(bin) do
              sz when sz < 128 ->
                [sz, bin]

              sz when sz < 16384 ->
                [<<2::size(2), sz::size(14)>>, bin]
            end

          <<1::size(1), _::bitstring>> = bin ->
            case byte_size(bin) + 1 do
              sz when sz < 128 ->
                [sz, 0, bin]

              sz when sz < 16384 ->
                [<<2::size(2), sz::size(14), 0::size(8)>>, bin]
            end
        end
    end
  end

  def encode_unconstrained_number(val) do
    oct = enint(val, [])
    len = length(oct)

    cond do
      len < 128 ->
        [len | oct]

      len < 16384 ->
        [<<2::size(2), len::size(14)>> | oct]
    end
  end

  def bitstring_from_positions([]) do
    <<>>
  end

  def bitstring_from_positions([_ | _] = l0) do
    l1 = :lists.sort(l0)
    l = diff(l1, -1)

    for n <- l, into: <<>> do
      <<1::size(n + 0)>>
    end
  end

  def bitstring_from_positions(l0, lb) do
    l1 = :lists.sort(l0)
    l = diff(l1, -1, lb - 1)

    for {b, n} <- l, into: <<>> do
      <<b::size(n + 0)>>
    end
  end

  def to_bitstring({0, bs}) when is_binary(bs) do
    bs
  end

  def to_bitstring({unused, bs0}) when is_binary(bs0) do
    sz = bit_size(bs0) - unused
    <<bs::size(sz)-bits, _::bits>> = bs0
    bs
  end

  def to_bitstring(bs) when is_bitstring(bs) do
    bs
  end

  def to_bitstring(int) when is_integer(int) and int >= 0 do
    l = int_to_bitlist(int)

    for b <- l, into: <<>> do
      <<b::size(1)>>
    end
  end

  def to_bitstring(l) when is_list(l) do
    for b <- l, into: <<>> do
      <<b::size(1)>>
    end
  end

  def to_bitstring({0, bs}, lb) when is_binary(bs) do
    case bit_size(bs) do
      sz when sz < lb ->
        <<bs::bits, 0::size(lb - sz)>>

      _ ->
        bs
    end
  end

  def to_bitstring({unused, bs0}, lb) when is_binary(bs0) do
    sz = bit_size(bs0) - unused

    cond do
      sz < lb ->
        <<bs0::size(sz)-bits, 0::size(lb - sz)>>

      true ->
        <<bs::size(sz)-bits, _::bits>> = bs0
        bs
    end
  end

  def to_bitstring(bs, lb) when is_bitstring(bs) do
    adjust_size(bs, lb)
  end

  def to_bitstring(int, lb) when is_integer(int) and int >= 0 do
    l = int_to_bitlist(int)

    bs =
      for b <- l, into: <<>> do
        <<b::size(1)>>
      end

    adjust_size(bs, lb)
  end

  def to_bitstring(l, lb) when is_list(l) do
    bs =
      for b <- l, into: <<>> do
        <<b::size(1)>>
      end

    adjust_size(bs, lb)
  end

  def to_named_bitstring(val) do
    bs = to_bitstring(val)
    bs_drop_trailing_zeroes(bs)
  end

  def to_named_bitstring({0, bs}, lb) when is_binary(bs) do
    adjust_trailing_zeroes(bs, lb)
  end

  def to_named_bitstring({unused, bs0}, lb) when is_binary(bs0) do
    sz = bit_size(bs0) - unused
    <<bs::size(sz)-bits, _::bits>> = bs0
    adjust_trailing_zeroes(bs, lb)
  end

  def to_named_bitstring(bs, lb) when is_bitstring(bs) do
    adjust_trailing_zeroes(bs, lb)
  end

  def to_named_bitstring(val, lb) do
    adjust_trailing_zeroes(to_bitstring(val), lb)
  end

  def is_default_bitstring(:asn1_DEFAULT, _, _) do
    true
  end

  def is_default_bitstring(named, named, _) do
    true
  end

  def is_default_bitstring(bs, _, bs) do
    true
  end

  def is_default_bitstring(val, _, def__) when is_bitstring(val) do
    sz = bit_size(def__)

    case val do
      <<^def__::size(sz)-bitstring, t::bitstring>> ->
        numZeroes = bit_size(t)

        case t do
          <<0::size(numZeroes)>> ->
            true

          _ ->
            false
        end

      _ ->
        false
    end
  end

  def is_default_bitstring(:asn1_DEFAULT, _, _, _, _) do
    true
  end

  def is_default_bitstring({unused, bin}, v0, v1, v2, v3)
      when is_integer(unused) do
    sz = bit_size(bin) - unused
    <<bs::size(sz)-bitstring, _::size(unused)>> = bin
    is_default_bitstring(bs, v0, v1, v2, v3)
  end

  def is_default_bitstring(named, named, _, _, _) do
    true
  end

  def is_default_bitstring(bs, _, bs, _, _) do
    true
  end

  def is_default_bitstring(list, _, _, list, _) do
    true
  end

  def is_default_bitstring(int, _, _, _, int) do
    true
  end

  def is_default_bitstring(val, _, def__, _, _) when is_bitstring(val) do
    sz = bit_size(def__)

    case val do
      <<^def__::size(sz)-bitstring, t::bitstring>> ->
        numZeroes = bit_size(t)

        case t do
          <<0::size(numZeroes)>> ->
            true

          _ ->
            false
        end

      _ ->
        false
    end
  end

  def is_default_bitstring(val, _, _, list, _) when is_list(val) do
    is_default_bitstring_list(list, val)
  end

  def is_default_bitstring(_, _, _, _, _) do
    false
  end

  def extension_bitmap(val, pos, limit) do
    extension_bitmap(val, pos, limit, 0)
  end

  def open_type_to_binary({:asn1_OPENTYPE, bin}) when is_binary(bin) do
    bin
  end

  def legacy_open_type_to_binary({:asn1_OPENTYPE, bin}) when is_binary(bin) do
    bin
  end

  def legacy_open_type_to_binary(bin) when is_binary(bin) do
    bin
  end

  def legacy_open_type_to_binary(list) when is_list(list) do
    list
  end

  defp decode_named_bit_string_1(pos, [0 | bt], names, acc) do
    decode_named_bit_string_1(pos + 1, bt, names, acc)
  end

  defp decode_named_bit_string_1(pos, [1 | bt], names, acc) do
    case :lists.keyfind(pos, 2, names) do
      {name, _} ->
        decode_named_bit_string_1(pos + 1, bt, names, [name | acc])

      false ->
        decode_named_bit_string_1(pos + 1, bt, names, [{:bit, pos} | acc])
    end
  end

  defp decode_named_bit_string_1(_Pos, [], _Names, acc) do
    :lists.reverse(acc)
  end

  defp decode_chars_16bit_1([h | t]) when h < 256 do
    [h | decode_chars_16bit_1(t)]
  end

  defp decode_chars_16bit_1([h | t]) do
    [{0, 0, h >>> 8, h &&& 255} | decode_chars_16bit_1(t)]
  end

  defp decode_chars_16bit_1([]) do
    []
  end

  defp decode_big_chars_1([h | t]) when h < 256 do
    [h | decode_big_chars_1(t)]
  end

  defp decode_big_chars_1([h | t]) do
    [
      :erlang.list_to_tuple(:erlang.binary_to_list(<<h::size(32)>>))
      | decode_big_chars_1(t)
    ]
  end

  defp decode_big_chars_1([]) do
    []
  end

  defp dec_subidentifiers([h | t], av, al) when h >= 128 do
    dec_subidentifiers(t, av <<< 7 ||| (h &&& 127), al)
  end

  defp dec_subidentifiers([h | t], av, al) do
    dec_subidentifiers(t, 0, [av <<< 7 ||| h | al])
  end

  defp dec_subidentifiers([], _Av, al) do
    :lists.reverse(al)
  end

  defp enc_char(c0, lb, tab) do
    try do
      :erlang.element(c0 - lb, tab)
    catch
      :error, :badarg ->
        illegal_char_error()
    else
      :ill ->
        illegal_char_error()

      c ->
        c
    end
  end

  defp enc_char_cm(c0, lb, limit) do
    c = c0 - lb

    cond do
      0 <= c and c < limit ->
        c

      true ->
        illegal_char_error()
    end
  end

  defp illegal_char_error() do
    :erlang.error({:error, {:asn1, 'value forbidden by FROM constraint'}})
  end

  defp encode_fragmented_1(bin, unit, n) do
    segSz = unit * n * 16384

    case bin do
      <<b::size(segSz)-bitstring, t::bitstring>> ->
        [
          [<<3::size(2), n::size(6)>>, b]
          | encode_fragmented_1(t, unit, n)
        ]

      _ when n > 1 ->
        encode_fragmented_1(bin, unit, n - 1)

      _ ->
        case div(bit_size(bin), unit) do
          len when len < 128 ->
            [len, bin]

          len when len < 16384 ->
            [<<2::size(2), len::size(14)>>, bin]
        end
    end
  end

  defp e_object_identifier([[e1, e2] | tail])
       when (e1 >= 0 and e1 < 2 and
               e2 < 40) or
              e1 === 2 do
    head = 40 * e1 + e2
    e_object_elements([head | tail], [])
  end

  defp e_object_identifier([[_, _] | _Tail] = oid) do
    exit({:error, {:asn1, {:illegal_value, oid}}})
  end

  defp e_object_elements([], acc) do
    :lists.reverse(acc)
  end

  defp e_object_elements([h | t], acc) do
    e_object_elements(t, [e_object_element(h) | acc])
  end

  defp e_object_element(num) when num < 128 do
    [num]
  end

  defp e_object_element(num) do
    [e_o_e(num >>> 7), num &&& 127]
  end

  defp e_o_e(num) when num < 128 do
    num ||| 128
  end

  defp e_o_e(num) do
    [e_o_e(num >>> 7), (num &&& 127) ||| 128]
  end

  defp enint(-1, [b1 | t]) when b1 > 127 do
    [b1 | t]
  end

  defp enint(n, acc) do
    enint(n >>> 8, [n &&& 255 | acc])
  end

  defp diff([h | t], prev) do
    [h - prev | diff(t, h)]
  end

  defp diff([], _) do
    []
  end

  defp diff([h | t], prev, last) do
    [{1, h - prev} | diff(t, h, last)]
  end

  defp diff([], prev, last) when last >= prev do
    [{0, last - prev}]
  end

  defp diff([], _, _) do
    []
  end

  defp int_to_bitlist(0) do
    []
  end

  defp int_to_bitlist(int) do
    [int &&& 1 | int_to_bitlist(int >>> 1)]
  end

  defp adjust_size(bs, lb) do
    case bit_size(bs) do
      sz when sz < lb ->
        <<bs::size(sz)-bits, 0::size(lb - sz)>>

      _ ->
        bs
    end
  end

  def adjust_trailing_zeroes(bs0, lb) do
    case bit_size(bs0) do
      sz when sz < lb ->
        <<bs0::size(sz)-bits, 0::size(lb - sz)>>

      ^lb ->
        bs0

      _ ->
        <<_::size(lb)-bits, tail::bits>> = bs0
        sz = lb + bit_size(bs_drop_trailing_zeroes(tail))
        <<bs::size(sz)-bits, _::bits>> = bs0
        bs
    end
  end

  def bs_drop_trailing_zeroes(bs) do
    bs_drop_trailing_zeroes(bs, bit_size(bs))
  end

  defp bs_drop_trailing_zeroes(bs, 0) do
    bs
  end

  defp bs_drop_trailing_zeroes(bs0, sz0) when sz0 < 8 do
    <<byte::size(sz0)>> = bs0
    sz = sz0 - ntz(byte)
    <<bs::size(sz)-bits, _::bits>> = bs0
    bs
  end

  defp bs_drop_trailing_zeroes(bs0, sz0) do
    sz1 = sz0 - 8
    <<bs1::size(sz1)-bits, byte::size(8)>> = bs0

    case ntz(byte) do
      8 ->
        bs_drop_trailing_zeroes(bs1, sz1)

      ntz ->
        sz = sz0 - ntz
        <<bs::size(sz)-bits, _::size(ntz)-bits>> = bs0
        bs
    end
  end

  defp ntz(byte) do
    t =
      {8, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0,
       1, 0, 5, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0,
       2, 0, 1, 0, 6, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4, 0, 1, 0, 2, 0, 1, 0, 3, 0,
       1, 0, 2, 0, 1, 0, 5, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4, 0, 1, 0, 2, 0, 1, 0,
       3, 0, 1, 0, 2, 0, 1, 0, 7, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4, 0, 1, 0, 2, 0,
       1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 5, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4, 0, 1, 0,
       2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 6, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4, 0,
       1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 5, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0,
       4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0}

    :erlang.element(byte + 1, t)
  end

  defp is_default_bitstring_list([h | def__], [h | val]) do
    is_default_bitstring_list(def__, val)
  end

  defp is_default_bitstring_list([], []) do
    true
  end

  defp is_default_bitstring_list([], [_ | _] = val) do
    :lists.all(
      fn
        0 ->
          true

        _ ->
          false
      end,
      val
    )
  end

  defp is_default_bitstring_list(_, _) do
    false
  end

  defp extension_bitmap(_Val, pos, limit, acc) when pos >= limit do
    acc
  end

  defp extension_bitmap(val, pos, limit, acc) do
    bit =
      case :erlang.element(pos, val) do
        :asn1_NOVALUE ->
          0

        :asn1_DEFAULT ->
          0

        _ ->
          1
      end

    extension_bitmap(val, pos + 1, limit, acc <<< 1 ||| bit)
  end
end
