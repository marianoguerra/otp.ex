defmodule :m_asn1ct_imm do
  use Bitwise
  import :asn1ct_gen, only: [emit: 1]
  require Record

  Record.defrecord(:r_st, :st,
    var: :undefined,
    base: :undefined
  )

  def dec_slim_cg(imm0, bytesVar) do
    {imm, _} = optimize_alignment(imm0)
    :asn1ct_name.new(:v)

    [
      h
      | t
    ] = :erlang.atom_to_list(:asn1ct_name.curr(:v)) ++ '@'

    varBase = [h - (?a - ?A) | t]
    st0 = r_st(var: 0, base: varBase)
    {res, pre, _} = flatten(imm, bytesVar, st0)
    dcg_list_outside(pre)
    res
  end

  def dec_code_gen(imm, bytesVar) do
    emit(['begin', :nl])
    {dst, dstBuf} = dec_slim_cg(imm, bytesVar)
    emit([',', :nl, '{', dst, ',', dstBuf, '}', :nl, 'end'])
    :ok
  end

  def optimize_alignment(imm) do
    opt_al(imm, :unknown)
  end

  def optimize_alignment(imm, al) do
    opt_al(imm, al)
  end

  def per_dec_boolean() do
    {:map, {:get_bits, 1, [1]}, [{0, false}, {1, true}]}
  end

  def per_dec_enumerated([{v, _}], _Aligned) do
    {:value, v}
  end

  def per_dec_enumerated(namedList0, aligned) do
    ub = length(namedList0) - 1
    constraint = [{:ValueRange, {0, ub}}]
    int = per_dec_integer(constraint, aligned)
    namedList = per_dec_enumerated_fix_list(namedList0, [:enum_error], 0)
    {:map, int, opt_map(namedList, int)}
  end

  def per_dec_enumerated(baseNamedList, namedListExt0, aligned) do
    base = per_dec_enumerated(baseNamedList, aligned)
    namedListExt = per_dec_enumerated_fix_list(namedListExt0, [:enum_default], 0)
    ext = {:map, per_dec_normally_small_number(aligned), namedListExt}
    bit_case(base, ext)
  end

  def per_dec_extension_map(aligned) do
    len = per_dec_normally_small_length(aligned)
    {:get_bits, len, [1, :bitstring]}
  end

  def per_dec_integer(constraint0, aligned) do
    constraint = effective_constraint(:integer, constraint0)
    per_dec_integer_1(constraint, aligned)
  end

  def per_dec_length(singleValue, _, _Aligned)
      when is_integer(singleValue) do
    {:value, singleValue}
  end

  def per_dec_length({{fixed, fixed}, []}, allowZero, aligned) do
    bit_case(
      per_dec_length(fixed, allowZero, aligned),
      per_dec_length(:no, allowZero, aligned)
    )
  end

  def per_dec_length({{_, _} = constr, []}, allowZero, aligned) do
    bit_case(
      per_dec_length(constr, allowZero, aligned),
      per_dec_length(:no, allowZero, aligned)
    )
  end

  def per_dec_length({lb, ub}, _AllowZero, aligned)
      when is_integer(lb) and is_integer(lb) do
    per_dec_constrained(lb, ub, aligned)
  end

  def per_dec_length(:no, allowZero, aligned) do
    decode_unconstrained_length(allowZero, aligned)
  end

  def per_dec_named_integer(constraint, namedList0, aligned) do
    int = per_dec_integer(constraint, aligned)

    namedList =
      for {v, k} <- namedList0 do
        {k, v}
      end ++ [:integer_default]

    {:map, int, opt_map(namedList, int)}
  end

  def per_dec_k_m_string(stringType, constraint, aligned) do
    szConstr = effective_constraint(:bitstring, constraint)
    n = string_num_bits(stringType, constraint, aligned)
    imm = dec_string(szConstr, n, aligned, :k_m_string)
    chars = char_tab(constraint, stringType, n)
    convert_string(n, chars, imm)
  end

  def per_dec_octet_string(constraint, aligned) do
    dec_string(constraint, 8, aligned, :"OCTET STRING")
  end

  def per_dec_raw_bitstring(constraint, aligned) do
    dec_string(constraint, 1, aligned, :"BIT STRING")
  end

  def per_dec_open_type(aligned) do
    dec_string(:no, 8, aligned, :open_type)
  end

  def per_dec_real(aligned) do
    dec = fn v, buf ->
      emit(['{', {:call, :real_common, :decode_real, [v]}, :com, buf, '}'])
    end

    {:call, dec,
     {:get_bits, decode_unconstrained_length(true, aligned), [8, :binary, {:align, aligned}]}}
  end

  def per_dec_restricted_string(aligned) do
    decLen = decode_unconstrained_length(true, aligned)
    {:get_bits, decLen, [8, :binary]}
  end

  def per_enc_bit_string(val, [], constraint0, aligned) do
    {b, [[], bits]} = mk_vars([], [:bits])

    constraint =
      effective_constraint(
        :bitstring,
        constraint0
      )

    b ++
      [
        {:call, :erlang, :bit_size, [val], bits}
        | per_enc_length(val, 1, bits, constraint, aligned, :"BIT STRING")
      ]
  end

  def per_enc_bit_string(val0, nNL0, constraint0, aligned) do
    {b, [val, bs, bits, positions]} =
      mk_vars(
        val0,
        [:bs, :bits, :positions]
      )

    nNL = :lists.keysort(2, nNL0)

    constraint =
      effective_constraint(
        :bitstring,
        constraint0
      )

    extraArgs =
      case constr_min_size(constraint) do
        :no ->
          []

        lb ->
          [lb]
      end

    toBs =
      case extraArgs do
        [] ->
          {:call, :per_common, :bs_drop_trailing_zeroes, [val]}

        [0] ->
          {:call, :per_common, :bs_drop_trailing_zeroes, [val]}

        [lower] ->
          {:call, :per_common, :adjust_trailing_zeroes, [val, lower]}
      end

    b ++
      [
        [
          {:try, [bit_string_name2pos_fun(nNL, val)],
           {positions,
            [{:call, :per_common, :bitstring_from_positions, [positions | extraArgs]}]}, [toBs],
           bs},
          {:call, :erlang, :bit_size, [bs], bits}
        ]
        | per_enc_length(bs, 1, bits, constraint, aligned, :"BIT STRING")
      ]
  end

  def per_enc_legacy_bit_string(val0, [], constraint0, aligned) do
    {b, [val, bs, bits]} = mk_vars(val0, [:bs, :bits])

    constraint =
      effective_constraint(
        :bitstring,
        constraint0
      )

    extraArgs =
      case constr_min_size(constraint) do
        :no ->
          []

        lb ->
          [lb]
      end

    b ++
      [
        [
          {:call, :per_common, :to_bitstring, [val | extraArgs], bs},
          {:call, :erlang, :bit_size, [bs], bits}
        ]
        | per_enc_length(bs, 1, bits, constraint, aligned, :"BIT STRING")
      ]
  end

  def per_enc_legacy_bit_string(val0, nNL0, constraint0, aligned) do
    {b, [val, bs, bits, positions]} =
      mk_vars(
        val0,
        [:bs, :bits, :positions]
      )

    nNL = :lists.keysort(2, nNL0)

    constraint =
      effective_constraint(
        :bitstring,
        constraint0
      )

    extraArgs =
      case constr_min_size(constraint) do
        :no ->
          []

        0 ->
          []

        lb ->
          [lb]
      end

    b ++
      [
        [
          {:try, [bit_string_name2pos_fun(nNL, val)],
           {positions,
            [{:call, :per_common, :bitstring_from_positions, [positions | extraArgs]}]},
           [{:call, :per_common, :to_named_bitstring, [val | extraArgs]}], bs},
          {:call, :erlang, :bit_size, [bs], bits}
        ]
        | per_enc_length(bs, 1, bits, constraint, aligned, :"BIT STRING")
      ]
  end

  def per_enc_boolean(val0, _Aligned) do
    {b, [val]} = mk_vars(val0, [])

    b ++
      build_cond([
        [{:eq, val, false}, {:put_bits, 0, 1, [1]}],
        [{:eq, val, true}, {:put_bits, 1, 1, [1]}],
        [:_, {:error, {:illegal_boolean, val}}]
      ])
  end

  def per_enc_choice(val0, cs0, _Aligned) do
    {b, [val]} = mk_vars(val0, [])

    cs =
      for {tag, imm} <- cs0 do
        [{:eq, val, tag} | opt_choice(imm)]
      end

    b ++ build_cond(cs)
  end

  def per_enc_enumerated(val0, {root, ext}, aligned) do
    {b, [val]} = mk_vars(val0, [])
    constr = enumerated_constraint(root)
    rootCs = per_enc_enumerated_root(root, [{:put_bits, 0, 1, [1]}], val, constr, aligned)
    extCs = per_enc_enumerated_ext(ext, val, aligned)
    b ++ [{:cond, rootCs ++ extCs ++ enumerated_error(val)}]
  end

  def per_enc_enumerated(val0, root, aligned) do
    {b, [val]} = mk_vars(val0, [])
    constr = enumerated_constraint(root)
    cs = per_enc_enumerated_root(root, [], val, constr, aligned)
    b ++ [{:cond, cs ++ enumerated_error(val)}]
  end

  defp enumerated_error(val) do
    [[:_, {:error, {:illegal_enumerated, val}}]]
  end

  def per_enc_integer(val0, constraint0, aligned) do
    {b, [val]} = mk_vars(val0, [])
    constraint = effective_constraint(:integer, constraint0)
    b ++ per_enc_integer_1(val, constraint, aligned)
  end

  def per_enc_integer(val0, nNL, constraint0, aligned) do
    {b, [val]} = mk_vars(val0, [])
    constraint = effective_constraint(:integer, constraint0)

    cs =
      for {n, v} <- nNL do
        [{:eq, val, n} | per_enc_integer_1(v, constraint, aligned)]
      end

    case per_enc_integer_1(val, constraint, aligned) do
      [{:cond, intCs}] ->
        b ++ [{:cond, cs ++ intCs}]

      other ->
        b ++ [{:cond, cs ++ [[:_ | other]]}]
    end
  end

  def per_enc_null(_Val, _Aligned) do
    []
  end

  def per_enc_k_m_string(val0, stringType, constraint, aligned) do
    {b, [val, bin, len]} = mk_vars(val0, [:bin, :len])

    szConstraint =
      effective_constraint(
        :bitstring,
        constraint
      )

    unit = string_num_bits(stringType, constraint, aligned)
    chars0 = char_tab(constraint, stringType, unit)

    enc =
      case unit do
        16 ->
          {:call, :per_common, :encode_chars_16bit, [val], bin}

        32 ->
          {:call, :per_common, :encode_big_chars, [val], bin}

        8 ->
          {:call, :erlang, :list_to_binary, [val], bin}

        _ ->
          case enc_char_tab(chars0) do
            :notab ->
              {:call, :per_common, :encode_chars, [val, unit], bin}

            {:tab, tab} ->
              {:call, :per_common, :encode_chars, [val, unit, tab], bin}

            {:compact_map, map} ->
              {:call, :per_common, :encode_chars_compact_map, [val, unit, map], bin}
          end
      end

    case unit do
      8 ->
        b ++ [enc, {:call, :erlang, :byte_size, [bin], len}]

      _ ->
        b ++ [{:call, :erlang, :length, [val], len}, enc]
    end ++ per_enc_length(bin, unit, len, szConstraint, aligned, :k_m_string)
  end

  def per_enc_open_type(imm0, aligned) do
    imm =
      case aligned do
        true ->
          imm0 ++ [{:put_bits, 0, 0, [1, :align]}]

        false ->
          imm0
      end

    {[], [[], val, len, bin]} =
      mk_vars(
        [],
        [:output, :len, :bin]
      )

    [
      [
        {:list, imm, val},
        {:call, enc_mod(aligned), :complete, [val], bin},
        {:call, :erlang, :byte_size, [bin], len}
      ]
      | per_enc_length(bin, 8, len, aligned)
    ]
  end

  def per_enc_octet_string(bin, constraint0, aligned) do
    {b, [[], len]} = mk_vars([], [:len])

    constraint =
      effective_constraint(
        :bitstring,
        constraint0
      )

    b ++
      [
        {:call, :erlang, :byte_size, [bin], len}
        | per_enc_length(bin, 8, len, constraint, aligned, :"OCTET STRING")
      ]
  end

  def per_enc_legacy_octet_string(val0, constraint0, aligned) do
    {b, [val, bin, len]} = mk_vars(val0, [:bin, :len])

    constraint =
      effective_constraint(
        :bitstring,
        constraint0
      )

    b ++
      [
        [
          {:call, :erlang, :iolist_to_binary, [val], bin},
          {:call, :erlang, :byte_size, [bin], len}
        ]
        | per_enc_length(bin, 8, len, constraint, aligned, :"OCTET STRING")
      ]
  end

  def per_enc_restricted_string(val0, {m, f}, aligned) do
    {b, [val, bin, len]} = mk_vars(val0, [:bin, :len])

    b ++
      [
        [{:call, m, f, [val], bin}, {:call, :erlang, :byte_size, [bin], len}]
        | per_enc_length(bin, 8, len, aligned)
      ]
  end

  def per_enc_small_number(val, aligned) do
    build_cond([
      [{:lt, val, 64}, {:put_bits, val, 7, [1]}],
      [
        [:_, {:put_bits, 1, 1, [1]}]
        | per_enc_unsigned(
            val,
            aligned
          )
      ]
    ])
  end

  def per_enc_extension_bit(val0, _Aligned) do
    {b, [val]} = mk_vars(val0, [])
    b ++ build_cond([[{:eq, val, []}, {:put_bits, 0, 1, [1]}], [:_, {:put_bits, 1, 1, [1]}]])
  end

  def per_enc_extensions(val0, pos0, numBits, aligned) when numBits > 0 do
    pos = pos0 + 1
    {b, [val, bitmap]} = mk_vars(val0, [:bitmap])
    length = per_enc_small_length(numBits, aligned)

    putBits =
      case numBits do
        1 ->
          [{:put_bits, 1, 1, [1]}]

        _ ->
          [{:put_bits, bitmap, numBits, [1]}]
      end

    b ++
      [
        {:call, :per_common, :extension_bitmap, [val, pos, pos + numBits], bitmap},
        {:list, [{:cond, [[{:eq, bitmap, 0}], [:_ | length ++ putBits]]}], {:var, 'Extensions'}}
      ]
  end

  def per_enc_extensions_map(val0, vars, undefined, aligned) do
    numBits = length(vars)
    {b, [_Val, bitmap]} = mk_vars(val0, [:bitmap])
    length = per_enc_small_length(numBits, aligned)

    putBits =
      case numBits do
        1 ->
          [{:put_bits, 1, 1, [1]}]

        _ ->
          [{:put_bits, bitmap, numBits, [1]}]
      end

    bitmapExpr = extensions_bitmap(vars, undefined)

    b ++
      [
        {:assign, bitmap, bitmapExpr},
        {:list, [{:cond, [[{:eq, bitmap, 0}], [:_ | length ++ putBits]]}], {:var, 'Extensions'}}
      ]
  end

  def per_enc_optional(val, defVals) when is_list(defVals) do
    zero = {:put_bits, 0, 1, [1]}
    one = {:put_bits, 1, 1, [1]}

    [
      {:cond,
       for defVal <- defVals do
         [{:eq, val, defVal}, zero]
       end ++ [[:_, one]]}
    ]
  end

  def per_enc_optional(val, {:call, m, f, a}) do
    {[], [[], tmp]} = mk_vars([], [:tmp])
    zero = {:put_bits, 0, 1, [1]}
    one = {:put_bits, 1, 1, [1]}
    [{:call, m, f, [val | a], tmp}, {:cond, [[{:eq, tmp, true}, zero], [:_, one]]}]
  end

  def per_enc_sof(val0, constraint, elementVar, elementImm, aligned) do
    {b, [val, len]} = mk_vars(val0, [:len])

    szConstraint =
      effective_constraint(
        :bitstring,
        constraint
      )

    lenImm = enc_length(len, szConstraint, aligned)
    lc0 = [{:lc, elementImm, {:var, :erlang.atom_to_list(elementVar)}, val}]
    lc = opt_lc(lc0, lenImm)
    preBlock = b ++ [{:call, :erlang, :length, [val], len}]

    case lenImm do
      [{:cond, [[c | action]]}] ->
        preBlock ++ [{:cond, [[c | action ++ lc]]}]

      [{:sub, _, _, _} = sub, {:cond, [[c | action]]}] ->
        preBlock ++ [sub, {:cond, [[c | action ++ lc]]}]

      encLen ->
        preBlock ++ encLen ++ lc
    end
  end

  def enc_absent(val0, {:call, m, f, a}, body) do
    {b, [var, tmp]} = mk_vars(val0, [:tmp])
    b ++ [{:call, m, f, [var | a], tmp}, {:cond, [[{:eq, tmp, true}], [:_ | body]]}]
  end

  def enc_absent(val0, absVals, body) when is_list(absVals) do
    {b, [var]} = mk_vars(val0, [])

    cs =
      for aval <- absVals do
        [{:eq, var, aval}]
      end ++ [[:_ | body]]

    b ++ build_cond(cs)
  end

  def enc_append([[] | t]) do
    enc_append(t)
  end

  def enc_append([
        [{:put_bits, _, _, _} | _] = pb
        | [
            imm
            | t
          ] = t0
      ]) do
    case opt_choice(pb ++ imm) do
      [{:put_bits, _, _, _} | _] ->
        [{:block, pb} | enc_append(t0)]

      opt ->
        enc_append([opt | t])
    end
  end

  def enc_append([imm0 | [imm1 | t] = t0]) do
    try do
      combine_imms(imm0, imm1)
    catch
      :impossible ->
        [{:block, imm0} | enc_append(t0)]
    else
      imm ->
        enc_append([imm | t])
    end
  end

  def enc_append([h | t]) do
    [{:block, h} | enc_append(t)]
  end

  def enc_append([]) do
    []
  end

  def enc_element(n, val0) do
    {[], [val, dst]} = mk_vars(val0, [:element])
    {[{:call, :erlang, :element, [n, val], dst}], dst}
  end

  def enc_maps_get(n, val0) do
    {[], [val, dst0]} = mk_vars(val0, [:element])
    {:var, dst} = dst0
    dstExpr = {:expr, :lists.concat(['\#{', n, ':=', dst, '}'])}
    {:var, srcVar} = val
    {[{:assign, dstExpr, srcVar}], dst0}
  end

  def enc_comment(comment) do
    {:comment, comment}
  end

  def enc_cg(imm0, false) do
    imm1 = enc_cse(imm0)
    imm2 = enc_pre_cg(imm1)
    imm = enc_opt(imm2)
    enc_cg(imm)
  end

  def enc_cg(imm0, true) do
    imm1 = enc_cse(imm0)
    imm2 = enc_hoist_align(imm1)
    imm3 = enc_opt_al(imm2)
    imm4 = per_fixup(imm3)
    imm5 = enc_pre_cg(imm4)
    imm = enc_opt(imm5)
    enc_cg(imm)
  end

  defp is_aligned(t, lb, ub) when t === :"OCTET STRING" or t === :"BIT STRING" do
    lb !== ub or lb > 16
  end

  defp is_aligned(:k_m_string, _Lb, ub) do
    ub >= 16
  end

  defp dec_string(sv, u, aligned0, t) when is_integer(sv) do
    bits = u * sv
    aligned = aligned0 and is_aligned(t, bits, bits)
    {:get_bits, sv, [u, :binary, {:align, aligned}]}
  end

  defp dec_string({{sv, sv}, []}, u, aligned, t) do
    bit_case(
      dec_string(sv, u, aligned, t),
      dec_string(:no, u, aligned, t)
    )
  end

  defp dec_string({{_, _} = c, []}, u, aligned, t) do
    bit_case(
      dec_string(c, u, aligned, t),
      dec_string(:no, u, aligned, t)
    )
  end

  defp dec_string({lb, ub}, u, aligned0, t) do
    len = per_dec_constrained(lb, ub, aligned0)
    aligned = aligned0 and is_aligned(t, lb * u, ub * u)
    {:get_bits, len, [u, :binary, {:align, aligned}]}
  end

  defp dec_string(_, u, aligned, _T) do
    al = [{:align, aligned}]

    decRest = fn v, buf ->
      :asn1ct_func.call(:per_common, :decode_fragmented, [v, buf, u])
    end

    {:case,
     [
       {:test, {:get_bits, 1, [1 | al]}, 0,
        {:value, {:get_bits, {:get_bits, 7, [1]}, [u, :binary]}}},
       {:test, {:get_bits, 1, [1 | al]}, 1,
        {:test, {:get_bits, 1, [1]}, 0, {:value, {:get_bits, {:get_bits, 14, [1]}, [u, :binary]}}}},
       {:test, {:get_bits, 1, [1 | al]}, 1,
        {:test, {:get_bits, 1, [1]}, 1, {:value, {:call, decRest, {:get_bits, 6, [1]}}}}}
     ]}
  end

  defp per_dec_enumerated_fix_list([{v, _} | t], tail, n) do
    [{n, v} | per_dec_enumerated_fix_list(t, tail, n + 1)]
  end

  defp per_dec_enumerated_fix_list([], tail, _) do
    tail
  end

  defp per_dec_integer_1([{:SingleValue, value}], _Aligned) do
    {:value, value}
  end

  defp per_dec_integer_1([{:ValueRange, {:MIN, _}}], aligned) do
    per_dec_unconstrained(aligned)
  end

  defp per_dec_integer_1([{:ValueRange, {lb, :MAX}}], aligned)
       when is_integer(lb) do
    per_decode_semi_constrained(lb, aligned)
  end

  defp per_dec_integer_1([{:ValueRange, {lb, ub}}], aligned)
       when is_integer(lb) and is_integer(ub) do
    per_dec_constrained(lb, ub, aligned)
  end

  defp per_dec_integer_1([{{_, _} = constr0, _}], aligned) do
    constr = effective_constraint(:integer, [constr0])

    bit_case(
      per_dec_integer(constr, aligned),
      per_dec_unconstrained(aligned)
    )
  end

  defp per_dec_integer_1([], aligned) do
    per_dec_unconstrained(aligned)
  end

  defp per_dec_unconstrained(aligned) do
    {:get_bits, decode_unconstrained_length(false, aligned), [8, :signed]}
  end

  def per_dec_constrained(lb, ub, false) do
    range = ub - lb + 1
    get = {:get_bits, uper_num_bits(range), [1]}
    add_lb(lb, get)
  end

  def per_dec_constrained(lb, ub, true) do
    range = ub - lb + 1

    get =
      cond do
        range <= 255 ->
          {:get_bits, per_num_bits(range), [1, :unsigned]}

        range == 256 ->
          {:get_bits, 1, [8, :unsigned, {:align, true}]}

        range <= 65536 ->
          {:get_bits, 2, [8, :unsigned, {:align, true}]}

        true ->
          rangeOctLen = byte_size(:binary.encode_unsigned(range - 1))

          {:get_bits, per_dec_length({1, rangeOctLen}, false, true),
           [8, :unsigned, {:align, true}]}
      end

    add_lb(lb, get)
  end

  defp add_lb(0, get) do
    get
  end

  defp add_lb(lb, get) do
    {:add, get, lb}
  end

  def per_dec_normally_small_number(aligned) do
    small = {:get_bits, 6, [1]}
    unlimited = per_decode_semi_constrained(0, aligned)
    bit_case(small, unlimited)
  end

  defp per_dec_normally_small_length(aligned) do
    small = {:add, {:get_bits, 6, [1]}, 1}
    unlimited = decode_unconstrained_length(false, aligned)
    bit_case(small, unlimited)
  end

  defp per_decode_semi_constrained(lb, aligned) do
    add_lb(
      lb,
      {:get_bits, decode_unconstrained_length(false, aligned), [8]}
    )
  end

  defp bit_case(base, ext) do
    {:case, [{:test, {:get_bits, 1, [1]}, 0, base}, {:test, {:get_bits, 1, [1]}, 1, ext}]}
  end

  defp decode_unconstrained_length(allowZero, aligned) do
    al = [{:align, aligned}]

    zero =
      case allowZero do
        false ->
          [:non_zero]

        true ->
          []
      end

    {:case,
     [
       {:test, {:get_bits, 1, [1 | al]}, 0, {:value, {:get_bits, 7, [1 | zero]}}},
       {:test, {:get_bits, 1, [1 | al]}, 1,
        {:test, {:get_bits, 1, [1]}, 0, {:value, {:get_bits, 14, [1 | zero]}}}}
     ]}
  end

  defp uper_num_bits(n) do
    uper_num_bits(n, 1, 0)
  end

  defp uper_num_bits(n, t, b) when n <= t do
    b
  end

  defp uper_num_bits(n, t, b) do
    uper_num_bits(n, t <<< 1, b + 1)
  end

  defp per_num_bits(2) do
    1
  end

  defp per_num_bits(n) when n <= 4 do
    2
  end

  defp per_num_bits(n) when n <= 8 do
    3
  end

  defp per_num_bits(n) when n <= 16 do
    4
  end

  defp per_num_bits(n) when n <= 32 do
    5
  end

  defp per_num_bits(n) when n <= 64 do
    6
  end

  defp per_num_bits(n) when n <= 128 do
    7
  end

  defp per_num_bits(n) when n <= 255 do
    8
  end

  defp opt_map(map, imm) do
    case matched_range(imm) do
      :unknown ->
        map

      {lb, ub} ->
        opt_map_1(map, lb, ub)
    end
  end

  defp opt_map_1([{i, _} = pair | t], lb, ub) do
    cond do
      i === lb and i <= ub ->
        [pair | opt_map_1(t, lb + 1, ub)]

      lb < i and i <= ub ->
        [pair | opt_map_1(t, lb, ub)]

      true ->
        opt_map_1(t, lb, ub)
    end
  end

  defp opt_map_1(map, lb, ub) do
    cond do
      lb <= ub ->
        map

      true ->
        []
    end
  end

  defp matched_range({:get_bits, bits0, [u | flags]})
       when is_integer(u) do
    case not :lists.member(:signed, flags) and is_integer(bits0) do
      true ->
        bits = u * bits0
        {0, 1 <<< (bits - 1)}

      false ->
        :unknown
    end
  end

  defp matched_range({:add, imm, add}) do
    case matched_range(imm) do
      :unknown ->
        :unknown

      {lb, ub} ->
        {lb + add, ub + add}
    end
  end

  defp matched_range(_Op) do
    :unknown
  end

  defp string_num_bits(stringType, constraint, aligned) do
    case get_constraint(constraint, :PermittedAlphabet) do
      {:SingleValue, sv} ->
        charbits(length(sv), aligned)

      :no ->
        case stringType do
          :IA5String ->
            charbits(128, aligned)

          :VisibleString ->
            charbits(95, aligned)

          :PrintableString ->
            charbits(74, aligned)

          :NumericString ->
            charbits(11, aligned)

          :UniversalString ->
            32

          :BMPString ->
            16
        end
    end
  end

  defp charbits(numChars, false) do
    uper_num_bits(numChars)
  end

  defp charbits(numChars, true) do
    1 <<< uper_num_bits(uper_num_bits(numChars))
  end

  defp convert_string(8, :notab, imm) do
    {:convert, :binary_to_list, imm}
  end

  defp convert_string(numBits, :notab, imm) when numBits < 8 do
    dec = fn v, buf ->
      emit(['{', {:call, :per_common, :decode_chars, [v, numBits]}, :com, buf, '}'])
    end

    {:call, dec, imm}
  end

  defp convert_string(numBits, :notab, imm) when numBits === 16 do
    dec = fn v, buf ->
      emit(['{', {:call, :per_common, :decode_chars_16bit, [v]}, :com, buf, '}'])
    end

    {:call, dec, imm}
  end

  defp convert_string(numBits, :notab, imm) do
    dec = fn v, buf ->
      emit(['{', {:call, :per_common, :decode_big_chars, [v, numBits]}, :com, buf, '}'])
    end

    {:call, dec, imm}
  end

  defp convert_string(numBits, chars, imm) do
    dec = fn v, buf ->
      emit([
        '{',
        {:call, :per_common, :decode_chars, [v, numBits, {:asis, chars}]},
        :com,
        buf,
        '}'
      ])
    end

    {:call, dec, imm}
  end

  defp char_tab(c, stringType, numBits) do
    case get_constraint(c, :PermittedAlphabet) do
      {:SingleValue, sv} ->
        char_tab_1(sv, numBits)

      :no ->
        case stringType do
          :IA5String ->
            :notab

          :VisibleString ->
            :notab

          :PrintableString ->
            chars = ' \'()+,-./0123456789:=?ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'
            char_tab_1(chars, numBits)

          :NumericString ->
            char_tab_1(' 0123456789', numBits)

          :UniversalString ->
            :notab

          :BMPString ->
            :notab
        end
    end
  end

  defp char_tab_1(chars, numBits) do
    max = :lists.max(chars)
    bitValMax = 1 <<< (numBits - 1)

    cond do
      max <= bitValMax ->
        :notab

      true ->
        :erlang.list_to_tuple(:lists.sort(chars))
    end
  end

  defp opt_al({:get_bits, e0, opts0}, a0) do
    {e, a1} = opt_al(e0, a0)
    opts = opt_al_1(a1, opts0)
    a = update_al(a1, e, opts)
    {{:get_bits, e, opts}, a}
  end

  defp opt_al({:call, fun, e0}, a0) do
    {e, a} = opt_al(e0, a0)
    {{:call, fun, e}, a}
  end

  defp opt_al({:convert, op, e0}, a0) do
    {e, a} = opt_al(e0, a0)
    {{:convert, op, e}, a}
  end

  defp opt_al({:value, v} = term, a)
       when is_integer(v) or
              is_atom(v) do
    {term, a}
  end

  defp opt_al({:value, e0}, a0) do
    {e, a} = opt_al(e0, a0)
    {{:value, e}, a}
  end

  defp opt_al({:add, e0, i}, a0) when is_integer(i) do
    {e, a} = opt_al(e0, a0)
    {{:add, e, i}, a}
  end

  defp opt_al({:test, e0, v, b0}, a0) do
    {e, a1} = opt_al(e0, a0)
    {b, a2} = opt_al(b0, a1)
    {{:test, e, v, b}, a2}
  end

  defp opt_al({:case, cs0}, a0) do
    {cs, a} = opt_al_cs(cs0, a0)
    {{:case, cs}, a}
  end

  defp opt_al({:map, e0, cs}, a0) do
    {e, a} = opt_al(e0, a0)
    {{:map, e, cs}, a}
  end

  defp opt_al(i, a) when is_integer(i) do
    {i, a}
  end

  defp opt_al_cs([c0 | cs0], a0) do
    {c, a1} = opt_al(c0, a0)
    {cs, a2} = opt_al_cs(cs0, a0)
    {[c | cs], merge_al(a1, a2)}
  end

  defp opt_al_cs([], _) do
    {[], :none}
  end

  defp merge_al(:unknown, _) do
    :unknown
  end

  defp merge_al(other, :none) do
    other
  end

  defp merge_al(_, :unknown) do
    :unknown
  end

  defp merge_al(i0, i1) do
    case {rem(i0, 8), rem(i1, 8)} do
      {i, i} ->
        i

      {_, _} ->
        :unknown
    end
  end

  defp opt_al_1(:unknown, opts) do
    opts
  end

  defp opt_al_1(a, opts0) do
    case alignment(opts0) do
      :none ->
        opts0

      :full ->
        case rem(a, 8) do
          0 ->
            :proplists.delete(:align, opts0)

          bits ->
            opts1 = :proplists.delete(:align, opts0)
            [{:align, 8 - bits} | opts1]
        end

      ^a ->
        opts0
    end
  end

  defp update_al(a0, e, opts) do
    a =
      case alignment(opts) do
        :none ->
          a0

        :full ->
          0

        bits when is_integer(a0) ->
          0 = rem(a0 + bits, 8)

        _ ->
          0
      end

    [u] =
      for u <- opts, is_integer(u) do
        u
      end

    cond do
      rem(u, 8) === 0 ->
        a

      is_integer(a) and is_integer(e) ->
        a + u * e

      true ->
        :unknown
    end
  end

  defp flatten({:get_bits, i, u}, buf0, st0)
       when is_integer(i) do
    {dst, st} = new_var_pair(st0)
    gb = {:get_bits, {i, buf0}, u, dst}
    flatten_align(gb, [], st)
  end

  defp flatten({:get_bits, e0, u}, buf0, st0) do
    {e, pre, st1} = flatten(e0, buf0, st0)
    {dst, st2} = new_var_pair(st1)
    gb = {:get_bits, e, u, dst}
    flatten_align(gb, pre, st2)
  end

  defp flatten({:test, {:get_bits, i, u}, v, e0}, buf0, st0)
       when is_integer(i) do
    {dstBuf0, st1} = new_var('Buf', st0)
    gb = {:get_bits, {i, buf0}, u, {v, dstBuf0}}
    {{_Dst, dstBuf}, pre0, st2} = flatten_align(gb, [], st1)
    {e, pre1, st3} = flatten(e0, dstBuf, st2)
    {e, pre0 ++ pre1, st3}
  end

  defp flatten({:add, e0, i}, buf0, st0) do
    {{src, buf}, pre, st1} = flatten(e0, buf0, st0)
    {dst, st} = new_var('Add', st1)
    {{dst, buf}, pre ++ [{:add, src, i, dst}], st}
  end

  defp flatten({:case, cs0}, buf0, st0) do
    {dst, st1} = new_var_pair(st0)
    {cs1, st} = flatten_cs(cs0, buf0, st1)
    {al, cs2} = flatten_hoist_align(cs1)
    {dst, al ++ [{:case, buf0, cs2, dst}], st}
  end

  defp flatten({:map, e0, cs0}, buf0, st0) do
    {{e, dstBuf}, pre, st1} = flatten(e0, buf0, st0)
    {dst, st2} = new_var('Int', st1)
    cs = flatten_map_cs(cs0, e)
    {{dst, dstBuf}, pre ++ [{:map, e, cs, {dst, dstBuf}}], st2}
  end

  defp flatten({:value, v}, buf0, st0) when is_atom(v) do
    {{'\'' ++ :erlang.atom_to_list(v) ++ '\'', buf0}, [], st0}
  end

  defp flatten({:value, v0}, buf0, st0) when is_integer(v0) do
    {{v0, buf0}, [], st0}
  end

  defp flatten({:value, v0}, buf0, st0) do
    flatten(v0, buf0, st0)
  end

  defp flatten({:convert, op, e0}, buf0, st0) do
    {{e, buf}, pre, st1} = flatten(e0, buf0, st0)
    {dst, st2} = new_var('Conv', st1)
    {{dst, buf}, pre ++ [{:convert, op, e, dst}], st2}
  end

  defp flatten({:call, fun, e0}, buf0, st0) do
    {src, pre, st1} = flatten(e0, buf0, st0)
    {dst, st2} = new_var_pair(st1)
    {dst, pre ++ [{:call, fun, src, dst}], st2}
  end

  defp flatten_cs([c0 | cs0], buf, st0) do
    {c, pre, st1} = flatten(c0, buf, st0)
    {cs, st2} = flatten_cs(cs0, buf, st0)
    st3 = r_st(st2, var: max(r_st(st1, :var), r_st(st2, :var)))
    {[pre ++ [{:return, c}] | cs], st3}
  end

  defp flatten_cs([], _, st) do
    {[], st}
  end

  defp flatten_map_cs(cs, var) do
    flatten_map_cs_1(cs, {var, cs})
  end

  defp flatten_map_cs_1([{k, v} | cs], defData) do
    [
      {{:asis, k}, {:asis, v}}
      | flatten_map_cs_1(
          cs,
          defData
        )
    ]
  end

  defp flatten_map_cs_1([:integer_default], {int, _}) do
    [{:_, int}]
  end

  defp flatten_map_cs_1([:enum_default], {int, _}) do
    [{:_, ['{asn1_enum,', int, '}']}]
  end

  defp flatten_map_cs_1([:enum_error], {var, _}) do
    [{:_, ['exit({error,{asn1,{decode_enumerated,', var, '}}})']}]
  end

  defp flatten_map_cs_1([], _) do
    []
  end

  defp flatten_hoist_align([[{:align_bits, _, _} = ab | t] | cs]) do
    flatten_hoist_align_1(cs, ab, [t])
  end

  defp flatten_hoist_align(cs) do
    {[], cs}
  end

  defp flatten_hoist_align_1([[ab | t] | cs], ab, acc) do
    flatten_hoist_align_1(cs, ab, [t | acc])
  end

  defp flatten_hoist_align_1([], ab, acc) do
    {[ab], :lists.reverse(acc)}
  end

  defp flatten_align({:get_bits, {srcBits, srcBuf}, u, dst} = gb0, pre, st0) do
    case alignment(u) do
      :none ->
        flatten_align_1(u, dst, pre ++ [gb0], st0)

      :full ->
        {padBits, st1} = new_var('Pad', st0)
        {dstBuf, st2} = new_var('Buf', st1)
        ab = {:align_bits, srcBuf, padBits}
        agb = {:get_bits, {padBits, srcBuf}, [1], {:_, dstBuf}}
        gb = {:get_bits, {srcBits, dstBuf}, u, dst}
        flatten_align_1(u, dst, pre ++ [ab, agb, gb], st2)

      padBits when is_integer(padBits) and padBits > 0 ->
        {dstBuf, st1} = new_var('Buf', st0)
        agb = {:get_bits, {padBits, srcBuf}, [1], {:_, dstBuf}}
        gb = {:get_bits, {srcBits, dstBuf}, u, dst}
        flatten_align_1(u, dst, pre ++ [agb, gb], st1)
    end
  end

  defp flatten_align_1(u, {d, _} = dst, pre, st) do
    case is_non_zero(u) do
      false ->
        {dst, pre, st}

      true ->
        {dst, pre ++ [{:non_zero, d}], st}
    end
  end

  defp new_var_pair(st0) do
    {var, st1} = new_var('V', st0)
    {buf, st2} = new_var('Buf', st1)
    {{var, buf}, st2}
  end

  defp new_var(tag, r_st(base: varBase, var: n) = st) do
    {varBase ++ tag ++ :erlang.integer_to_list(n), r_st(st, var: n + 1)}
  end

  defp alignment([{:align, false} | _]) do
    :none
  end

  defp alignment([{:align, true} | _]) do
    :full
  end

  defp alignment([{:align, bits} | _]) do
    bits
  end

  defp alignment([_ | t]) do
    alignment(t)
  end

  defp alignment([]) do
    :none
  end

  defp is_non_zero(fl) do
    :lists.member(:non_zero, fl)
  end

  defp dcg_list_outside([{:align_bits, buf, szVar} | t]) do
    emit([szVar, ' = bit_size(', buf, ') band 7'])
    iter_dcg_list_outside(t)
  end

  defp dcg_list_outside([{:case, buf, cs, dst} | t]) do
    dcg_case(buf, cs, dst)
    iter_dcg_list_outside(t)
  end

  defp dcg_list_outside([{:map, val, cs, dst} | t]) do
    dcg_map(val, cs, dst)
    iter_dcg_list_outside(t)
  end

  defp dcg_list_outside([{:add, s1, s2, dst} | t]) do
    emit([dst, ' = ', s1, ' + ', s2])
    iter_dcg_list_outside(t)
  end

  defp dcg_list_outside([{:return, {v, buf}} | t]) do
    emit(['{', v, ',', buf, '}'])
    iter_dcg_list_outside(t)
  end

  defp dcg_list_outside([{:call, fun, {v, buf}, {dst, dstBuf}} | t]) do
    emit(['{', dst, ',', dstBuf, '}  = '])
    fun.(v, buf)
    iter_dcg_list_outside(t)
  end

  defp dcg_list_outside([{:convert, {m, f}, v, dst} | t]) do
    emit([dst, ' = ', {:asis, m}, ':', {:asis, f}, '(', v, ')'])
    iter_dcg_list_outside(t)
  end

  defp dcg_list_outside([{:convert, op, v, dst} | t]) do
    emit([dst, ' = ', op, '(', v, ')'])
    iter_dcg_list_outside(t)
  end

  defp dcg_list_outside([{:get_bits, {_, buf0}, _, _} | _] = l0) do
    emit('<<')
    {l, buf} = dcg_list_inside(l0, :buf)
    emit([buf, '/bitstring>> = ', buf0])
    iter_dcg_list_outside(l)
  end

  defp dcg_list_outside([]) do
    emit('ignore')
    :ok
  end

  defp iter_dcg_list_outside([_ | _] = t) do
    emit([',', :nl])
    dcg_list_outside(t)
  end

  defp iter_dcg_list_outside([]) do
    :ok
  end

  defp dcg_case(buf, cs, {dst, dstBuf}) do
    emit(['{', dst, ',', dstBuf, '} = case ', buf, ' of', :nl])
    dcg_case_cs(cs)
    emit('end')
  end

  defp dcg_case_cs([c | cs]) do
    emit('<<')
    {t0, dstBuf} = dcg_list_inside(c, :buf)
    emit([dstBuf, '/bitstring>>'])
    t1 = dcg_guard(t0)
    dcg_list_outside(t1)

    case cs do
      [] ->
        emit([:nl])

      [_ | _] ->
        emit([';', :nl])
    end

    dcg_case_cs(cs)
  end

  defp dcg_case_cs([]) do
    :ok
  end

  defp dcg_guard([{:non_zero, src} | t]) do
    emit([' when ', src, ' =/= 0 ->', :nl])
    t
  end

  defp dcg_guard(t) do
    emit([' ->', :nl])
    t
  end

  defp dcg_map(val, cs, {dst, _}) do
    emit([dst, ' = case ', val, ' of', :nl])
    dcg_map_cs(cs)
    emit('end')
  end

  defp dcg_map_cs([{k, v}]) do
    emit([k, ' -> ', v, :nl])
  end

  defp dcg_map_cs([{k, v} | cs]) do
    emit([k, ' -> ', v, ';', :nl])
    dcg_map_cs(cs)
  end

  defp dcg_list_inside(
         [{:get_bits, {sz, _}, fl0, {dst, dstBuf}} | t],
         _
       ) do
    fl = bit_flags(fl0, [])
    emit([mk_dest(dst), ':', sz, fl, ','])
    dcg_list_inside(t, dstBuf)
  end

  defp dcg_list_inside(l, dst) do
    {l, dst}
  end

  defp bit_flags([{:align, _} | t], acc) do
    bit_flags(t, acc)
  end

  defp bit_flags([:non_zero | t], acc) do
    bit_flags(t, acc)
  end

  defp bit_flags([u | t], acc) when is_integer(u) do
    bit_flags(t, ['unit:' ++ :erlang.integer_to_list(u) | acc])
  end

  defp bit_flags([h | t], acc) do
    bit_flags(t, [:erlang.atom_to_list(h) | acc])
  end

  defp bit_flags([], []) do
    ''
  end

  defp bit_flags([], acc) do
    case '/' ++ bit_flags_1(acc, '') do
      '/unit:1' ->
        []

      opts ->
        opts
    end
  end

  defp bit_flags_1([h | t], sep) do
    sep ++ h ++ bit_flags_1(t, '-')
  end

  defp bit_flags_1([], _) do
    []
  end

  defp mk_dest(i) when is_integer(i) do
    :erlang.integer_to_list(i)
  end

  defp mk_dest(s) do
    s
  end

  defp split_off_nonbuilding(imm) do
    :lists.splitwith(&is_nonbuilding/1, imm)
  end

  defp is_nonbuilding({:assign, _, _}) do
    true
  end

  defp is_nonbuilding({:call, _, _, _, _}) do
    true
  end

  defp is_nonbuilding({:comment, _}) do
    true
  end

  defp is_nonbuilding({:lc, _, _, _, _}) do
    true
  end

  defp is_nonbuilding({:set, _, _}) do
    true
  end

  defp is_nonbuilding({:list, _, _}) do
    true
  end

  defp is_nonbuilding({:sub, _, _, _}) do
    true
  end

  defp is_nonbuilding({:try, _, _, _, _}) do
    true
  end

  defp is_nonbuilding(_) do
    false
  end

  defp mk_vars(input0, temps) do
    :asn1ct_name.new(:enc)
    curr = :asn1ct_name.curr(:enc)
    [h | t] = :erlang.atom_to_list(curr)
    base = [h - (?a - ?A) | t ++ '@']

    case input0 do
      {:var, name} when is_list(name) ->
        {[], [input0 | mk_vars_1(base, temps)]}

      [] ->
        {[], [input0 | mk_vars_1(base, temps)]}

      _ when is_integer(input0) ->
        {[], [input0 | mk_vars_1(base, temps)]}
    end
  end

  defp mk_vars_1(base, vars) do
    for v <- vars do
      mk_var(base, v)
    end
  end

  defp mk_var(base, v) do
    {:var, base ++ :erlang.atom_to_list(v)}
  end

  defp per_enc_integer_1(val, [], aligned) do
    [{:cond, [[:_ | per_enc_unconstrained(val, aligned)]]}]
  end

  defp per_enc_integer_1(
         val,
         [{{:SingleValue, [_ | _] = svs} = constr, []}],
         aligned
       ) do
    [{:ValueRange, {lb, ub}}] = effective_constraint(:integer, [constr])

    root =
      for sv <- svs do
        {[], _, put} = per_enc_constrained(sv, lb, ub, aligned)
        [[{:eq, val, sv}, {:put_bits, 0, 1, [1]}] | put]
      end

    cs =
      root ++
        [
          [
            [:_, {:put_bits, 1, 1, [1]}]
            | per_enc_unconstrained(val, aligned)
          ]
        ]

    build_cond(cs)
  end

  defp per_enc_integer_1(val0, [{{_, _} = constr, []}], aligned) do
    {prefix, check, action} = per_enc_integer_2(val0, constr, aligned)

    prefix ++
      build_cond([
        [
          [check, {:put_bits, 0, 1, [1]}]
          | action
        ],
        [
          [:_, {:put_bits, 1, 1, [1]}]
          | per_enc_unconstrained(val0, aligned)
        ]
      ])
  end

  defp per_enc_integer_1(val0, [constr], aligned) do
    {prefix, check, action} = per_enc_integer_2(val0, constr, aligned)
    prefix ++ build_cond([[check | action], [:_, {:error, {:illegal_integer, val0}}]])
  end

  defp per_enc_integer_2(val, {:SingleValue, sv}, aligned)
       when is_integer(sv) do
    per_enc_constrained(val, sv, sv, aligned)
  end

  defp per_enc_integer_2(val, {:ValueRange, {:MIN, ub}}, aligned)
       when is_integer(ub) do
    {[], {:lt, val, ub + 1}, per_enc_unconstrained(val, aligned)}
  end

  defp per_enc_integer_2(val0, {:ValueRange, {lb, :MAX}}, aligned)
       when is_integer(lb) do
    {prefix, val} = sub_lb(val0, lb)
    {prefix, {:ge, val, 0}, per_enc_unsigned(val, aligned)}
  end

  defp per_enc_integer_2(val, {:ValueRange, {lb, ub}}, aligned)
       when is_integer(lb) and is_integer(ub) do
    per_enc_constrained(val, lb, ub, aligned)
  end

  defp per_enc_constrained(val, sv, sv, _Aligned) do
    {[], {:eq, val, sv}, []}
  end

  defp per_enc_constrained(val0, lb, ub, false) do
    {prefix, val} = sub_lb(val0, lb)
    range = ub - lb + 1
    numBits = uper_num_bits(range)
    check = {:ult, val, range}
    put = [{:put_bits, val, numBits, [1]}]
    {prefix, check, put}
  end

  defp per_enc_constrained(val0, lb, ub, true) do
    {prefix, val} = sub_lb(val0, lb)
    range = ub - lb + 1
    check = {:ult, val, range}

    cond do
      range < 256 ->
        numBits = per_num_bits(range)
        put = [{:put_bits, val, numBits, [1]}]
        {prefix, check, put}

      range === 256 ->
        numBits = 8
        put = [{:put_bits, val, numBits, [1, :align]}]
        {prefix, check, put}

      range <= 65536 ->
        put = [{:put_bits, val, 16, [1, :align]}]
        {prefix, check, put}

      true ->
        rangeOctsLen = byte_size(:binary.encode_unsigned(range - 1))
        bitsNeeded = per_num_bits(rangeOctsLen)
        {prefix, check, per_enc_constrained_huge(bitsNeeded, val)}
    end
  end

  defp per_enc_constrained_huge(bitsNeeded, {:var, varBase} = val) do
    bin = {:var, varBase ++ '@bin'}
    binSize0 = {:var, varBase ++ '@bin_size0'}
    binSize = {:var, varBase ++ '@bin_size'}

    [
      {:call, :binary, :encode_unsigned, [val], bin},
      {:call, :erlang, :byte_size, [bin], binSize0},
      {:sub, binSize0, 1, binSize},
      {:cond,
       [[:_, {:put_bits, binSize, bitsNeeded, [1]}, {:put_bits, bin, :binary, [8, :align]}]]}
    ]
  end

  defp per_enc_constrained_huge(bitsNeeded, val) when is_integer(val) do
    bin = :binary.encode_unsigned(val)
    binSize = :erlang.byte_size(bin)
    [{:put_bits, binSize - 1, bitsNeeded, [1]}, {:put_bits, val, 8 * binSize, [1, :align]}]
  end

  defp per_enc_unconstrained(val, aligned) do
    case aligned do
      false ->
        []

      true ->
        [{:put_bits, 0, 0, [1, :align]}]
    end ++ [{:call, :per_common, :encode_unconstrained_number, [val]}]
  end

  defp per_enc_unsigned(val, aligned) do
    case is_integer(val) do
      false ->
        {:var, varBase} = val
        bin = {:var, varBase ++ '@bin'}
        binSize = {:var, varBase ++ '@bin_size'}

        [
          [
            {:call, :binary, :encode_unsigned, [val], bin},
            {:call, :erlang, :byte_size, [bin], binSize}
          ]
          | per_enc_length(bin, 8, binSize, aligned)
        ]

      true ->
        bin = :binary.encode_unsigned(val)
        len = byte_size(bin)
        per_enc_length(bin, 8, len, aligned)
    end
  end

  defp per_enc_length(bin, unit, len, aligned) do
    u = unit(1, aligned)
    putBits = put_bits_binary(bin, unit, aligned)
    encFragmented = {:call, :per_common, :encode_fragmented, [bin, unit]}

    al =
      case aligned do
        false ->
          []

        true ->
          [{:put_bits, 0, 0, [1, :align]}]
      end

    build_cond([
      [{:lt, len, 128}, {:put_bits, len, 8, u}, putBits],
      [{:lt, len, 16384}, {:put_bits, 2, 2, u}, {:put_bits, len, 14, [1]}, putBits],
      [:_ | al ++ [encFragmented]]
    ])
  end

  defp per_enc_length(bin, unit, len, :no, aligned, _Type) do
    per_enc_length(bin, unit, len, aligned)
  end

  defp per_enc_length(bin, unit, len, {{lb, ub}, []}, aligned, type) do
    {prefix, check, putLen} = per_enc_constrained(len, lb, ub, aligned)
    noExt = {:put_bits, 0, 1, [1]}
    u = unit(unit, aligned, type, lb * unit, ub * unit)
    putBits = [{:put_bits, bin, :binary, u}]
    [{:cond, extConds0}] = per_enc_length(bin, unit, len, aligned)
    ext = {:put_bits, 1, 1, [1]}
    extConds = prepend_to_cond(extConds0, ext)

    build_length_cond(
      prefix,
      [[[check, noExt] | putLen ++ putBits] | extConds]
    )
  end

  defp per_enc_length(bin, unit, len, {lb, ub}, aligned, type)
       when is_integer(lb) do
    {prefix, check, putLen} = per_enc_constrained(len, lb, ub, aligned)
    u = unit(unit, aligned, type, lb * unit, ub * unit)
    putBits = [{:put_bits, bin, :binary, u}]
    build_length_cond(prefix, [[check | putLen ++ putBits]])
  end

  defp per_enc_length(bin, unit0, len, sv, aligned, type)
       when is_integer(sv) do
    numBits = sv * unit0

    unit =
      case rem(numBits, 8) do
        0 ->
          8

        _ ->
          unit0
      end

    u = unit(unit, aligned, type, numBits, numBits)
    pb = {:put_bits, bin, :binary, u}
    [{:cond, [[{:eq, len, sv}, pb]]}]
  end

  defp enc_length(len, :no, aligned) do
    u = unit(1, aligned)

    build_cond([
      [{:lt, len, 128}, {:put_bits, len, 8, u}],
      [{:lt, len, 16384}, {:put_bits, 2, 2, u}, {:put_bits, len, 14, [1]}]
    ])
  end

  defp enc_length(len, {{lb, ub}, []}, aligned) do
    {prefix, check, putLen} = per_enc_constrained(len, lb, ub, aligned)
    noExt = {:put_bits, 0, 1, [1]}
    [{:cond, extConds0}] = enc_length(len, :no, aligned)
    ext = {:put_bits, 1, 1, [1]}
    extConds = prepend_to_cond(extConds0, ext)

    build_length_cond(
      prefix,
      [[[check, noExt] | putLen] | extConds]
    )
  end

  defp enc_length(len, {lb, ub}, aligned) when is_integer(lb) do
    {prefix, check, putLen} = per_enc_constrained(len, lb, ub, aligned)
    build_length_cond(prefix, [[check | putLen]])
  end

  defp enc_length(len, sv, _Aligned) when is_integer(sv) do
    [{:cond, [[{:eq, len, sv}]]}]
  end

  defp extensions_bitmap(vs, undefined) do
    highest = 1 <<< (length(vs) - 1)
    cs = extensions_bitmap_1(vs, undefined, highest)
    :lists.flatten(:lists.join(' bor ', cs))
  end

  defp extensions_bitmap_1([{:var, v} | vs], undefined, power) do
    s = [
      'case ',
      v,
      ' of\n',
      '  ',
      undefined,
      ' -> 0;\n  _ -> ',
      :erlang.integer_to_list(power),
      '\nend'
    ]

    [s | extensions_bitmap_1(vs, undefined, power >>> 1)]
  end

  defp extensions_bitmap_1([], _, _) do
    []
  end

  defp put_bits_binary(bin, _Unit, aligned) when is_binary(bin) do
    sz = byte_size(bin)
    <<int::size(sz)-unit(8)>> = bin
    {:put_bits, int, 8 * sz, unit(1, aligned)}
  end

  defp put_bits_binary(bin, unit, aligned) do
    {:put_bits, bin, :binary, unit(unit, aligned)}
  end

  defp sub_lb(val, 0) do
    {[], val}
  end

  defp sub_lb({:var, var} = val0, lb) do
    val = {:var, var ++ '@sub'}
    {[{:sub, val0, lb, val}], val}
  end

  defp sub_lb(val, lb) when is_integer(val) do
    {[], val - lb}
  end

  defp build_length_cond([{:sub, var0, base, var}] = prefix, cs) do
    prefix ++ [{:cond, opt_length_nzlb(cs, {var0, var, base}, 0)}]
  end

  defp build_length_cond([], cs) do
    [{:cond, opt_length_zlb(cs, 0)}]
  end

  defp opt_length_zlb([[{:ult, var, val} | actions] | t], ub) do
    opt_length_zlb([[{:lt, var, val} | actions] | t], ub)
  end

  defp opt_length_zlb([[{:lt, _, val} | _] = h | t], ub) do
    cond do
      val <= ub ->
        opt_length_zlb(t, ub)

      true ->
        [h | opt_length_zlb(t, max(ub, val))]
    end
  end

  defp opt_length_zlb([h | t], ub) do
    [h | opt_length_zlb(t, ub)]
  end

  defp opt_length_zlb([], _) do
    []
  end

  defp opt_length_nzlb([[{:ult, var, val} | _] = h | t], {_, var, base} = st, _Ub) do
    [h | opt_length_nzlb(t, st, base + val)]
  end

  defp opt_length_nzlb([[{:lt, var0, val} | _] = h | t], {var0, _, _} = st, ub) do
    cond do
      val <= ub ->
        opt_length_nzlb(t, st, ub)

      true ->
        [h | opt_length_nzlb(t, st, val)]
    end
  end

  defp opt_length_nzlb([h | t], st, ub) do
    [h | opt_length_nzlb(t, st, ub)]
  end

  defp opt_length_nzlb([], _, _) do
    []
  end

  defp build_cond(conds0) do
    case eval_cond(conds0, :gb_sets.empty()) do
      [[:_ | actions]] ->
        actions

      conds ->
        [{:cond, conds}]
    end
  end

  defp eval_cond([[:_, {:cond, cs}]], seen) do
    eval_cond(cs, seen)
  end

  defp eval_cond([[cond__ | actions] = h | t], seen0) do
    case :gb_sets.is_element(cond__, seen0) do
      false ->
        seen = :gb_sets.insert(cond__, seen0)

        case eval_cond_1(cond__) do
          false ->
            eval_cond(t, seen)

          true ->
            [[:_ | actions]]

          :maybe ->
            [h | eval_cond(t, seen)]
        end

      true ->
        eval_cond(t, seen0)
    end
  end

  defp eval_cond([], _) do
    []
  end

  defp eval_cond_1({:ult, i, n})
       when is_integer(i) and
              is_integer(n) do
    0 <= i and i < n
  end

  defp eval_cond_1({:eq, [], []}) do
    true
  end

  defp eval_cond_1({:eq, i, n})
       when is_integer(i) and
              is_integer(n) do
    i === n
  end

  defp eval_cond_1({:ge, i, n})
       when is_integer(i) and
              is_integer(n) do
    i >= n
  end

  defp eval_cond_1({:lt, i, n})
       when is_integer(i) and
              is_integer(n) do
    i < n
  end

  defp eval_cond_1(_) do
    :maybe
  end

  defp prepend_to_cond([h | t], code) do
    [prepend_to_cond_1(h, code) | prepend_to_cond(t, code)]
  end

  defp prepend_to_cond([], _) do
    []
  end

  defp prepend_to_cond_1([check | t], code) do
    [[check, code] | t]
  end

  defp enc_char_tab(:notab) do
    :notab
  end

  defp enc_char_tab(tab0) do
    tab1 = :erlang.tuple_to_list(tab0)
    first = hd(tab1)
    tab = enc_char_tab_1(tab1, first, 0)

    case :lists.member(:ill, tab) do
      false ->
        {:compact_map, {first, tuple_size(tab0)}}

      true ->
        {:tab, {first - 1, :erlang.list_to_tuple(tab)}}
    end
  end

  defp enc_char_tab_1([h | t], h, i) do
    [i | enc_char_tab_1(t, h + 1, i + 1)]
  end

  defp enc_char_tab_1([_ | _] = t, h, i) do
    [:ill | enc_char_tab_1(t, h + 1, i)]
  end

  defp enc_char_tab_1([], _, _) do
    []
  end

  defp enumerated_constraint([_]) do
    [{:SingleValue, 0}]
  end

  defp enumerated_constraint(root) do
    [{:ValueRange, {0, length(root) - 1}}]
  end

  defp per_enc_enumerated_root(nNL, prefix, val, constr, aligned) do
    per_enc_enumerated_root_1(nNL, prefix, val, constr, aligned, 0)
  end

  defp per_enc_enumerated_root_1([{h, _} | t], prefix, val, constr, aligned, n) do
    [
      [{:eq, val, h} | prefix ++ per_enc_integer_1(n, constr, aligned)]
      | per_enc_enumerated_root_1(t, prefix, val, constr, aligned, n + 1)
    ]
  end

  defp per_enc_enumerated_root_1([], _, _, _, _, _) do
    []
  end

  defp per_enc_enumerated_ext(nNL, val, aligned) do
    per_enc_enumerated_ext_1(nNL, val, aligned, 0)
  end

  defp per_enc_enumerated_ext_1([{h, _} | t], val, aligned, n) do
    [
      [
        [{:eq, val, h}, {:put_bits, 1, 1, [1]}]
        | per_enc_small_number(n, aligned)
      ]
      | per_enc_enumerated_ext_1(t, val, aligned, n + 1)
    ]
  end

  defp per_enc_enumerated_ext_1([], _, _, _) do
    []
  end

  defp per_enc_small_length(val0, aligned) do
    {sub, val} = sub_lb(val0, 1)
    u = unit(1, aligned)

    sub ++
      build_cond([
        [{:lt, val, 64}, {:put_bits, val, 7, [1]}],
        [{:lt, val0, 128}, {:put_bits, 1, 1, [1]}, {:put_bits, val0, 8, u}],
        [:_, {:put_bits, 1, 1, [1]}, {:put_bits, 2, 2, u}, {:put_bits, val0, 14, [1]}]
      ])
  end

  defp constr_min_size(:no) do
    :no
  end

  defp constr_min_size({{lb, _}, []}) when is_integer(lb) do
    lb
  end

  defp constr_min_size({lb, _}) when is_integer(lb) do
    lb
  end

  defp constr_min_size(sv) when is_integer(sv) do
    sv
  end

  defp enc_mod(false) do
    :uper
  end

  defp enc_mod(true) do
    :per
  end

  defp unit(u, false) do
    [u]
  end

  defp unit(u, true) do
    [u, :align]
  end

  defp unit(u, aligned, type, lb, ub) do
    case aligned and is_aligned(type, lb, ub) do
      true ->
        [u, :align]

      false ->
        [u]
    end
  end

  defp opt_choice(imm) do
    {pb, t0} =
      :lists.splitwith(
        fn
          {:put_bits, v, _, _}
          when is_integer(v) ->
            true

          _ ->
            false
        end,
        imm
      )

    try do
      {prefix, t} = split_off_nonbuilding(t0)
      prefix ++ opt_choice_1(t, pb)
    catch
      :impossible ->
        imm
    end
  end

  defp opt_choice_1([{:cond, cs0}], pb) do
    case cs0 do
      [[c | act]] ->
        [{:cond, [[c | pb ++ act]]}]

      [[c | act], [:_, {:error, _}] = error] ->
        [{:cond, [[c | pb ++ act], error]}]

      _ ->
        [{:cond, opt_choice_2(cs0, pb)}]
    end
  end

  defp opt_choice_1(_, _) do
    throw(:impossible)
  end

  defp opt_choice_2(
         [[c | [{:put_bits, _, _, _} | _] = act] | t],
         pb
       ) do
    [[c | pb ++ act] | opt_choice_2(t, pb)]
  end

  defp opt_choice_2([[_, {:error, _}] = h | t], pb) do
    [h | opt_choice_2(t, pb)]
  end

  defp opt_choice_2([_ | _], _) do
    throw(:impossible)
  end

  defp opt_choice_2([], _) do
    []
  end

  defp opt_lc(
         [
           {:lc,
            [
              {:call, :erlang, :iolist_to_binary, [var], bin},
              {:call, :erlang, :byte_size, [bin], lenVar},
              {:cond, [[{:eq, lenVar, len}, {:put_bits, bin, _, [_ | align]}]]}
            ], var, val}
         ] = lc,
         lenImm
       ) do
    checkImm = [
      {:cond,
       [
         [{:eq, {:expr, 'length(' ++ mk_val(var) ++ ')'}, len}],
         [{:eq, {:expr, 'byte_size(' ++ mk_val(var) ++ ')'}, len}]
       ]}
    ]

    al =
      case align do
        [] ->
          []

        [:align] ->
          [{:put_bits, 0, 0, [1 | align]}]
      end

    case al === [] or is_end_aligned(lenImm) or lb_is_nonzero(lenImm) do
      false ->
        lc

      true ->
        al ++ [{:lc, checkImm, var, val, {:var, '_'}}, {:call, :erlang, :iolist_to_binary, [val]}]
    end
  end

  defp opt_lc([{:lc, elementImm0, v, l}] = lc, lenImm) do
    case enc_opt_al_1(elementImm0, 0) do
      {elementImm, 0} ->
        case is_end_aligned(lenImm) or
               (is_beginning_aligned(elementImm0) and lb_is_nonzero(lenImm)) do
          false ->
            lc

          true ->
            [{:put_bits, 0, 0, [1, :align]}, {:lc, elementImm, v, l}]
        end

      _ ->
        lc
    end
  end

  defp is_beginning_aligned([{:cond, cs}]) do
    :lists.all(
      fn [_ | act] ->
        is_beginning_aligned(act)
      end,
      cs
    )
  end

  defp is_beginning_aligned([{:error, _} | _]) do
    true
  end

  defp is_beginning_aligned([{:put_bits, _, _, u} | _]) do
    case u do
      [_, :align] ->
        true

      [_] ->
        false
    end
  end

  defp is_beginning_aligned(imm0) do
    case split_off_nonbuilding(imm0) do
      {[], _} ->
        false

      {[_ | _], imm} ->
        is_beginning_aligned(imm)
    end
  end

  defp is_end_aligned(imm) do
    case enc_opt_al_1(imm, :unknown) do
      {_, 0} ->
        true

      {_, _} ->
        false
    end
  end

  defp lb_is_nonzero([{:sub, _, _, _} | _]) do
    true
  end

  defp lb_is_nonzero(_) do
    false
  end

  defp combine_imms(immA0, immB0) do
    {prefix0, immA} = split_off_nonbuilding(immA0)
    {prefix1, immB} = split_off_nonbuilding(immB0)
    prefix = prefix0 ++ prefix1
    combined = do_combine(immA ++ immB, 3.0)
    prefix ++ combined
  end

  defp do_combine([{:error, _} = imm | _], _Budget) do
    [imm]
  end

  defp do_combine([{:cond, cs0} | t], budget0) do
    budget = debit(budget0, num_clauses(cs0, 0))

    cs =
      for [c | act] <- cs0 do
        [c | do_combine(act ++ t, budget)]
      end

    [{:cond, cs}]
  end

  defp do_combine([{:put_bits, v, _, _} | _] = l, budget)
       when is_integer(v) do
    {pb, t} = collect_put_bits(l)
    do_combine_put_bits(pb, t, budget)
  end

  defp do_combine(_, _) do
    throw(:impossible)
  end

  defp do_combine_put_bits(pb, [], _Budget) do
    pb
  end

  defp do_combine_put_bits(pb, [{:cond, cs0} | t], budget) do
    cs =
      for [c | act] <- cs0 do
        case act do
          [{:error, _}] ->
            [c | act]

          _ ->
            [c | do_combine(pb ++ act, budget)]
        end
      end

    do_combine([{:cond, cs} | t], budget)
  end

  defp do_combine_put_bits(_, _, _) do
    throw(:impossible)
  end

  defp debit(budget0, alternatives) do
    case budget0 - :math.log2(alternatives) do
      budget when budget > 0.0 ->
        budget

      _ ->
        throw(:impossible)
    end
  end

  defp num_clauses([[_, {:error, _}] | t], n) do
    num_clauses(t, n)
  end

  defp num_clauses([_ | t], n) do
    num_clauses(t, n + 1)
  end

  defp num_clauses([], n) do
    n
  end

  defp collect_put_bits(imm) do
    :lists.splitwith(
      fn
        {:put_bits, v, _, _}
        when is_integer(v) ->
          true

        _ ->
          false
      end,
      imm
    )
  end

  defp enc_cse([
         {:call, :erlang, :element, args, v} = h
         | t
       ]) do
    [h | enc_cse_1(t, args, v)]
  end

  defp enc_cse(imm) do
    imm
  end

  defp enc_cse_1([{:call, :erlang, :element, args, dst} | t], args, v) do
    [{:set, v, dst} | enc_cse_1(t, args, v)]
  end

  defp enc_cse_1([{:block, bl} | t], args, v) do
    [{:block, enc_cse_1(bl, args, v)} | enc_cse_1(t, args, v)]
  end

  defp enc_cse_1([h | t], args, v) do
    [h | enc_cse_1(t, args, v)]
  end

  defp enc_cse_1([], _, _) do
    []
  end

  defp enc_pre_cg(imm) do
    enc_pre_cg_1(imm, :outside_list, :in_seq)
  end

  defp enc_pre_cg_1([], _StL, _StB) do
    nil
  end

  defp enc_pre_cg_1([h], stL, stB) do
    enc_pre_cg_2(h, stL, stB)
  end

  defp enc_pre_cg_1([h0 | t0], stL, stB) do
    case is_nonbuilding(h0) do
      true ->
        h = enc_pre_cg_nonbuilding(h0, stL)
        seq = {:seq, h, enc_pre_cg_1(t0, stL, :in_seq)}

        case stB do
          :outside_seq ->
            {:block, seq}

          :in_seq ->
            seq
        end

      false ->
        h = enc_pre_cg_2(h0, :in_head, :outside_seq)
        t = enc_pre_cg_1(t0, :in_tail, :outside_seq)
        enc_make_cons(h, t)
    end
  end

  defp enc_pre_cg_2(:align, stL, _StB) do
    case stL do
      :in_head ->
        :align

      :in_tail ->
        {:cons, :align, nil}
    end
  end

  defp enc_pre_cg_2({:apply, _, _} = imm, _, _) do
    imm
  end

  defp enc_pre_cg_2({:block, bl0}, stL, stB) do
    enc_pre_cg_1(bl0, stL, stB)
  end

  defp enc_pre_cg_2({:call, _, _, _} = imm, _, _) do
    imm
  end

  defp enc_pre_cg_2({:call_gen, _, _, _, _, _} = imm, _, _) do
    imm
  end

  defp enc_pre_cg_2({:cond, cs0}, stL, _StB) do
    cs =
      for [c | act] <- cs0 do
        {c, enc_pre_cg_1(act, stL, :outside_seq)}
      end

    {:cond, cs}
  end

  defp enc_pre_cg_2({:error, _} = e, _, _) do
    e
  end

  defp enc_pre_cg_2({:lc, b0, v, l}, stL, _StB) do
    b = enc_pre_cg_1(b0, stL, :outside_seq)
    {:lc, b, v, l}
  end

  defp enc_pre_cg_2({:put_bits, v, 8, [1]}, stL, _StB) do
    case stL do
      :in_head ->
        {:integer, v}

      :in_tail ->
        {:cons, {:integer, v}, nil}

      :outside_list ->
        {:cons, {:integer, v}, nil}
    end
  end

  defp enc_pre_cg_2({:put_bits, v, :binary, _}, _StL, _StB) do
    v
  end

  defp enc_pre_cg_2({:put_bits, _, _, [_]} = putBits, _StL, _StB) do
    {:binary, [putBits]}
  end

  defp enc_pre_cg_2({:var, _} = imm, _, _) do
    imm
  end

  defp enc_make_cons({:binary, h}, {:binary, t}) do
    {:binary, h ++ t}
  end

  defp enc_make_cons({:binary, h0}, {:cons, {:binary, h1}, t}) do
    enc_make_cons({:binary, h0 ++ h1}, t)
  end

  defp enc_make_cons({:binary, h}, {:cons, {:integer, int}, t}) do
    enc_make_cons(
      {:binary, h ++ [{:put_bits, int, 8, [1]}]},
      t
    )
  end

  defp enc_make_cons({:integer, int}, {:binary, t}) do
    {:binary, [{:put_bits, int, 8, [1]} | t]}
  end

  defp enc_make_cons({:integer, int}, {:cons, {:binary, h}, t}) do
    enc_make_cons(
      {:binary, [{:put_bits, int, 8, [1]} | h]},
      t
    )
  end

  defp enc_make_cons(h, t) do
    {:cons, h, t}
  end

  defp enc_pre_cg_nonbuilding({:lc, b0, var, list, dst}, stL) do
    b = enc_pre_cg_1(b0, stL, :outside_seq)
    {:lc, b, var, list, dst}
  end

  defp enc_pre_cg_nonbuilding({:list, list0, dst}, _StL) do
    list = enc_pre_cg_1(list0, :outside_list, :outside_seq)
    {:list, list, dst}
  end

  defp enc_pre_cg_nonbuilding({:try, try0, {p, succ0}, else0, dst}, stL) do
    try = enc_pre_cg_1(try0, stL, :outside_seq)
    succ = enc_pre_cg_1(succ0, stL, :outside_seq)
    else__ = enc_pre_cg_1(else0, stL, :outside_seq)
    {:try, try, {p, succ}, else__, dst}
  end

  defp enc_pre_cg_nonbuilding(imm, _) do
    imm
  end

  Record.defrecord(:r_ost, :ost,
    sym: :undefined,
    t: :undefined
  )

  defp enc_opt(imm0) do
    {imm, _} = enc_opt(imm0, r_ost(sym: :gb_trees.empty()))
    imm
  end

  defp enc_opt(:align, st) do
    {:align, r_ost(st, t: t_align({0, 7}))}
  end

  defp enc_opt({:apply, what, as}, st) do
    {{:apply, what, subst_list(as, st)}, r_ost(st, t: t_any())}
  end

  defp enc_opt({:assign, _, _} = imm, st) do
    {imm, st}
  end

  defp enc_opt({:binary, putBits0}, st) do
    putBits =
      for {:put_bits, v, sz, f} <- putBits0 do
        {:put_bits, subst(v, st), sz, f}
      end

    numBits =
      :lists.foldl(
        fn {:put_bits, _, bits, _}, sum ->
          sum + bits
        end,
        0,
        putBits
      )

    {{:binary, putBits}, r_ost(st, t: t_bitstring(numBits))}
  end

  defp enc_opt({:block, bl0}, st0) do
    {bl, st} = enc_opt(bl0, st0)
    {{:block, bl}, st}
  end

  defp enc_opt(
         {:call, :binary, :encode_unsigned, [int], bin} = imm,
         st0
       ) do
    type = get_type(int, st0)

    st =
      case t_range(type) do
        :any ->
          set_type(bin, t_binary(), st0)

        {lb0, ub0} ->
          lb = bit_size(:binary.encode_unsigned(lb0))
          ub = bit_size(:binary.encode_unsigned(ub0))
          set_type(bin, t_binary({lb, ub}), st0)
      end

    {imm, st}
  end

  defp enc_opt(
         {:call, :erlang, :bit_size, [bin], dst} = imm0,
         st0
       ) do
    type = get_type(bin, st0)

    case t_range(type) do
      :any ->
        st1 = set_type(bin, t_bitstring(), st0)

        st =
          propagate(
            dst,
            fn t, s ->
              bit_size_propagate(bin, t, s)
            end,
            st1
          )

        {imm0, st}

      {lb, ub} = range ->
        st = set_type(dst, t_integer(range), st0)

        imm =
          case lb do
            ^ub ->
              :none

            _ ->
              imm0
          end

        {imm, st}
    end
  end

  defp enc_opt(
         {:call, :erlang, :byte_size, [bin], dst} = imm0,
         st0
       ) do
    type = get_type(bin, st0)

    case t_range(type) do
      :any ->
        st1 = set_type(bin, t_binary(), st0)

        st =
          propagate(
            dst,
            fn t, s ->
              byte_size_propagate(bin, t, s)
            end,
            st1
          )

        {imm0, st}

      {lb0, ub0} ->
        lb = div(lb0 + 7, 8)
        ub = div(ub0 + 7, 8)
        st = set_type(dst, t_integer({lb, ub}), st0)

        imm =
          case lb do
            ^ub ->
              :none

            _ ->
              imm0
          end

        {imm, st}
    end
  end

  defp enc_opt(
         {:call, :erlang, :iolist_to_binary, _} = imm,
         st
       ) do
    {imm, r_ost(st, t: t_binary())}
  end

  defp enc_opt(
         {:call, :erlang, :length, [list], dst} = imm0,
         st0
       ) do
    st1 =
      propagate(
        dst,
        fn t, s ->
          length_propagate(list, t, s)
        end,
        st0
      )

    {imm0, st1}
  end

  defp enc_opt({:call, :per, :complete, [data], dst}, st0) do
    type = get_type(data, st0)
    st = set_type(dst, t_binary(t_range(type)), st0)

    case t_type(type) do
      :binary ->
        {{:set, data, dst}, st}

      :bitlist ->
        {{:call, :erlang, :list_to_bitstring, [data], dst}, st}

      :iolist ->
        {{:call, :erlang, :iolist_to_binary, [data], dst}, st}

      nil ->
        imm = {:list, {:binary, [{:put_bits, 0, 8, [1]}]}, dst}
        enc_opt(imm, st0)

      _ ->
        {{:call, :per, :complete, [data], dst}, st}
    end
  end

  defp enc_opt({:call, :uper, :complete, [data], dst}, st0) do
    type = get_type(data, st0)
    st = set_type(dst, t_binary(t_range(type)), st0)

    case t_type(type) do
      :binary ->
        {{:set, data, dst}, st0}

      :iolist ->
        {{:call, :erlang, :iolist_to_binary, [data], dst}, st}

      nil ->
        imm = {:list, {:binary, [{:put_bits, 0, 8, [1]}]}, dst}
        enc_opt(imm, st0)

      _ ->
        {{:call, :uper, :complete, [data], dst}, st}
    end
  end

  defp enc_opt(
         {:call, :per_common, :encode_chars, [[list, numBits] | _], dst} = imm,
         st0
       ) do
    st1 = set_type(dst, t_bitstring(), st0)

    st =
      propagate(
        list,
        fn t, s ->
          char_propagate(dst, t, numBits, s)
        end,
        st1
      )

    {imm, st}
  end

  defp enc_opt(
         {:call, :per_common, :encode_chars_16bit, [list], dst} = imm,
         st0
       ) do
    st1 = set_type(dst, t_binary(), st0)

    st =
      propagate(
        list,
        fn t, s ->
          char_propagate(dst, t, 16, s)
        end,
        st1
      )

    {imm, st}
  end

  defp enc_opt(
         {:call, :per_common, :encode_big_chars, [list], dst} = imm,
         st0
       ) do
    st1 = set_type(dst, t_binary(), st0)

    st =
      propagate(
        list,
        fn t, s ->
          char_propagate(dst, t, 32, s)
        end,
        st1
      )

    {imm, st}
  end

  defp enc_opt(
         {:call, :per_common, :encode_fragmented, [_, unit]} = imm,
         st
       ) do
    t =
      case rem(unit, 8) do
        0 ->
          t_iolist()

        _ ->
          t_bitlist()
      end

    {imm, r_ost(st, t: t)}
  end

  defp enc_opt(
         {:call, :per_common, :encode_unconstrained_number, _} = imm,
         st
       ) do
    {imm, r_ost(st, t: t_iolist())}
  end

  defp enc_opt(
         {:call, :per_common, :bitstring_from_positions, _} = imm,
         st
       ) do
    {imm, r_ost(st, t: t_bitstring())}
  end

  defp enc_opt(
         {:call, :per_common, :to_named_bitstring, _} = imm,
         st
       ) do
    {imm, r_ost(st, t: t_bitstring())}
  end

  defp enc_opt({:call, _, _, _} = imm, st) do
    {imm, r_ost(st, t: t_any())}
  end

  defp enc_opt({:call, _, _, _, _} = imm, st) do
    {imm, r_ost(st, t: :undefined)}
  end

  defp enc_opt({:call_gen, n, k, f, l, as}, st) do
    {{:call_gen, n, k, f, l, subst(as, st)}, r_ost(st, t: t_any())}
  end

  defp enc_opt({:cond, cs0}, st0) do
    case enc_opt_cs(cs0, st0) do
      [{:_, imm, type}] ->
        {imm, r_ost(st0, t: type)}

      [{cond__, imm, type0} | cs1] ->
        {cs, type} = enc_opt_cond_1(cs1, type0, [{cond__, imm}])
        {{:cond, cs}, r_ost(st0, t: type)}
    end
  end

  defp enc_opt({:comment, _} = imm, st) do
    {imm, r_ost(st, t: :undefined)}
  end

  defp enc_opt({:cons, h0, t0}, st0) do
    {h, r_ost(t: typeH) = st1} = enc_opt(h0, st0)
    {t, r_ost(t: typeT) = st} = enc_opt(t0, st1)
    {{:cons, h, t}, r_ost(st, t: t_cons(typeH, typeT))}
  end

  defp enc_opt({:error, _} = imm, st) do
    {imm, r_ost(st, t: t_any())}
  end

  defp enc_opt({:integer, v}, st) do
    {{:integer, subst(v, st)}, r_ost(st, t: t_integer())}
  end

  defp enc_opt({:lc, e0, b, c}, st) do
    {e, _} = enc_opt(e0, st)
    {{:lc, e, b, c}, r_ost(st, t: t_any())}
  end

  defp enc_opt({:lc, e0, b, c, dst}, st) do
    {e, _} = enc_opt(e0, st)
    {{:lc, e, b, c, dst}, r_ost(st, t: :undefined)}
  end

  defp enc_opt({:list, imm0, dst}, st0) do
    {imm, r_ost(t: type) = st1} = enc_opt(imm0, st0)
    st = set_type(dst, type, st1)
    {{:list, imm, dst}, r_ost(st, t: :undefined)}
  end

  defp enc_opt(nil, st) do
    {nil, r_ost(st, t: t_nil())}
  end

  defp enc_opt({:seq, h0, t0}, st0) do
    {h, st1} = enc_opt(h0, st0)
    {t, st} = enc_opt(t0, st1)
    {enc_opt_seq(h, t), st}
  end

  defp enc_opt({:set, _, _} = imm, st) do
    {imm, r_ost(st, t: :undefined)}
  end

  defp enc_opt({:sub, src0, int, dst}, st0) do
    src = subst(src0, st0)
    type = get_type(src, st0)

    st =
      case t_range(type) do
        :any ->
          propagate(
            dst,
            fn t, s ->
              set_type(src, t_add(t, int), s)
            end,
            st0
          )

        {lb, ub} ->
          set_type(dst, t_integer({lb - int, ub - int}), st0)
      end

    {{:sub, src, int, dst}, r_ost(st, t: :undefined)}
  end

  defp enc_opt({:try, try0, {p, succ0}, else0, dst}, st0) do
    {try, _} = enc_opt(try0, st0)
    {succ, _} = enc_opt(succ0, st0)
    {else__, _} = enc_opt(else0, st0)
    {{:try, try, {p, succ}, else__, dst}, r_ost(st0, t: :undefined)}
  end

  defp enc_opt({:var, _} = imm, st) do
    type = get_type(imm, st)
    {subst(imm, st), r_ost(st, t: type)}
  end

  defp remove_trailing_align({:block, bl}) do
    {:block, remove_trailing_align(bl)}
  end

  defp remove_trailing_align({:cons, h, {:cons, :align, nil}}) do
    h
  end

  defp remove_trailing_align({:seq, h, t}) do
    {:seq, h, remove_trailing_align(t)}
  end

  defp remove_trailing_align(imm) do
    imm
  end

  defp enc_opt_seq(:none, t) do
    t
  end

  defp enc_opt_seq(
         {:list, imm, data},
         {:seq, {:call, :per, :complete, [data], _}, _} = t
       ) do
    {:seq, {:list, remove_trailing_align(imm), data}, t}
  end

  defp enc_opt_seq({:call, _, _, _, {:var, _} = dst} = h, t) do
    case is_var_unused(dst, t) do
      false ->
        {:seq, h, t}

      true ->
        t
    end
  end

  defp enc_opt_seq(h, t) do
    {:seq, h, t}
  end

  defp is_var_unused(_, :align) do
    true
  end

  defp is_var_unused(v, {:call, _, _, args}) do
    not :lists.member(v, args)
  end

  defp is_var_unused(v, {:cons, h, t}) do
    is_var_unused(v, h) and is_var_unused(v, t)
  end

  defp is_var_unused(_, _) do
    false
  end

  defp bit_size_propagate(bin, type, st) do
    case t_range(type) do
      :any ->
        st

      {lb, ub} ->
        set_type(bin, t_bitstring({lb, ub}), st)
    end
  end

  defp byte_size_propagate(bin, type, st) do
    case t_range(type) do
      :any ->
        st

      {lb, ub} ->
        set_type(bin, t_binary({lb * 8, ub * 8}), st)
    end
  end

  defp char_propagate(dst, t, numBits, st) do
    case t_range(t) do
      :any ->
        st

      {sz, sz} when rem(sz * numBits, 8) === 0 ->
        bits = sz * numBits
        set_type(dst, t_binary({bits, bits}), st)

      {lb, ub} ->
        range = {lb * numBits, ub * numBits}

        case rem(numBits, 8) do
          0 ->
            set_type(dst, t_binary(range), st)

          _ ->
            set_type(dst, t_bitstring(range), st)
        end
    end
  end

  defp length_propagate(list, type, st) do
    set_type(list, t_list(t_range(type)), st)
  end

  defp enc_opt_cond_1([{cond__, {:error, _} = imm, _} | t], st, acc) do
    enc_opt_cond_1(t, st, [{cond__, imm} | acc])
  end

  defp enc_opt_cond_1([{cond__, imm, curr0} | t], curr1, acc) do
    curr = t_join(curr0, curr1)
    enc_opt_cond_1(t, curr, [{cond__, imm} | acc])
  end

  defp enc_opt_cond_1([], st, acc) do
    {:lists.reverse(acc), st}
  end

  defp enc_opt_cs([{cond__, imm0} | t], st0) do
    case eo_eval_cond(cond__, st0) do
      false ->
        enc_opt_cs(t, st0)

      true ->
        {imm, r_ost(t: type)} = enc_opt(imm0, st0)
        [{:_, imm, type}]

      :maybe ->
        st = update_type_info(cond__, st0)
        {imm, r_ost(t: type)} = enc_opt(imm0, st)
        [{cond__, imm, type} | enc_opt_cs(t, st0)]
    end
  end

  defp enc_opt_cs([], _) do
    []
  end

  defp eo_eval_cond(:_, _) do
    true
  end

  defp eo_eval_cond({op, {:var, _} = var, val}, st) do
    type = get_type(var, st)

    case t_range(type) do
      :any ->
        :maybe

      {_, _} = range ->
        eval_cond_range(op, range, val)
    end
  end

  defp eo_eval_cond({_Op, {:expr, _}, _Val}, _St) do
    :maybe
  end

  defp eval_cond_range(:lt, {lb, ub}, val) do
    cond do
      ub < val ->
        true

      val <= lb ->
        false

      true ->
        :maybe
    end
  end

  defp eval_cond_range(_Op, _Range, _Val) do
    :maybe
  end

  defp update_type_info({:ult, {:var, _} = var, val}, st) do
    int = t_integer({0, val - 1})
    type = t_meet(get_type(var, st), int)
    set_type(var, type, st)
  end

  defp update_type_info({:lt, {:var, _} = var, val}, st) do
    int = t_integer({0, val - 1})
    type = t_meet(get_type(var, st), int)
    set_type(var, type, st)
  end

  defp update_type_info({:eq, {:var, _} = var, val}, st)
       when is_integer(val) do
    int = t_integer(val)
    type = t_meet(get_type(var, st), int)
    set_type(var, type, st)
  end

  defp update_type_info({:eq, _, _}, st) do
    st
  end

  defp update_type_info({:ge, _, _}, st) do
    st
  end

  defp subst_list(as, st) do
    for a <- as do
      subst(a, st)
    end
  end

  defp subst({:var, _} = var, st) do
    type = get_type(var, st)

    case t_type(type) do
      :integer ->
        case t_range(type) do
          :any ->
            var

          {val, val} ->
            val

          {_, _} ->
            var
        end

      _ ->
        var
    end
  end

  defp subst(v, _St) do
    v
  end

  defp set_type({:var, var}, {_, _} = type, r_ost(sym: sym0) = st0) do
    sym1 = :gb_trees.enter(var, type, sym0)

    case :gb_trees.lookup({:propagate, var}, sym1) do
      :none ->
        r_ost(st0, sym: sym1)

      {:value, propagate} ->
        sym = :gb_trees.delete({:propagate, var}, sym1)
        st = r_ost(st0, sym: sym)
        propagate.(type, st)
    end
  end

  defp get_type({:var, v}, r_ost(sym: sym)) do
    case :gb_trees.lookup(v, sym) do
      :none ->
        t_any()

      {:value, t} ->
        t
    end
  end

  defp propagate({:var, var}, propagate, r_ost(sym: sym0) = st)
       when is_function(propagate, 2) do
    sym = :gb_trees.enter({:propagate, var}, propagate, sym0)
    r_ost(st, sym: sym)
  end

  defp t_align(range) do
    {:align, t__range(range)}
  end

  defp t_any() do
    {:any, :any}
  end

  defp t_binary() do
    {:binary, :any}
  end

  defp t_binary(range) do
    {:binary, t__range(range)}
  end

  defp t_bitlist() do
    {:bitlist, :any}
  end

  defp t_bitstring() do
    {:bitstring, :any}
  end

  defp t_bitstring(range0) do
    case t__range(range0) do
      {bits, bits} = range when rem(bits, 8) === 0 ->
        {:binary, range}

      range ->
        {:bitstring, range}
    end
  end

  defp t_add({:integer, {lb, ub}}, n) do
    {:integer, {lb + n, ub + n}}
  end

  defp t_cons({_, _} = t1, {_, _} = t2) do
    t =
      case {t__cons_type(t1), t__cons_type(t2)} do
        {_, :any} ->
          :any

        {:any, _} ->
          :any

        {:align, _} ->
          :align

        {_, :align} ->
          :align

        {:binary, :binary} ->
          :iolist

        {:binary, :bitstring} ->
          :bitlist

        {:bitstring, :binary} ->
          :bitlist

        {:bitstring, :bitstring} ->
          :bitlist
      end

    {t, t__cons_ranges(t__cons_range(t1), t__cons_range(t2))}
  end

  defp t_integer() do
    {:integer, :any}
  end

  defp t_integer(range) do
    {:integer, t__range(range)}
  end

  defp t_iolist() do
    {:iolist, :any}
  end

  defp t_list(range) do
    {:list, t__range(range)}
  end

  defp t_nil() do
    {nil, {0, 0}}
  end

  defp t_meet({t1, range1}, {t2, range2}) do
    {t_meet_types(t1, t2), t_meet_ranges(range1, range2)}
  end

  defp t_meet_types(:integer, :integer) do
    :integer
  end

  defp t_meet_types(:any, :integer) do
    :integer
  end

  defp t_meet_ranges(:any, range) do
    range
  end

  defp t_meet_ranges({lb1, ub1}, {lb2, ub2}) do
    cond do
      lb1 <= ub2 and lb2 <= ub1 ->
        {max(lb1, lb2), ub1}

      lb2 <= ub1 and lb1 <= ub2 ->
        {max(lb1, lb2), ub2}
    end
  end

  defp t_join({t1, range1}, {t2, range2}) do
    t = t_join_types(:lists.sort([t1, t2]))
    range = t_join_ranges(range1, range2)
    {t, range}
  end

  defp t_join_ranges({lb1, ub1}, {lb2, ub2}) do
    {min(lb1, lb2), max(ub1, ub2)}
  end

  defp t_join_ranges(:any, _) do
    :any
  end

  defp t_join_ranges(_, :any) do
    :any
  end

  defp t_join_types([t, t]) do
    t
  end

  defp t_join_types([:align, :any]) do
    :any
  end

  defp t_join_types([:align, _]) do
    :align
  end

  defp t_join_types([:any, _]) do
    :any
  end

  defp t_join_types([:bitlist, :bitstring]) do
    :any
  end

  defp t_join_types([:bitlist, :integer]) do
    :any
  end

  defp t_join_types([:bitlist, :iolist]) do
    :bitlist
  end

  defp t_join_types([:bitlist, nil]) do
    :bitlist
  end

  defp t_join_types([:binary, :bitlist]) do
    :bitlist
  end

  defp t_join_types([:binary, :bitstring]) do
    :bitstring
  end

  defp t_join_types([:binary, :integer]) do
    :binary
  end

  defp t_join_types([:binary, :iolist]) do
    :iolist
  end

  defp t_join_types([:binary, nil]) do
    :iolist
  end

  defp t_join_types([:bitstring, :integer]) do
    :any
  end

  defp t_join_types([:bitstring, :iolist]) do
    :any
  end

  defp t_join_types([:bitstring, nil]) do
    :any
  end

  defp t_join_types([:integer, _]) do
    :any
  end

  defp t_join_types([:iolist, nil]) do
    :iolist
  end

  defp t_type({t, _}) do
    t
  end

  defp t_range({_, range}) do
    range
  end

  defp t__cons_type({:align, _}) do
    :align
  end

  defp t__cons_type({:any, _}) do
    :any
  end

  defp t__cons_type({:binary, _}) do
    :binary
  end

  defp t__cons_type({:bitstring, _}) do
    :bitstring
  end

  defp t__cons_type({:bitlist, _}) do
    :bitstring
  end

  defp t__cons_type({:integer, _}) do
    :binary
  end

  defp t__cons_type({:iolist, _}) do
    :binary
  end

  defp t__cons_type({nil, _}) do
    :binary
  end

  defp t__cons_range({:integer, _}) do
    {8, 8}
  end

  defp t__cons_range({_, range}) do
    range
  end

  defp t__cons_ranges({lb1, ub1}, {lb2, ub2}) do
    {lb1 + lb2, ub1 + ub2}
  end

  defp t__cons_ranges(:any, _) do
    :any
  end

  defp t__cons_ranges(_, :any) do
    :any
  end

  defp t__range({lb, ub} = range)
       when is_integer(lb) and
              is_integer(ub) do
    range
  end

  defp t__range(:any) do
    :any
  end

  defp t__range(val) when is_integer(val) do
    {val, val}
  end

  defp enc_cg({:cons, _, _} = cons) do
    enc_cg_cons(cons)
  end

  defp enc_cg({:block, imm}) do
    emit(['begin', :nl])
    enc_cg(imm)
    emit([:nl, 'end'])
  end

  defp enc_cg({:seq, {:comment, comment}, then}) do
    emit(['%% ', comment, :nl])
    enc_cg(then)
  end

  defp enc_cg({:seq, first, then}) do
    enc_cg(first)
    emit([:com, :nl])
    enc_cg(then)
  end

  defp enc_cg(:align) do
    emit(:align)
  end

  defp enc_cg({:apply, f0, as0}) do
    as = enc_call_args(as0, '')

    case f0 do
      {:local, f, _} when is_atom(f) ->
        emit([{:asis, f}, '(', as, ')'])

      {m, f, _} ->
        emit([{:asis, m}, ':', {:asis, f}, '(', as, ')'])
    end
  end

  defp enc_cg({:assign, dst0, expr}) do
    dst = mk_val(dst0)
    emit([dst, ' = ', expr])
  end

  defp enc_cg({:binary, putBits}) do
    emit(['<<', enc_cg_put_bits(putBits, ''), '>>'])
  end

  defp enc_cg({:call, m, f, as0}) do
    as =
      for a <- as0 do
        mk_val(a)
      end

    :asn1ct_func.call(m, f, as)
  end

  defp enc_cg({:call, m, f, as0, dst}) do
    as =
      for a <- as0 do
        mk_val(a)
      end

    emit([mk_val(dst), ' = '])
    :asn1ct_func.call(m, f, as)
  end

  defp enc_cg({:call_gen, prefix, key, gen, _, as0}) do
    as =
      for a <- as0 do
        mk_val(a)
      end

    :asn1ct_func.call_gen(prefix, key, gen, as)
  end

  defp enc_cg({:cond, cs}) do
    enc_cg_cond(cs)
  end

  defp enc_cg({:error, error}) when is_function(error, 0) do
    error.()
  end

  defp enc_cg({:error, {tag, var0}}) do
    var = mk_val(var0)
    emit(['exit({error,{asn1,{', tag, ',', var, '}}})'])
  end

  defp enc_cg({:integer, int}) do
    emit(mk_val(int))
  end

  defp enc_cg({:lc, body, var, list}) do
    emit('[')
    enc_cg(body)
    emit([' || ', mk_val(var), ' <- ', mk_val(list), ']'])
  end

  defp enc_cg({:lc, body, var, list, dst}) do
    emit([mk_val(dst), ' = ['])
    enc_cg(body)
    emit([' || ', mk_val(var), ' <- ', mk_val(list), ']'])
  end

  defp enc_cg({:list, list, dst}) do
    emit([mk_val(dst), ' = '])
    enc_cg(list)
  end

  defp enc_cg(nil) do
    emit('[]')
  end

  defp enc_cg({:sub, src0, int, dst0}) do
    src = mk_val(src0)
    dst = mk_val(dst0)
    emit([dst, ' = ', src, ' - ', int])
  end

  defp enc_cg({:set, {:var, src}, {:var, dst}}) do
    emit([dst, ' = ', src])
  end

  defp enc_cg({:try, try, {p, succ}, else__, dst}) do
    emit([mk_val(dst), ' = try '])
    enc_cg(try)
    emit([' of', :nl, mk_val(p), ' ->', :nl])
    enc_cg(succ)
    emit([:nl, 'catch throw:invalid ->', :nl])
    enc_cg(else__)
    emit([:nl, 'end'])
  end

  defp enc_cg({:var, v}) do
    emit(v)
  end

  defp enc_cg_cons(cons) do
    emit('[')
    enc_cg_cons_1(cons)
    emit(']')
  end

  defp enc_cg_cons_1({:cons, h, {:cons, _, _} = t}) do
    enc_cg(h)
    emit([:com, :nl])
    enc_cg_cons_1(t)
  end

  defp enc_cg_cons_1({:cons, h, nil}) do
    enc_cg(h)
  end

  defp enc_cg_cons_1({:cons, h, t}) do
    enc_cg(h)
    emit('|')
    enc_cg(t)
  end

  defp enc_call_args([a | as], sep) do
    [[sep, mk_val(a)] | enc_call_args(as, ', ')]
  end

  defp enc_call_args([], _) do
    []
  end

  defp enc_cg_cond(cs) do
    emit('if ')
    enc_cg_cond(cs, '')
    emit([:nl, 'end'])
  end

  defp enc_cg_cond([c | cs], sep) do
    emit(sep)
    enc_cg_cond_1(c)
    enc_cg_cond(cs, [';', :nl])
  end

  defp enc_cg_cond([], _) do
    :ok
  end

  defp enc_cg_cond_1({cond__, action}) do
    enc_cond_term(cond__)
    emit([' ->', :nl])
    enc_cg(action)
  end

  defp enc_cond_term(:_) do
    emit('true')
  end

  defp enc_cond_term({:ult, var0, int}) do
    var = mk_val(var0)
    n = uper_num_bits(int)

    case 1 <<< n do
      ^int ->
        emit([var, ' bsr ', n, ' =:= 0'])

      _ ->
        emit(['0 =< ', var, ', ', var, ' < ', int])
    end
  end

  defp enc_cond_term({:eq, var0, term}) do
    var = mk_val(var0)
    emit([var, ' =:= ', {:asis, term}])
  end

  defp enc_cond_term({:ge, var0, int}) do
    var = mk_val(var0)
    emit([var, ' >= ', int])
  end

  defp enc_cond_term({:lt, var0, int}) do
    var = mk_val(var0)
    emit([var, ' < ', int])
  end

  defp enc_cg_put_bits([{:put_bits, val0, n, [1]} | t], sep) do
    val = mk_val(val0)

    [
      [sep, val, ':', :erlang.integer_to_list(n)]
      | enc_cg_put_bits(t, ',')
    ]
  end

  defp enc_cg_put_bits([], _) do
    []
  end

  defp mk_val({:var, str}) do
    str
  end

  defp mk_val({:expr, str}) do
    str
  end

  defp mk_val(int) when is_integer(int) do
    :erlang.integer_to_list(int)
  end

  defp mk_val(other) do
    {:asis, other}
  end

  defp bit_string_name2pos_fun(nNL, src) do
    {:call_gen, 'bit_string_name2pos_', nNL,
     fn fd, name ->
       gen_name2pos(fd, name, nNL)
     end, [], [src]}
  end

  defp gen_name2pos(fd, name, names) do
    cs0 = gen_name2pos_cs(names, name)
    cs = cs0 ++ [bit_clause(name), nil_clause(), invalid_clause()]
    f0 = {:function, 1, name, 1, cs}
    f = :erl_parse.new_anno(f0)
    :file.write(fd, [:erl_pp.function(f)])
  end

  defp gen_name2pos_cs([{k, v} | t], name) do
    p = [{:cons, 0, {:atom, 0, k}, {:var, 0, :T}}]
    b = [{:cons, 0, {:integer, 0, v}, {:call, 0, {:atom, 0, name}, [{:var, 0, :T}]}}]
    [{:clause, 0, p, [], b} | gen_name2pos_cs(t, name)]
  end

  defp gen_name2pos_cs([], _) do
    []
  end

  defp bit_clause(name) do
    varT = {:var, 0, :T}
    varPos = {:var, 0, :Pos}
    p = [{:cons, 0, {:tuple, 0, [{:atom, 0, :bit}, varPos]}, varT}]
    g = [[{:call, 0, {:atom, 0, :is_integer}, [varPos]}]]
    b = [{:cons, 0, varPos, {:call, 0, {:atom, 0, name}, [varT]}}]
    {:clause, 0, p, g, b}
  end

  defp nil_clause() do
    p = b = [{nil, 0}]
    {:clause, 0, p, [], b}
  end

  defp invalid_clause() do
    p = [{:var, 0, :_}]
    b = [{:call, 0, {:atom, 0, :throw}, [{:atom, 0, :invalid}]}]
    {:clause, 0, p, [], b}
  end

  defp enc_hoist_align(imm0) do
    imm = enc_hoist_align_reverse(imm0, [])
    enc_hoist_align(imm, false, [])
  end

  defp enc_hoist_align_reverse([h | t], acc) do
    case enc_opt_al_1([h], 0) do
      {[^h], _} ->
        enc_hoist_align_reverse(t, [h | acc])

      {_, _} ->
        :lists.reverse(t, [[h, :stop] | acc])
    end
  end

  defp enc_hoist_align_reverse([], acc) do
    acc
  end

  defp enc_hoist_align([:stop | t], _Aligned, acc) do
    :lists.reverse(t, acc)
  end

  defp enc_hoist_align([{:block, bl0} | t], aligned, acc) do
    bl =
      case aligned do
        false ->
          bl0

        true ->
          enc_hoist_block(bl0)
      end

    case is_beginning_aligned(bl) do
      false ->
        enc_hoist_align(t, false, [{:block, bl} | acc])

      true ->
        enc_hoist_align(t, true, [
          [{:put_bits, 0, 0, [1, :align]}, {:block, bl}]
          | acc
        ])
    end
  end

  defp enc_hoist_align([h | t], _, acc) do
    enc_hoist_align(t, false, [h | acc])
  end

  defp enc_hoist_align([], _, acc) do
    acc
  end

  defp enc_hoist_block(bl) do
    try do
      enc_hoist_block_1(:lists.reverse(bl))
    catch
      :impossible ->
        bl
    end
  end

  defp enc_hoist_block_1([{:cond, cs0} | t]) do
    cs =
      for [c | act] <- cs0 do
        [c | enc_hoist_block_2(act)]
      end

    h = {:cond, cs}
    :lists.reverse(t, [h])
  end

  defp enc_hoist_block_1(_) do
    throw(:impossible)
  end

  defp enc_hoist_block_2([{:cond, _} | _] = l) do
    enc_hoist_block(l)
  end

  defp enc_hoist_block_2([{:error, _}] = l) do
    l
  end

  defp enc_hoist_block_2([]) do
    [{:put_bits, 0, 0, [1, :align]}]
  end

  defp enc_hoist_block_2(l) do
    case :lists.last(l) do
      {:put_bits, _, _, _} ->
        l ++ [{:put_bits, 0, 0, [1, :align]}]

      _ ->
        throw(:impossible)
    end
  end

  defp enc_opt_al(imm0) do
    {imm, _} = enc_opt_al_1(imm0, :unknown)
    imm
  end

  defp enc_opt_al_1([h0 | t0], al0) do
    {h, al1} = enc_opt_al(h0, al0)
    {t, al} = enc_opt_al_1(t0, al1)
    {h ++ t, al}
  end

  defp enc_opt_al_1([], al) do
    {[], al}
  end

  defp enc_opt_al({:assign, _, _} = imm, al) do
    {[imm], al}
  end

  defp enc_opt_al({:block, bl0}, al0) do
    {bl, al} = enc_opt_al_1(bl0, al0)
    {[{:block, bl}], al}
  end

  defp enc_opt_al(
         {:call, :erlang, :iolist_to_binary, [_]} = imm,
         al
       ) do
    {[imm], al}
  end

  defp enc_opt_al(
         {:call, :per_common, :encode_fragmented, [_, u]} = call,
         al
       ) do
    case rem(u, 8) do
      0 ->
        {[call], al}

      _ ->
        {[call], :unknown}
    end
  end

  defp enc_opt_al(
         {:call, :per_common, :encode_unconstrained_number, [_]} = call,
         _
       ) do
    {[call], 0}
  end

  defp enc_opt_al({:call, _, _, _, _} = call, al) do
    {[call], al}
  end

  defp enc_opt_al({:comment, _} = imm, al) do
    {[imm], al}
  end

  defp enc_opt_al({:cond, cs0}, al0) do
    {cs, al} = enc_opt_al_cond(cs0, al0)
    {[{:cond, cs}], al}
  end

  defp enc_opt_al({:error, _} = imm, al) do
    {[imm], al}
  end

  defp enc_opt_al({:list, imm0, dst}, al) do
    imm1 = enc_opt_hoist_align(imm0)
    {imm, _} = enc_opt_al_1(imm1, 0)
    {[{:list, imm, dst}], al}
  end

  defp enc_opt_al({:put_bits, v, n, [u, :align]}, al0)
       when rem(al0, 8) === 0 do
    al =
      cond do
        is_integer(n) ->
          n * u

        n === :binary and rem(u, 8) === 0 ->
          0

        true ->
          :unknown
      end

    {[{:put_bits, v, n, [u]}], al}
  end

  defp enc_opt_al({:put_bits, v, :binary, [u, :align]}, al0)
       when is_integer(al0) do
    n = 8 - rem(al0, 8)

    al =
      case rem(u, 8) do
        0 ->
          0

        _ ->
          :unknown
      end

    {[{:put_bits, 0, n, [1]}, {:put_bits, v, :binary, [u]}], al}
  end

  defp enc_opt_al({:put_bits, v, n0, [u, :align]}, al0)
       when is_integer(n0) and is_integer(al0) do
    n = n0 + (8 - rem(al0, 8))
    al = n0 * u
    {[{:put_bits, v, n, [1]}], al}
  end

  defp enc_opt_al({:put_bits, _, n, [u, :align]} = putBits, _)
       when is_integer(n) do
    {[putBits], n * u}
  end

  defp enc_opt_al(
         {:put_bits, _, :binary, [u, :align]} = putBits,
         _
       )
       when rem(u, 8) === 0 do
    {[putBits], 0}
  end

  defp enc_opt_al({:put_bits, _, n, [u]} = putBits, al)
       when is_integer(n) and is_integer(al) do
    {[putBits], al + n * u}
  end

  defp enc_opt_al({:put_bits, _, :binary, [u]} = putBits, al)
       when rem(u, 8) === 0 do
    {[putBits], al}
  end

  defp enc_opt_al({:set, _, _} = imm, al) do
    {[imm], al}
  end

  defp enc_opt_al({:sub, _, _, _} = imm, al) do
    {[imm], al}
  end

  defp enc_opt_al({:try, _, _, _, _} = imm, al) do
    {[imm], al}
  end

  defp enc_opt_al(imm, _) do
    {[imm], :unknown}
  end

  defp enc_opt_al_cond(cs0, al0) do
    enc_opt_al_cond_1(cs0, al0, [], [])
  end

  defp enc_opt_al_cond_1([[:_, {:error, _}] = c | cs], al, cAcc, aAcc) do
    enc_opt_al_cond_1(cs, al, [c | cAcc], aAcc)
  end

  defp enc_opt_al_cond_1([[c | act0] | cs0], al0, cAcc, aAcc) do
    {act, al1} = enc_opt_al_1(act0, al0)

    al =
      cond do
        al1 === :unknown ->
          al1

        true ->
          rem(al1, 8)
      end

    enc_opt_al_cond_1(cs0, al0, [[c | act] | cAcc], [al | aAcc])
  end

  defp enc_opt_al_cond_1([], _, cAcc, aAcc) do
    al =
      case :lists.usort(aAcc) do
        [] ->
          :unknown

        [al0] ->
          al0

        [_ | _] ->
          :unknown
      end

    {:lists.reverse(cAcc), al}
  end

  defp enc_opt_hoist_align([{:cond, cs0}, {:put_bits, 0, 0, [1, :align]}] = imm) do
    try do
      cs =
        for c <- cs0 do
          insert_align_last(c)
        end

      [{:cond, cs}]
    catch
      :impossible ->
        imm
    end
  end

  defp enc_opt_hoist_align(imm) do
    imm
  end

  defp insert_align_last([_, {:error, _}] = c) do
    c
  end

  defp insert_align_last([h | t]) do
    case :lists.last(t) do
      {:put_bits, _, _, _} ->
        [h | t ++ [{:put_bits, 0, 0, [1, :align]}]]

      _ ->
        throw(:impossible)
    end
  end

  defp per_fixup([{:apply, _, _} = h | t]) do
    [h | per_fixup(t)]
  end

  defp per_fixup([{:block, block} | t]) do
    [{:block, per_fixup(block)} | per_fixup(t)]
  end

  defp per_fixup([{:assign, _, _} = h | t]) do
    [h | per_fixup(t)]
  end

  defp per_fixup([{:comment, _} = h | t]) do
    [h | per_fixup(t)]
  end

  defp per_fixup([{:cond, cs0} | t]) do
    cs =
      for [c | act] <- cs0 do
        [c | per_fixup(act)]
      end

    [{:cond, cs} | per_fixup(t)]
  end

  defp per_fixup([{:call, _, _, _} = h | t]) do
    [h | per_fixup(t)]
  end

  defp per_fixup([{:call, _, _, _, _} = h | t]) do
    [h | per_fixup(t)]
  end

  defp per_fixup([{:call_gen, _, _, _, _, _} = h | t]) do
    [h | per_fixup(t)]
  end

  defp per_fixup([{:error, _} = h | t]) do
    [h | per_fixup(t)]
  end

  defp per_fixup([{:lc, b, v, l} | t]) do
    [{:lc, per_fixup(b), v, l} | per_fixup(t)]
  end

  defp per_fixup([{:lc, b, v, l, dst} | t]) do
    [{:lc, per_fixup(b), v, l, dst} | per_fixup(t)]
  end

  defp per_fixup([{:list, imm, dst} | t]) do
    [{:list, per_fixup(imm), dst} | per_fixup(t)]
  end

  defp per_fixup([{:set, _, _} = h | t]) do
    [h | per_fixup(t)]
  end

  defp per_fixup([{:sub, _, _, _} = h | t]) do
    [h | per_fixup(t)]
  end

  defp per_fixup([{:try, try0, {p, succ0}, else0, dst} | t]) do
    try = per_fixup(try0)
    succ = per_fixup(succ0)
    else__ = per_fixup(else0)
    [{:try, try, {p, succ}, else__, dst} | per_fixup(t)]
  end

  defp per_fixup([{:put_bits, _, _, _} | _] = l) do
    fixup_put_bits(l)
  end

  defp per_fixup([{:var, _} = h | t]) do
    [h | per_fixup(t)]
  end

  defp per_fixup([]) do
    []
  end

  defp fixup_put_bits([{:put_bits, 0, 0, [_, :align]} | t]) do
    [:align | fixup_put_bits(t)]
  end

  defp fixup_put_bits([{:put_bits, 0, 0, _} | t]) do
    fixup_put_bits(t)
  end

  defp fixup_put_bits([{:put_bits, v, n, [u, :align]} | t]) do
    [[:align, {:put_bits, v, n, [u]}] | fixup_put_bits(t)]
  end

  defp fixup_put_bits([{:put_bits, _, _, _} = h | t]) do
    [h | fixup_put_bits(t)]
  end

  defp fixup_put_bits(other) do
    per_fixup(other)
  end

  def effective_constraint(:integer, [{{_, _} = root, _} | _Rest]) do
    [{root, []}]
  end

  def effective_constraint(:integer, c) do
    sVs = get_constraints(c, :SingleValue)
    sV = effective_constr(:SingleValue, sVs)
    vRs = get_constraints(c, :ValueRange)
    vR = effective_constr(:ValueRange, vRs)
    greatest_common_range(sV, vR)
  end

  def effective_constraint(:bitstring, c) do
    case get_constraint(c, :SizeConstraint) do
      {{lb, ub}, []} = range when is_integer(lb) ->
        cond do
          is_integer(ub) and ub < 65536 ->
            range

          true ->
            :no
        end

      {lb, ub} = range when is_integer(lb) ->
        cond do
          is_integer(ub) and ub < 65536 ->
            cond do
              lb === ub ->
                lb

              true ->
                range
            end

          true ->
            :no
        end

      :no ->
        :no
    end
  end

  defp effective_constr(_, []) do
    []
  end

  defp effective_constr(:SingleValue, list) do
    sVList =
      :lists.flatten(
        :lists.map(
          fn x ->
            :erlang.element(2, x)
          end,
          list
        )
      )

    case :lists.usort(sVList) do
      [n] ->
        [{:SingleValue, n}]

      [_ | _] = l ->
        [{:ValueRange, {least_Lb(l), greatest_Ub(l)}}]
    end
  end

  defp effective_constr(:ValueRange, list) do
    lBs =
      :lists.map(
        fn {_, {lb, _}} ->
          lb
        end,
        list
      )

    uBs =
      :lists.map(
        fn {_, {_, ub}} ->
          ub
        end,
        list
      )

    lb = least_Lb(lBs)
    [{:ValueRange, {lb, :lists.max(uBs)}}]
  end

  defp greatest_common_range([], vR) do
    vR
  end

  defp greatest_common_range(sV, []) do
    sV
  end

  defp greatest_common_range([{_, int}], [{_, {:MIN, ub}}])
       when is_integer(int) and int > ub do
    [{:ValueRange, {:MIN, int}}]
  end

  defp greatest_common_range([{_, int}], [{_, {lb, ub}}])
       when is_integer(int) and int < lb do
    [{:ValueRange, {int, ub}}]
  end

  defp greatest_common_range([{_, int}], vR = [{_, {_Lb, _Ub}}])
       when is_integer(int) do
    vR
  end

  defp greatest_common_range([{_, l}], [{_, {lb, ub}}]) when is_list(l) do
    min = least_Lb([lb | l])
    max = greatest_Ub([ub | l])
    [{:ValueRange, {min, max}}]
  end

  defp greatest_common_range([{_, {lb1, ub1}}], [{_, {lb2, ub2}}]) do
    min = least_Lb([lb1, lb2])
    max = greatest_Ub([ub1, ub2])
    [{:ValueRange, {min, max}}]
  end

  defp least_Lb(l) do
    case :lists.member(:MIN, l) do
      true ->
        :MIN

      false ->
        :lists.min(l)
    end
  end

  defp greatest_Ub(l) do
    case :lists.member(:MAX, l) do
      true ->
        :MAX

      false ->
        :lists.max(l)
    end
  end

  defp get_constraint(c, key) do
    case :lists.keyfind(key, 1, c) do
      false ->
        :no

      {_, v} ->
        v
    end
  end

  defp get_constraints([{key, _} = pair | t], key) do
    [pair | get_constraints(t, key)]
  end

  defp get_constraints([_ | t], key) do
    get_constraints(t, key)
  end

  defp get_constraints([], _) do
    []
  end
end
