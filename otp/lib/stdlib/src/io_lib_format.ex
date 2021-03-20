defmodule :m_io_lib_format do
  use Bitwise

  def fwrite(format, args) do
    build(scan(format, args))
  end

  def fwrite(format, args, options) do
    build(scan(format, args), options)
  end

  def build(cs) do
    build(cs, [])
  end

  def build(cs, options) do
    charsLimit = get_option(:chars_limit, options, -1)
    res1 = build_small(cs)
    {p, s, w, other} = count_small(res1)

    case p + s + w do
      0 ->
        res1

      numOfLimited ->
        remainingChars = sub(charsLimit, other)
        build_limited(res1, p, numOfLimited, remainingChars, 0)
    end
  end

  def scan(format, args) when is_atom(format) do
    scan(:erlang.atom_to_list(format), args)
  end

  def scan(format, args) when is_binary(format) do
    scan(:erlang.binary_to_list(format), args)
  end

  def scan(format, args) do
    collect(format, args)
  end

  def unscan(cs) do
    {print(cs), args(cs)}
  end

  defp args([%{args: as} | cs]) do
    as ++ args(cs)
  end

  defp args([_C | cs]) do
    args(cs)
  end

  defp args([]) do
    []
  end

  defp print([
         %{
           control_char: c,
           width: f,
           adjust: ad,
           precision: p,
           pad_char: pad,
           encoding: encoding,
           strings: strings
         }
         | cs
       ]) do
    print(c, f, ad, p, pad, encoding, strings) ++ print(cs)
  end

  defp print([c | cs]) do
    [c | print(cs)]
  end

  defp print([]) do
    []
  end

  defp print(c, f, ad, p, pad, encoding, strings) do
    [?~] ++
      print_field_width(f, ad) ++
      print_precision(
        p,
        pad
      ) ++ print_pad_char(pad) ++ print_encoding(encoding) ++ print_strings(strings) ++ [c]
  end

  defp print_field_width(:none, _Ad) do
    ''
  end

  defp print_field_width(f, :left) do
    :erlang.integer_to_list(-f)
  end

  defp print_field_width(f, :right) do
    :erlang.integer_to_list(f)
  end

  defp print_precision(:none, ?\s) do
    ''
  end

  defp print_precision(:none, _Pad) do
    '.'
  end

  defp print_precision(p, _Pad) do
    [?. | :erlang.integer_to_list(p)]
  end

  defp print_pad_char(?\s) do
    ''
  end

  defp print_pad_char(pad) do
    [?., pad]
  end

  defp print_encoding(:unicode) do
    't'
  end

  defp print_encoding(:latin1) do
    ''
  end

  defp print_strings(false) do
    'l'
  end

  defp print_strings(true) do
    ''
  end

  defp collect([?~ | fmt0], args0) do
    {c, fmt1, args1} = collect_cseq(fmt0, args0)
    [c | collect(fmt1, args1)]
  end

  defp collect([c | fmt], args) do
    [c | collect(fmt, args)]
  end

  defp collect([], []) do
    []
  end

  defp collect_cseq(fmt0, args0) do
    {f, ad, fmt1, args1} = field_width(fmt0, args0)
    {p, fmt2, args2} = precision(fmt1, args1)
    {pad, fmt3, args3} = pad_char(fmt2, args2)
    spec0 = %{width: f, adjust: ad, precision: p, pad_char: pad, encoding: :latin1, strings: true}
    {spec1, fmt4} = modifiers(fmt3, spec0)
    {c, as, fmt5, args4} = collect_cc(fmt4, args3)
    spec2 = Map.merge(spec1, %{control_char: c, args: as})
    {spec2, fmt5, args4}
  end

  defp modifiers([?t | fmt], spec) do
    modifiers(fmt, Map.put(spec, :encoding, :unicode))
  end

  defp modifiers([?l | fmt], spec) do
    modifiers(fmt, Map.put(spec, :strings, false))
  end

  defp modifiers(fmt, spec) do
    {spec, fmt}
  end

  defp field_width([?- | fmt0], args0) do
    {f, fmt, args} = field_value(fmt0, args0)
    field_width(-f, fmt, args)
  end

  defp field_width(fmt0, args0) do
    {f, fmt, args} = field_value(fmt0, args0)
    field_width(f, fmt, args)
  end

  defp field_width(f, fmt, args) when f < 0 do
    {-f, :left, fmt, args}
  end

  defp field_width(f, fmt, args) when f >= 0 do
    {f, :right, fmt, args}
  end

  defp precision([?. | fmt], args) do
    field_value(fmt, args)
  end

  defp precision(fmt, args) do
    {:none, fmt, args}
  end

  defp field_value([?* | fmt], [a | args]) when is_integer(a) do
    {a, fmt, args}
  end

  defp field_value([c | fmt], args)
       when is_integer(c) and
              c >= ?0 and c <= ?9 do
    field_value([c | fmt], args, 0)
  end

  defp field_value(fmt, args) do
    {:none, fmt, args}
  end

  defp field_value([c | fmt], args, f)
       when is_integer(c) and
              c >= ?0 and c <= ?9 do
    field_value(fmt, args, 10 * f + (c - ?0))
  end

  defp field_value(fmt, args, f) do
    {f, fmt, args}
  end

  defp pad_char([?., ?* | fmt], [pad | args]) do
    {pad, fmt, args}
  end

  defp pad_char([?., pad | fmt], args) do
    {pad, fmt, args}
  end

  defp pad_char(fmt, args) do
    {?\s, fmt, args}
  end

  defp collect_cc([?w | fmt], [a | args]) do
    {?w, [a], fmt, args}
  end

  defp collect_cc([?p | fmt], [a | args]) do
    {?p, [a], fmt, args}
  end

  defp collect_cc([?W | fmt], [a, depth | args]) do
    {?W, [a, depth], fmt, args}
  end

  defp collect_cc([?P | fmt], [a, depth | args]) do
    {?P, [a, depth], fmt, args}
  end

  defp collect_cc([?s | fmt], [a | args]) do
    {?s, [a], fmt, args}
  end

  defp collect_cc([?e | fmt], [a | args]) do
    {?e, [a], fmt, args}
  end

  defp collect_cc([?f | fmt], [a | args]) do
    {?f, [a], fmt, args}
  end

  defp collect_cc([?g | fmt], [a | args]) do
    {?g, [a], fmt, args}
  end

  defp collect_cc([?b | fmt], [a | args]) do
    {?b, [a], fmt, args}
  end

  defp collect_cc([?B | fmt], [a | args]) do
    {?B, [a], fmt, args}
  end

  defp collect_cc([?x | fmt], [a, prefix | args]) do
    {?x, [a, prefix], fmt, args}
  end

  defp collect_cc([?X | fmt], [a, prefix | args]) do
    {?X, [a, prefix], fmt, args}
  end

  defp collect_cc([?+ | fmt], [a | args]) do
    {?+, [a], fmt, args}
  end

  defp collect_cc([?# | fmt], [a | args]) do
    {?#, [a], fmt, args}
  end

  defp collect_cc([?c | fmt], [a | args]) do
    {?c, [a], fmt, args}
  end

  defp collect_cc([?~ | fmt], args) when is_list(args) do
    {?~, [], fmt, args}
  end

  defp collect_cc([?n | fmt], args) when is_list(args) do
    {?n, [], fmt, args}
  end

  defp collect_cc([?i | fmt], [a | args]) do
    {?i, [a], fmt, args}
  end

  defp count_small(cs) do
    count_small(cs, %{p: 0, s: 0, w: 0, other: 0})
  end

  defp count_small([%{control_char: ?p} | cs], %{p: p} = cnts) do
    count_small(cs, %{cnts | p: p + 1})
  end

  defp count_small([%{control_char: ?P} | cs], %{p: p} = cnts) do
    count_small(cs, %{cnts | p: p + 1})
  end

  defp count_small([%{control_char: ?w} | cs], %{w: w} = cnts) do
    count_small(cs, %{cnts | w: w + 1})
  end

  defp count_small([%{control_char: ?W} | cs], %{w: w} = cnts) do
    count_small(cs, %{cnts | w: w + 1})
  end

  defp count_small([%{control_char: ?s} | cs], %{w: w} = cnts) do
    count_small(cs, %{cnts | w: w + 1})
  end

  defp count_small([s | cs], %{other: other} = cnts)
       when is_list(s) or is_binary(s) do
    count_small(
      cs,
      %{cnts | other: other + :io_lib.chars_length(s)}
    )
  end

  defp count_small([c | cs], %{other: other} = cnts)
       when is_integer(c) do
    count_small(cs, %{cnts | other: other + 1})
  end

  defp count_small([], %{p: p, s: s, w: w, other: other}) do
    {p, s, w, other}
  end

  defp build_small([
         %{
           control_char: c,
           args: as,
           width: f,
           adjust: ad,
           precision: p,
           pad_char: pad,
           encoding: enc
         } = cC
         | cs
       ]) do
    case control_small(c, as, f, ad, p, pad, enc) do
      :not_small ->
        [cC | build_small(cs)]

      s ->
        :lists.flatten(s) ++ build_small(cs)
    end
  end

  defp build_small([c | cs]) do
    [c | build_small(cs)]
  end

  defp build_small([]) do
    []
  end

  defp build_limited(
         [
           %{
             control_char: c,
             args: as,
             width: f,
             adjust: ad,
             precision: p,
             pad_char: pad,
             encoding: enc,
             strings: str
           }
           | cs
         ],
         numOfPs0,
         count0,
         maxLen0,
         i
       ) do
    maxChars =
      cond do
        maxLen0 < 0 ->
          maxLen0

        true ->
          div(maxLen0, count0)
      end

    s = control_limited(c, as, f, ad, p, pad, enc, str, maxChars, i)
    numOfPs = decr_pc(c, numOfPs0)
    count = count0 - 1

    maxLen =
      cond do
        maxLen0 < 0 ->
          maxLen0

        true ->
          len = :io_lib.chars_length(s)
          sub(maxLen0, len)
      end

    cond do
      numOfPs > 0 ->
        [s | build_limited(cs, numOfPs, count, maxLen, indentation(s, i))]

      true ->
        [s | build_limited(cs, numOfPs, count, maxLen, i)]
    end
  end

  defp build_limited([?\n | cs], numOfPs, count, maxLen, _I) do
    [?\n | build_limited(cs, numOfPs, count, maxLen, 0)]
  end

  defp build_limited([?\t | cs], numOfPs, count, maxLen, i) do
    [?\t | build_limited(cs, numOfPs, count, maxLen, div(i + 8, 8) * 8)]
  end

  defp build_limited([c | cs], numOfPs, count, maxLen, i) do
    [c | build_limited(cs, numOfPs, count, maxLen, i + 1)]
  end

  defp build_limited([], _, _, _, _) do
    []
  end

  defp decr_pc(?p, pc) do
    pc - 1
  end

  defp decr_pc(?P, pc) do
    pc - 1
  end

  defp decr_pc(_, pc) do
    pc
  end

  def indentation([?\n | cs], _I) do
    indentation(cs, 0)
  end

  def indentation([?\t | cs], i) do
    indentation(cs, div(i + 8, 8) * 8)
  end

  def indentation([c | cs], i) when is_integer(c) do
    indentation(cs, i + 1)
  end

  def indentation([c | cs], i) do
    indentation(cs, indentation(c, i))
  end

  def indentation([], i) do
    i
  end

  defp control_small(?s, [a], f, adj, p, pad, :latin1 = enc)
       when is_atom(a) do
    l = iolist_to_chars(:erlang.atom_to_list(a))
    string(l, f, adj, p, pad, enc)
  end

  defp control_small(?s, [a], f, adj, p, pad, :unicode = enc)
       when is_atom(a) do
    string(:erlang.atom_to_list(a), f, adj, p, pad, enc)
  end

  defp control_small(?e, [a], f, adj, p, pad, _Enc)
       when is_float(a) do
    fwrite_e(a, f, adj, p, pad)
  end

  defp control_small(?f, [a], f, adj, p, pad, _Enc)
       when is_float(a) do
    fwrite_f(a, f, adj, p, pad)
  end

  defp control_small(?g, [a], f, adj, p, pad, _Enc)
       when is_float(a) do
    fwrite_g(a, f, adj, p, pad)
  end

  defp control_small(?b, [a], f, adj, p, pad, _Enc)
       when is_integer(a) do
    unprefixed_integer(a, f, adj, base(p), pad, true)
  end

  defp control_small(?B, [a], f, adj, p, pad, _Enc)
       when is_integer(a) do
    unprefixed_integer(a, f, adj, base(p), pad, false)
  end

  defp control_small(?x, [a, prefix], f, adj, p, pad, _Enc)
       when is_integer(a) and is_atom(prefix) do
    prefixed_integer(a, f, adj, base(p), pad, :erlang.atom_to_list(prefix), true)
  end

  defp control_small(?x, [a, prefix], f, adj, p, pad, _Enc)
       when is_integer(a) do
    true = :io_lib.deep_char_list(prefix)
    prefixed_integer(a, f, adj, base(p), pad, prefix, true)
  end

  defp control_small(?X, [a, prefix], f, adj, p, pad, _Enc)
       when is_integer(a) and is_atom(prefix) do
    prefixed_integer(a, f, adj, base(p), pad, :erlang.atom_to_list(prefix), false)
  end

  defp control_small(?X, [a, prefix], f, adj, p, pad, _Enc)
       when is_integer(a) do
    true = :io_lib.deep_char_list(prefix)
    prefixed_integer(a, f, adj, base(p), pad, prefix, false)
  end

  defp control_small(?+, [a], f, adj, p, pad, _Enc)
       when is_integer(a) do
    base = base(p)
    prefix = [:erlang.integer_to_list(base), ?#]
    prefixed_integer(a, f, adj, base, pad, prefix, true)
  end

  defp control_small(?#, [a], f, adj, p, pad, _Enc)
       when is_integer(a) do
    base = base(p)
    prefix = [:erlang.integer_to_list(base), ?#]
    prefixed_integer(a, f, adj, base, pad, prefix, false)
  end

  defp control_small(?c, [a], f, adj, p, pad, :unicode)
       when is_integer(a) do
    char(a, f, adj, p, pad)
  end

  defp control_small(?c, [a], f, adj, p, pad, _Enc)
       when is_integer(a) do
    char(a &&& 255, f, adj, p, pad)
  end

  defp control_small(?~, [], f, adj, p, pad, _Enc) do
    char(?~, f, adj, p, pad)
  end

  defp control_small(?n, [], f, adj, p, pad, _Enc) do
    newline(f, adj, p, pad)
  end

  defp control_small(?i, [_A], _F, _Adj, _P, _Pad, _Enc) do
    []
  end

  defp control_small(_C, _As, _F, _Adj, _P, _Pad, _Enc) do
    :not_small
  end

  defp control_limited(?s, [l0], f, adj, p, pad, :latin1 = enc, _Str, cL, _I) do
    l = iolist_to_chars(l0, f, cL)
    string(l, limit_field(f, cL), adj, p, pad, enc)
  end

  defp control_limited(?s, [l0], f, adj, p, pad, :unicode = enc, _Str, cL, _I) do
    l = cdata_to_chars(l0, f, cL)
    uniconv(string(l, limit_field(f, cL), adj, p, pad, enc))
  end

  defp control_limited(?w, [a], f, adj, p, pad, enc, _Str, cL, _I) do
    chars =
      :io_lib.write(
        a,
        [{:depth, -1}, {:encoding, enc}, {:chars_limit, cL}]
      )

    term(chars, f, adj, p, pad)
  end

  defp control_limited(?p, [a], f, adj, p, pad, enc, str, cL, i) do
    print(a, -1, f, adj, p, pad, enc, str, cL, i)
  end

  defp control_limited(?W, [a, depth], f, adj, p, pad, enc, _Str, cL, _I)
       when is_integer(depth) do
    chars =
      :io_lib.write(
        a,
        [{:depth, depth}, {:encoding, enc}, {:chars_limit, cL}]
      )

    term(chars, f, adj, p, pad)
  end

  defp control_limited(?P, [a, depth], f, adj, p, pad, enc, str, cL, i)
       when is_integer(depth) do
    print(a, depth, f, adj, p, pad, enc, str, cL, i)
  end

  defp uniconv(c) do
    c
  end

  defp base(:none) do
    10
  end

  defp base(b) when is_integer(b) do
    b
  end

  defp term(t, :none, _Adj, :none, _Pad) do
    t
  end

  defp term(t, :none, adj, p, pad) do
    term(t, p, adj, p, pad)
  end

  defp term(t, f, adj, p0, pad) do
    l = :io_lib.chars_length(t)

    p =
      :erlang.min(
        l,
        case p0 do
          :none ->
            f

          _ ->
            min(p0, f)
        end
      )

    cond do
      l > p ->
        adjust(chars(?*, p), chars(pad, f - p), adj)

      f >= p ->
        adjust(t, chars(pad, f - l), adj)
    end
  end

  defp print(t, d, :none, adj, p, pad, e, str, chLim, i) do
    print(t, d, 80, adj, p, pad, e, str, chLim, i)
  end

  defp print(t, d, f, adj, :none, pad, e, str, chLim, i) do
    print(t, d, f, adj, i + 1, pad, e, str, chLim, i)
  end

  defp print(t, d, f, :right, p, _Pad, enc, str, chLim, _I) do
    options = [
      {:chars_limit, chLim},
      {:column, p},
      {:line_length, f},
      {:depth, d},
      {:encoding, enc},
      {:strings, str}
    ]

    :io_lib_pretty.print(t, options)
  end

  defp fwrite_e(fl, :none, adj, :none, pad) do
    fwrite_e(fl, :none, adj, 6, pad)
  end

  defp fwrite_e(fl, :none, _Adj, p, _Pad) when p >= 2 do
    float_e(fl, float_data(fl), p)
  end

  defp fwrite_e(fl, f, adj, :none, pad) do
    fwrite_e(fl, f, adj, 6, pad)
  end

  defp fwrite_e(fl, f, adj, p, pad) when p >= 2 do
    term(float_e(fl, float_data(fl), p), f, adj, f, pad)
  end

  defp float_e(fl, fd, p) when fl < 0.0 do
    [?- | float_e(-fl, fd, p)]
  end

  defp float_e(_Fl, {ds, e}, p) do
    case float_man(ds, 1, p - 1) do
      {[?0 | fs], true} ->
        [[?1 | fs] | float_exp(e)]

      {fs, false} ->
        [fs | float_exp(e - 1)]
    end
  end

  defp float_man(ds, 0, dc) do
    {cs, c} = float_man(ds, dc)
    {[?. | cs], c}
  end

  defp float_man([d | ds], i, dc) do
    case float_man(ds, i - 1, dc) do
      {cs, true} when d === ?9 ->
        {[?0 | cs], true}

      {cs, true} ->
        {[d + 1 | cs], false}

      {cs, false} ->
        {[d | cs], false}
    end
  end

  defp float_man([], i, dc) do
    {:lists.duplicate(i, ?0) ++
       [
         ?.
         | :lists.duplicate(
             dc,
             ?0
           )
       ], false}
  end

  defp float_man([d | _], 0) when d >= ?5 do
    {[], true}
  end

  defp float_man([_ | _], 0) do
    {[], false}
  end

  defp float_man([d | ds], dc) do
    case float_man(ds, dc - 1) do
      {cs, true} when d === ?9 ->
        {[?0 | cs], true}

      {cs, true} ->
        {[d + 1 | cs], false}

      {cs, false} ->
        {[d | cs], false}
    end
  end

  defp float_man([], dc) do
    {:lists.duplicate(dc, ?0), false}
  end

  defp float_exp(e) when e >= 0 do
    [?e, ?+ | :erlang.integer_to_list(e)]
  end

  defp float_exp(e) do
    [?e | :erlang.integer_to_list(e)]
  end

  defp fwrite_f(fl, :none, adj, :none, pad) do
    fwrite_f(fl, :none, adj, 6, pad)
  end

  defp fwrite_f(fl, :none, _Adj, p, _Pad) when p >= 1 do
    float_f(fl, float_data(fl), p)
  end

  defp fwrite_f(fl, f, adj, :none, pad) do
    fwrite_f(fl, f, adj, 6, pad)
  end

  defp fwrite_f(fl, f, adj, p, pad) when p >= 1 do
    term(float_f(fl, float_data(fl), p), f, adj, f, pad)
  end

  defp float_f(fl, fd, p) when fl < 0.0 do
    [?- | float_f(-fl, fd, p)]
  end

  defp float_f(fl, {ds, e}, p) when e <= 0 do
    float_f(fl, {:lists.duplicate(-e + 1, ?0) ++ ds, 1}, p)
  end

  defp float_f(_Fl, {ds, e}, p) do
    case float_man(ds, e, p) do
      {fs, true} ->
        '1' ++ fs

      {fs, false} ->
        fs
    end
  end

  defp float_data(fl) do
    float_data(:erlang.float_to_list(fl), [])
  end

  defp float_data([?e | e], ds) do
    {:lists.reverse(ds), :erlang.list_to_integer(e) + 1}
  end

  defp float_data([d | cs], ds) when d >= ?0 and d <= ?9 do
    float_data(cs, [d | ds])
  end

  defp float_data([_ | cs], ds) do
    float_data(cs, ds)
  end

  def fwrite_g(0.0) do
    '0.0'
  end

  def fwrite_g(float) when is_float(float) do
    {frac, exp} = mantissa_exponent(float)
    {place, digits} = fwrite_g_1(float, exp, frac)

    r =
      insert_decimal(
        place,
        for d <- digits do
          ?0 + d
        end
      )

    for true <- [float < 0.0] do
      ?-
    end ++ r
  end

  defp mantissa_exponent(f) do
    case <<f::size(64)-float>> do
      <<_S::size(1), 0::size(11), m::size(52)>> ->
        e = log2floor(m)
        {m <<< (53 - e), e - 52 - 1075}

      <<_S::size(1), bE::size(11), m::size(52)>>
      when bE < 2047 ->
        {m + (1 <<< 52), bE - 1075}
    end
  end

  defp fwrite_g_1(float, exp, frac) do
    round = frac &&& 1 === 0

    cond do
      exp >= 0 ->
        bExp = 1 <<< exp

        cond do
          frac === 1 <<< 52 ->
            scale(frac * bExp * 4, 4, bExp * 2, bExp, round, round, float)

          true ->
            scale(frac * bExp * 2, 2, bExp, bExp, round, round, float)
        end

      exp < -1074 ->
        bExp = 1 <<< (-1074 - exp)
        scale(frac * 2, 1 <<< (1 - exp), bExp, bExp, round, round, float)

      exp > -1074 and frac === 1 <<< 52 ->
        scale(frac * 4, 1 <<< (2 - exp), 2, 1, round, round, float)

      true ->
        scale(frac * 2, 1 <<< (1 - exp), 1, 1, round, round, float)
    end
  end

  defp scale(r, s, mPlus, mMinus, lowOk, highOk, float) do
    est = int_ceil(:math.log10(abs(float)) - 1.0e-10)

    cond do
      est >= 0 ->
        fixup(r, s * int_pow(10, est), mPlus, mMinus, est, lowOk, highOk)

      true ->
        scale = int_pow(10, -est)
        fixup(r * scale, s, mPlus * scale, mMinus * scale, est, lowOk, highOk)
    end
  end

  defp fixup(r, s, mPlus, mMinus, k, lowOk, highOk) do
    tooLow =
      cond do
        highOk ->
          r + mPlus >= s

        true ->
          r + mPlus > s
      end

    case tooLow do
      true ->
        {k + 1, generate(r, s, mPlus, mMinus, lowOk, highOk)}

      false ->
        {k, generate(r * 10, s, mPlus * 10, mMinus * 10, lowOk, highOk)}
    end
  end

  defp generate(r0, s, mPlus, mMinus, lowOk, highOk) do
    d = div(r0, s)
    r = rem(r0, s)

    tC1 =
      cond do
        lowOk ->
          r <= mMinus

        true ->
          r < mMinus
      end

    tC2 =
      cond do
        highOk ->
          r + mPlus >= s

        true ->
          r + mPlus > s
      end

    case {tC1, tC2} do
      {false, false} ->
        [d | generate(r * 10, s, mPlus * 10, mMinus * 10, lowOk, highOk)]

      {false, true} ->
        [d + 1]

      {true, false} ->
        [d]

      {true, true} when r * 2 < s ->
        [d]

      {true, true} ->
        [d + 1]
    end
  end

  defp insert_decimal(0, s) do
    '0.' ++ s
  end

  defp insert_decimal(place, s) do
    l = length(s)

    cond do
      place < 0 or place >= l ->
        expL = :erlang.integer_to_list(place - 1)

        expDot =
          cond do
            l === 1 ->
              2

            true ->
              1
          end

        expCost = length(expL) + 1 + expDot

        cond do
          place < 0 ->
            cond do
              2 - place <= expCost ->
                '0.' ++ :lists.duplicate(-place, ?0) ++ s

              true ->
                insert_exp(expL, s)
            end

          true ->
            cond do
              place - l + 2 <= expCost ->
                s ++ :lists.duplicate(place - l, ?0) ++ '.0'

              true ->
                insert_exp(expL, s)
            end
        end

      true ->
        {s0, s1} = :lists.split(place, s)
        s0 ++ '.' ++ s1
    end
  end

  defp insert_exp(expL, [c]) do
    [c] ++ '.0e' ++ expL
  end

  defp insert_exp(expL, [c | s]) do
    [c] ++ '.' ++ s ++ 'e' ++ expL
  end

  defp int_ceil(x) when is_float(x) do
    t = trunc(x)

    case x - t do
      neg when neg < 0 ->
        t

      pos when pos > 0 ->
        t + 1

      _ ->
        t
    end
  end

  defp int_pow(x, 0) when is_integer(x) do
    1
  end

  defp int_pow(x, n)
       when is_integer(x) and is_integer(n) and
              n > 0 do
    int_pow(x, n, 1)
  end

  defp int_pow(x, n, r) when n < 2 do
    r * x
  end

  defp int_pow(x, n, r) do
    int_pow(
      x * x,
      n >>> 1,
      case n &&& 1 do
        1 ->
          r * x

        0 ->
          r
      end
    )
  end

  defp log2floor(int) when is_integer(int) and int > 0 do
    log2floor(int, 0)
  end

  defp log2floor(0, n) do
    n
  end

  defp log2floor(int, n) do
    log2floor(int >>> 1, 1 + n)
  end

  defp fwrite_g(fl, f, adj, :none, pad) do
    fwrite_g(fl, f, adj, 6, pad)
  end

  defp fwrite_g(fl, f, adj, p, pad) when p >= 1 do
    a = abs(fl)

    e =
      cond do
        a < 0.1 ->
          -2

        a < 1.0 ->
          -1

        a < 10.0 ->
          0

        a < 100.0 ->
          1

        a < 1.0e3 ->
          2

        a < 1.0e4 ->
          3

        true ->
          :fwrite_f
      end

    cond do
      (p <= 1 and e === -1) or (p - 1 > e and e >= -1) ->
        fwrite_f(fl, f, adj, p - 1 - e, pad)

      p <= 1 ->
        fwrite_e(fl, f, adj, 2, pad)

      true ->
        fwrite_e(fl, f, adj, p, pad)
    end
  end

  defp iolist_to_chars(cs, f, charsLimit)
       when charsLimit < 0 or
              charsLimit >= f do
    iolist_to_chars(cs)
  end

  defp iolist_to_chars(cs, _, charsLimit) do
    limit_iolist_to_chars(cs, sub(charsLimit, 3), [], :normal)
  end

  defp iolist_to_chars([c | cs])
       when is_integer(c) and c >= ?\0 and
              c <= 255 do
    [c | iolist_to_chars(cs)]
  end

  defp iolist_to_chars([i | cs]) do
    [iolist_to_chars(i) | iolist_to_chars(cs)]
  end

  defp iolist_to_chars([]) do
    []
  end

  defp iolist_to_chars(b) when is_binary(b) do
    :erlang.binary_to_list(b)
  end

  defp limit_iolist_to_chars(cs, 0, s, :normal) do
    l = limit_iolist_to_chars(cs, 4, s, :final)

    case :erlang.iolist_size(l) do
      n when n < 4 ->
        l

      4 ->
        '...'
    end
  end

  defp limit_iolist_to_chars(_Cs, 0, _S, :final) do
    []
  end

  defp limit_iolist_to_chars([c | cs], limit, s, mode)
       when c >= ?\0 and
              c <= 255 do
    [c | limit_iolist_to_chars(cs, limit - 1, s, mode)]
  end

  defp limit_iolist_to_chars([i | cs], limit, s, mode) do
    limit_iolist_to_chars(i, limit, [cs | s], mode)
  end

  defp limit_iolist_to_chars([], _Limit, [], _Mode) do
    []
  end

  defp limit_iolist_to_chars([], limit, [cs | s], mode) do
    limit_iolist_to_chars(cs, limit, s, mode)
  end

  defp limit_iolist_to_chars(b, limit, s, mode) when is_binary(b) do
    case byte_size(b) do
      sz when sz > limit ->
        {b1, b2} = :erlang.split_binary(b, limit)
        [:erlang.binary_to_list(b1) | limit_iolist_to_chars(b2, 0, s, mode)]

      sz ->
        [:erlang.binary_to_list(b) | limit_iolist_to_chars([], limit - sz, s, mode)]
    end
  end

  defp cdata_to_chars(cs, f, charsLimit)
       when charsLimit < 0 or
              charsLimit >= f do
    cdata_to_chars(cs)
  end

  defp cdata_to_chars(cs, _, charsLimit) do
    limit_cdata_to_chars(cs, sub(charsLimit, 3), :normal)
  end

  defp cdata_to_chars([c | cs]) when is_integer(c) and c >= ?\0 do
    [c | cdata_to_chars(cs)]
  end

  defp cdata_to_chars([i | cs]) do
    [cdata_to_chars(i) | cdata_to_chars(cs)]
  end

  defp cdata_to_chars([]) do
    []
  end

  defp cdata_to_chars(b) when is_binary(b) do
    case (try do
            :unicode.characters_to_list(b)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      l when is_list(l) ->
        l

      _ ->
        :erlang.binary_to_list(b)
    end
  end

  defp limit_cdata_to_chars(cs, 0, :normal) do
    l = limit_cdata_to_chars(cs, 4, :final)

    case :string.length(l) do
      n when n < 4 ->
        l

      4 ->
        '...'
    end
  end

  defp limit_cdata_to_chars(_Cs, 0, :final) do
    []
  end

  defp limit_cdata_to_chars(cs, limit, mode) do
    case :string.next_grapheme(cs) do
      {:error, <<c, cs1::binary>>} ->
        [c | limit_cdata_to_chars(cs1, limit - 1, mode)]

      {:error, [c | cs1]} ->
        [c | limit_cdata_to_chars(cs1, limit - 1, mode)]

      [] ->
        []

      [gC | cs1] ->
        [gC | limit_cdata_to_chars(cs1, limit - 1, mode)]
    end
  end

  defp limit_field(f, charsLimit)
       when charsLimit < 0 or
              f === :none do
    f
  end

  defp limit_field(f, charsLimit) do
    max(3, min(f, charsLimit))
  end

  defp string(s, :none, _Adj, :none, _Pad, _Enc) do
    s
  end

  defp string(s, f, adj, :none, pad, enc) do
    string_field(s, f, adj, :io_lib.chars_length(s), pad, enc)
  end

  defp string(s, :none, _Adj, p, pad, enc) do
    string_field(s, p, :left, :io_lib.chars_length(s), pad, enc)
  end

  defp string(s, f, adj, p, pad, enc) when f >= p do
    n = :io_lib.chars_length(s)

    cond do
      f > p ->
        cond do
          n > p ->
            adjust(flat_trunc(s, p, enc), chars(pad, f - p), adj)

          n < p ->
            adjust([s | chars(pad, p - n)], chars(pad, f - p), adj)

          true ->
            adjust(s, chars(pad, f - p), adj)
        end

      true ->
        string_field(s, f, adj, n, pad, enc)
    end
  end

  defp string_field(s, f, _Adj, n, _Pad, enc) when n > f do
    flat_trunc(s, f, enc)
  end

  defp string_field(s, f, adj, n, pad, _Enc) when n < f do
    adjust(s, chars(pad, f - n), adj)
  end

  defp string_field(s, _, _, _, _, _) do
    s
  end

  defp unprefixed_integer(int, f, adj, base, pad, lowercase)
       when base >= 2 and base <= 1 + ?Z - ?A + 10 do
    cond do
      int < 0 ->
        s =
          cond_lowercase(
            :erlang.integer_to_list(-int, base),
            lowercase
          )

        term([?- | s], f, adj, :none, pad)

      true ->
        s =
          cond_lowercase(
            :erlang.integer_to_list(int, base),
            lowercase
          )

        term(s, f, adj, :none, pad)
    end
  end

  defp prefixed_integer(int, f, adj, base, pad, prefix, lowercase)
       when base >= 2 and base <= 1 + ?Z - ?A + 10 do
    cond do
      int < 0 ->
        s =
          cond_lowercase(
            :erlang.integer_to_list(-int, base),
            lowercase
          )

        term([?-, prefix | s], f, adj, :none, pad)

      true ->
        s =
          cond_lowercase(
            :erlang.integer_to_list(int, base),
            lowercase
          )

        term([prefix | s], f, adj, :none, pad)
    end
  end

  defp char(c, :none, _Adj, :none, _Pad) do
    [c]
  end

  defp char(c, f, _Adj, :none, _Pad) do
    chars(c, f)
  end

  defp char(c, :none, _Adj, p, _Pad) do
    chars(c, p)
  end

  defp char(c, f, adj, p, pad) when f >= p do
    adjust(chars(c, p), chars(pad, f - p), adj)
  end

  defp newline(:none, _Adj, _P, _Pad) do
    '\n'
  end

  defp newline(f, :right, _P, _Pad) do
    chars(?\n, f)
  end

  defp adjust(data, [], _) do
    data
  end

  defp adjust(data, pad, :left) do
    [data | pad]
  end

  defp adjust(data, pad, :right) do
    [pad | data]
  end

  defp flat_trunc(list, n, :latin1)
       when is_integer(n) and
              n >= 0 do
    {s, _} = :lists.split(n, :lists.flatten(list))
    s
  end

  defp flat_trunc(list, n, :unicode)
       when is_integer(n) and
              n >= 0 do
    :string.slice(list, 0, n)
  end

  defp chars(_C, 0) do
    []
  end

  defp chars(c, 1) do
    [c]
  end

  defp chars(c, 2) do
    [c, c]
  end

  defp chars(c, 3) do
    [c, c, c]
  end

  defp chars(c, n) when is_integer(n) and n &&& 1 === 0 do
    s = chars(c, n >>> 1)
    [s | s]
  end

  defp chars(c, n) when is_integer(n) do
    s = chars(c, n >>> 1)
    [c, s | s]
  end

  defp cond_lowercase(string, true) do
    lowercase(string)
  end

  defp cond_lowercase(string, false) do
    string
  end

  defp lowercase([h | t])
       when is_integer(h) and h >= ?A and
              h <= ?Z do
    [h - ?A + ?a | lowercase(t)]
  end

  defp lowercase([h | t]) do
    [h | lowercase(t)]
  end

  defp lowercase([]) do
    []
  end

  defp sub(t, _) when t < 0 do
    t
  end

  defp sub(t, e) when t >= e do
    t - e
  end

  defp sub(_, _) do
    0
  end

  defp get_option(key, tupleList, default) do
    case :lists.keyfind(key, 1, tupleList) do
      false ->
        default

      {^key, value} ->
        value

      _ ->
        default
    end
  end
end
