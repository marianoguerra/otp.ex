defmodule :m_rand do
  use Bitwise

  defp uniform_range(range, %{:next => next, :bits => bits} = alg, r, v) do
    weakLowBits = :maps.get(:weak_low_bits, alg, 0)
    shift = bits - weakLowBits
    shiftMask = ~~~(1 <<< (weakLowBits - 1))
    rangeMinus1 = range - 1

    cond do
      range &&& rangeMinus1 === 0 ->
        {v1, r1, _} = uniform_range(range >>> bits, next, r, v, shiftMask, shift, bits)
        {v1 &&& rangeMinus1 + 1, {alg, r1}}

      true ->
        {v1, r1, b} = uniform_range(range >>> (bits - 2), next, r, v, shiftMask, shift, bits)
        i = rem(v1, range)

        cond do
          v1 - i <= 1 <<< (b - range) ->
            {i + 1, {alg, r1}}

          true ->
            {v2, r2} = next.(r1)
            uniform_range(range, alg, r2, v2)
        end
    end
  end

  defp uniform_range(range, next, r, v, shiftMask, shift, b) do
    cond do
      range <= 1 ->
        {v, r, b}

      true ->
        {v1, r1} = next.(r)

        uniform_range(
          range >>> shift,
          next,
          r1,
          (v &&& shiftMask <<< shift) ||| v1,
          shiftMask,
          shift,
          b + shift
        )
    end
  end

  def export_seed() do
    case :erlang.get(:rand_seed) do
      {%{:type => alg}, seed} ->
        {alg, seed}

      _ ->
        :undefined
    end
  end

  def export_seed_s({%{:type => alg}, algState}) do
    {alg, algState}
  end

  def seed(alg) do
    seed_put(seed_s(alg))
  end

  def seed_s({algHandler, _AlgState} = state)
      when is_map(algHandler) do
    state
  end

  def seed_s({alg, algState}) when is_atom(alg) do
    {algHandler, _SeedFun} = mk_alg(alg)
    {algHandler, algState}
  end

  def seed_s(alg) do
    seed_s(
      alg,
      {:erlang.phash2([{node(), self()}]), :erlang.system_time(), :erlang.unique_integer()}
    )
  end

  def seed(alg, seed) do
    seed_put(seed_s(alg, seed))
  end

  def seed_s(alg, seed) do
    {algHandler, seedFun} = mk_alg(alg)
    algState = seedFun.(seed)
    {algHandler, algState}
  end

  def uniform() do
    {x, state} = uniform_s(seed_get())
    _ = seed_put(state)
    x
  end

  def uniform(n) do
    {x, state} = uniform_s(n, seed_get())
    _ = seed_put(state)
    x
  end

  def uniform_s(state = {%{:uniform => uniform}, _}) do
    uniform.(state)
  end

  def uniform_s({%{:bits => bits, :next => next} = alg, r0}) do
    {v, r1} = next.(r0)
    {(v >>> (bits - 53)) * 1.1102230246251565e-16, {alg, r1}}
  end

  def uniform_s({%{:max => max, :next => next} = alg, r0}) do
    {v, r1} = next.(r0)
    {v / (max + 1), {alg, r1}}
  end

  def uniform_s(n, state = {%{:uniform_n => uniformN}, _})
      when is_integer(n) and 1 <= n do
    uniformN.(n, state)
  end

  def uniform_s(n, {%{:bits => bits, :next => next} = alg, r0})
      when is_integer(n) and 1 <= n do
    {v, r1} = next.(r0)
    maxMinusN = 1 <<< (bits - n)

    cond do
      0 <= maxMinusN ->
        cond do
          v < n ->
            {v + 1, {alg, r1}}

          true ->
            i = rem(v, n)

            cond do
              v - i <= maxMinusN ->
                {i + 1, {alg, r1}}

              true ->
                uniform_s(n, {alg, r1})
            end
        end

      true ->
        uniform_range(n, alg, r1, v)
    end
  end

  def uniform_s(n, {%{:max => max, :next => next} = alg, r0})
      when is_integer(n) and 1 <= n do
    {v, r1} = next.(r0)

    cond do
      n <= max ->
        {rem(v, n) + 1, {alg, r1}}

      true ->
        f = v / (max + 1)
        {trunc(f * n) + 1, {alg, r1}}
    end
  end

  def uniform_real() do
    {x, seed} = uniform_real_s(seed_get())
    _ = seed_put(seed)
    x
  end

  def uniform_real_s({%{:bits => bits, :next => next} = alg, r0}) do
    {v1, r1} = next.(r0)
    m1 = v1 >>> (bits - 56)

    cond do
      1 <<< 55 <= m1 ->
        {(m1 >>> 3) * :math.pow(2.0, -53), {alg, r1}}

      1 <<< 54 <= m1 ->
        {(m1 >>> 2) * :math.pow(2.0, -54), {alg, r1}}

      1 <<< 53 <= m1 ->
        {(m1 >>> 1) * :math.pow(2.0, -55), {alg, r1}}

      1 <<< 52 <= m1 ->
        {m1 * :math.pow(2.0, -56), {alg, r1}}

      true ->
        {v2, r2} = next.(r1)
        uniform_real_s(alg, next, m1, -56, r2, v2, bits)
    end
  end

  def uniform_real_s({%{:max => _, :next => next} = alg, r0}) do
    {v1, r1} = next.(r0)
    m1 = v1 &&& 1 <<< (56 - 1)

    cond do
      1 <<< 55 <= m1 ->
        {(m1 >>> 3) * :math.pow(2.0, -53), {alg, r1}}

      1 <<< 54 <= m1 ->
        {(m1 >>> 2) * :math.pow(2.0, -54), {alg, r1}}

      1 <<< 53 <= m1 ->
        {(m1 >>> 1) * :math.pow(2.0, -55), {alg, r1}}

      1 <<< 52 <= m1 ->
        {m1 * :math.pow(2.0, -56), {alg, r1}}

      true ->
        {v2, r2} = next.(r1)
        uniform_real_s(alg, next, m1, -56, r2, v2, 56)
    end
  end

  defp uniform_real_s(alg, _Next, m0, -1064, r1, v1, bits) do
    b0 = 53 - bc(m0, 1 <<< (52 - 1), 52)

    {(m0 <<< b0 ||| v1 >>> (bits - b0)) *
       :math.pow(
         2.0,
         -1064 - b0
       ), {alg, r1}}
  end

  defp uniform_real_s(alg, next, m0, bitNo, r1, v1, bits) do
    cond do
      1 <<< 51 <= m0 ->
        {(m0 <<< 1 ||| v1 >>> (bits - 1)) *
           :math.pow(
             2.0,
             bitNo - 1
           ), {alg, r1}}

      1 <<< 50 <= m0 ->
        {(m0 <<< 2 ||| v1 >>> (bits - 2)) *
           :math.pow(
             2.0,
             bitNo - 2
           ), {alg, r1}}

      1 <<< 49 <= m0 ->
        {(m0 <<< 3 ||| v1 >>> (bits - 3)) *
           :math.pow(
             2.0,
             bitNo - 3
           ), {alg, r1}}

      m0 == 0 ->
        m1 = v1 >>> (bits - 56)

        cond do
          1 <<< 55 <= m1 ->
            {(m1 >>> 3) * :math.pow(2.0, bitNo - 53), {alg, r1}}

          1 <<< 54 <= m1 ->
            {(m1 >>> 2) * :math.pow(2.0, bitNo - 54), {alg, r1}}

          1 <<< 53 <= m1 ->
            {(m1 >>> 1) * :math.pow(2.0, bitNo - 55), {alg, r1}}

          1 <<< 52 <= m1 ->
            {m1 * :math.pow(2.0, bitNo - 56), {alg, r1}}

          bitNo === -1008 ->
            cond do
              1 <<< 42 <= m1 ->
                uniform_real_s(alg, next, m1, bitNo - 56, r1)

              true ->
                uniform_real_s({alg, r1})
            end

          true ->
            uniform_real_s(alg, next, m1, bitNo - 56, r1)
        end

      true ->
        b0 = 53 - bc(m0, 1 <<< (49 - 1), 49)

        {(m0 <<< b0 ||| v1 >>> (bits - b0)) *
           :math.pow(
             2.0,
             bitNo - b0
           ), {alg, r1}}
    end
  end

  defp uniform_real_s(%{:bits => bits} = alg, next, m0, bitNo, r0) do
    {v1, r1} = next.(r0)
    uniform_real_s(alg, next, m0, bitNo, r1, v1, bits)
  end

  defp uniform_real_s(%{:max => _} = alg, next, m0, bitNo, r0) do
    {v1, r1} = next.(r0)
    uniform_real_s(alg, next, m0, bitNo, r1, v1 &&& 1 <<< (56 - 1), 56)
  end

  def jump(state = {%{:jump => jump}, _}) do
    jump.(state)
  end

  def jump({%{}, _}) do
    :erlang.error(:not_implemented)
  end

  def jump() do
    seed_put(jump(seed_get()))
  end

  def normal() do
    {x, seed} = normal_s(seed_get())
    _ = seed_put(seed)
    x
  end

  def normal(mean, variance) do
    mean + :math.sqrt(variance) * normal()
  end

  def normal_s(state0) do
    {sign, r, state} = get_52(state0)
    idx = r &&& 1 <<< (8 - 1)
    idx1 = idx + 1
    {ki, wi} = normal_kiwi(idx1)
    x = r * wi

    case r < ki do
      true when sign === 0 ->
        {x, state}

      true ->
        {-x, state}

      false when sign === 0 ->
        normal_s(idx, sign, x, state)

      false ->
        normal_s(idx, sign, -x, state)
    end
  end

  def normal_s(mean, variance, state0) when variance > 0 do
    {x, state} = normal_s(state0)
    {mean + :math.sqrt(variance) * x, state}
  end

  defp seed_put(seed) do
    :erlang.put(:rand_seed, seed)
    seed
  end

  defp seed_get() do
    case :erlang.get(:rand_seed) do
      :undefined ->
        seed(:exsss)

      old ->
        old
    end
  end

  defp mk_alg(:exs64) do
    {%{:type => :exs64, :max => 1 <<< (64 - 1), :next => &exs64_next/1}, &exs64_seed/1}
  end

  defp mk_alg(:exsplus) do
    {%{
       :type => :exsplus,
       :max => 1 <<< (58 - 1),
       :next => &exsplus_next/1,
       :jump => &exsplus_jump/1
     }, &exsplus_seed/1}
  end

  defp mk_alg(:exsp) do
    {%{
       :type => :exsp,
       :bits => 58,
       :weak_low_bits => 1,
       :next => &exsplus_next/1,
       :uniform => &exsp_uniform/1,
       :uniform_n => &exsp_uniform/2,
       :jump => &exsplus_jump/1
     }, &exsplus_seed/1}
  end

  defp mk_alg(:exsss) do
    {%{
       :type => :exsss,
       :bits => 58,
       :next => &exsss_next/1,
       :uniform => &exsss_uniform/1,
       :uniform_n => &exsss_uniform/2,
       :jump => &exsplus_jump/1
     }, &exsss_seed/1}
  end

  defp mk_alg(:exs1024) do
    {%{
       :type => :exs1024,
       :max => 1 <<< (64 - 1),
       :next => &exs1024_next/1,
       :jump => &exs1024_jump/1
     }, &exs1024_seed/1}
  end

  defp mk_alg(:exs1024s) do
    {%{
       :type => :exs1024s,
       :bits => 64,
       :weak_low_bits => 3,
       :next => &exs1024_next/1,
       :jump => &exs1024_jump/1
     }, &exs1024_seed/1}
  end

  defp mk_alg(:exrop) do
    {%{
       :type => :exrop,
       :bits => 58,
       :weak_low_bits => 1,
       :next => &exrop_next/1,
       :uniform => &exrop_uniform/1,
       :uniform_n => &exrop_uniform/2,
       :jump => &exrop_jump/1
     }, &exrop_seed/1}
  end

  defp mk_alg(:exro928ss) do
    {%{
       :type => :exro928ss,
       :bits => 58,
       :next => &exro928ss_next/1,
       :uniform => &exro928ss_uniform/1,
       :uniform_n => &exro928ss_uniform/2,
       :jump => &exro928_jump/1
     }, &exro928_seed/1}
  end

  defp exs64_seed(l) when is_list(l) do
    [r] = seed64_nz(1, l)
    r
  end

  defp exs64_seed(a) when is_integer(a) do
    [r] = seed64(1, a &&& 1 <<< (64 - 1))
    r
  end

  defp exs64_seed({a1, a2, a3}) do
    {v1, _} = exs64_next(a1 &&& (1 <<< (32 - 1)) * 4_294_967_197 + 1)
    {v2, _} = exs64_next(a2 &&& (1 <<< (32 - 1)) * 4_294_967_231 + 1)
    {v3, _} = exs64_next(a3 &&& (1 <<< (32 - 1)) * 4_294_967_279 + 1)
    rem(v1 * v2 * v3, 1 <<< (64 - 1 - 1)) + 1
  end

  defp exs64_next(r) do
    r1 = r ^^^ (r >>> 12)
    r2 = r1 ^^^ (r1 &&& 1 <<< (64 - 25 - 1) <<< 25)
    r3 = r2 ^^^ (r2 >>> 27)
    {r3 * 2_685_821_657_736_338_717 &&& 1 <<< (64 - 1), r3}
  end

  defp exsplus_seed(l) when is_list(l) do
    [s0, s1] = seed58_nz(2, l)
    [s0 | s1]
  end

  defp exsplus_seed(x) when is_integer(x) do
    [s0, s1] = seed58(2, x &&& 1 <<< (64 - 1))
    [s0 | s1]
  end

  defp exsplus_seed({a1, a2, a3}) do
    {_, r1} =
      exsplus_next([
        a1 * 4_294_967_197 + 1 &&& 1 <<< (58 - 1)
        | a2 * 4_294_967_231 + 1 &&& 1 <<< (58 - 1)
      ])

    {_, r2} =
      exsplus_next([
        a3 * 4_294_967_279 + 1 &&& 1 <<< (58 - 1)
        | tl(r1)
      ])

    r2
  end

  defp exsss_seed(l) when is_list(l) do
    [s0, s1] = seed58_nz(2, l)
    [s0 | s1]
  end

  defp exsss_seed(x) when is_integer(x) do
    [s0, s1] = seed58(2, x &&& 1 <<< (64 - 1))
    [s0 | s1]
  end

  defp exsss_seed({a1, a2, a3}) do
    {_, x0} = seed58(a1 &&& 1 <<< (64 - 1))
    {s0, x1} = seed58(a2 &&& (1 <<< (64 - 1)) ^^^ x0)
    {s1, _} = seed58(a3 &&& (1 <<< (64 - 1)) ^^^ x1)
    [s0 | s1]
  end

  defp exsplus_next([s1 | s0]) do
    newS1 =
      (
        s1_1 = s1 ^^^ (s1 &&& 1 <<< (58 - 24 - 1) <<< 24)
        s1_1 ^^^ s0 ^^^ (s1_1 >>> 11) ^^^ (s0 >>> 41)
      )

    {s0 + newS1 &&& 1 <<< (58 - 1), [s0 | newS1]}
  end

  defp exsss_next([s1 | s0]) do
    newS1 =
      (
        s1_1 = s1 ^^^ (s1 &&& 1 <<< (58 - 24 - 1) <<< 24)
        s1_1 ^^^ s0 ^^^ (s1_1 >>> 11) ^^^ (s0 >>> 41)
      )

    {(
       v_0 = s0 + (s0 &&& 1 <<< (58 - 2 - 1) <<< 2) &&& 1 <<< (58 - 1)
       v_1 = (v_0 &&& 1 <<< (58 - 7 - 1) <<< 7) ||| v_0 >>> (58 - 7)
       v_1 + (v_1 &&& 1 <<< (58 - 3 - 1) <<< 3) &&& 1 <<< (58 - 1)
     ), [s0 | newS1]}
  end

  defp exsp_uniform({alg, r0}) do
    {i, r1} = exsplus_next(r0)
    {(i >>> (58 - 53)) * 1.1102230246251565e-16, {alg, r1}}
  end

  defp exsss_uniform({alg, r0}) do
    {i, r1} = exsss_next(r0)
    {(i >>> (58 - 53)) * 1.1102230246251565e-16, {alg, r1}}
  end

  defp exsp_uniform(range, {alg, r}) do
    {v, r1} = exsplus_next(r)
    maxMinusRange = 1 <<< (58 - range)

    cond do
      0 <= maxMinusRange ->
        cond do
          v < range ->
            {v + 1, {alg, r1}}

          true ->
            i = rem(v, range)

            cond do
              v - i <= maxMinusRange ->
                {i + 1, {alg, r1}}

              true ->
                exsp_uniform(range, {alg, r1})
            end
        end

      true ->
        uniform_range(range, alg, r1, v)
    end
  end

  defp exsss_uniform(range, {alg, r}) do
    {v, r1} = exsss_next(r)
    maxMinusRange = 1 <<< (58 - range)

    cond do
      0 <= maxMinusRange ->
        cond do
          v < range ->
            {v + 1, {alg, r1}}

          true ->
            i = rem(v, range)

            cond do
              v - i <= maxMinusRange ->
                {i + 1, {alg, r1}}

              true ->
                exsss_uniform(range, {alg, r1})
            end
        end

      true ->
        uniform_range(range, alg, r1, v)
    end
  end

  defp exsplus_jump({alg, s}) do
    {s1, aS1} = exsplus_jump(s, [0 | 0], 13_386_170_678_560_663, 58)
    {_, aS2} = exsplus_jump(s1, aS1, 235_826_144_310_425_740, 58)
    {alg, aS2}
  end

  defp exsplus_jump(s, aS, _, 0) do
    {s, aS}
  end

  defp exsplus_jump(s, [aS0 | aS1], j, n) do
    {_, nS} = exsplus_next(s)

    case j &&& 1 <<< (1 - 1) do
      1 ->
        [s0 | s1] = s
        exsplus_jump(nS, [aS0 ^^^ s0 | aS1 ^^^ s1], j >>> 1, n - 1)

      0 ->
        exsplus_jump(nS, [aS0 | aS1], j >>> 1, n - 1)
    end
  end

  defp exs1024_seed(l) when is_list(l) do
    {seed64_nz(16, l), []}
  end

  defp exs1024_seed(x) when is_integer(x) do
    {seed64(16, x &&& 1 <<< (64 - 1)), []}
  end

  defp exs1024_seed({a1, a2, a3}) do
    b1 = (a1 &&& (1 <<< (21 - 1)) + 1) * 2_097_131 &&& 1 <<< (21 - 1)
    b2 = (a2 &&& (1 <<< (21 - 1)) + 1) * 2_097_133 &&& 1 <<< (21 - 1)
    b3 = (a3 &&& (1 <<< (21 - 1)) + 1) * 2_097_143 &&& 1 <<< (21 - 1)
    {exs1024_gen1024(b1 <<< 43 ||| b2 <<< 22 ||| b3 <<< 1 ||| 1), []}
  end

  defp exs1024_gen1024(r) do
    exs1024_gen1024(16, r, [])
  end

  defp exs1024_gen1024(0, _, l) do
    l
  end

  defp exs1024_gen1024(n, r, l) do
    {x, r2} = exs64_next(r)
    exs1024_gen1024(n - 1, r2, [x | l])
  end

  defp exs1024_calc(s0, s1) do
    s11 = s1 ^^^ (s1 &&& 1 <<< (64 - 31 - 1) <<< 31)
    s12 = s11 ^^^ (s11 >>> 11)
    s01 = s0 ^^^ (s0 >>> 30)
    nS1 = s01 ^^^ s12
    {nS1 * 1_181_783_497_276_652_981 &&& 1 <<< (64 - 1), nS1}
  end

  defp exs1024_next({[[s0, s1] | l3], rL}) do
    {x, nS1} = exs1024_calc(s0, s1)
    {x, {[nS1 | l3], [s0 | rL]}}
  end

  defp exs1024_next({[h], rL}) do
    nL = [h | :lists.reverse(rL)]
    exs1024_next({nL, []})
  end

  defp exs1024_jump({alg, {l, rL}}) do
    p = length(rL)

    aS =
      exs1024_jump(
        {l, rL},
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        [
          114_527_183_042_123_105,
          160_423_628_620_659_260,
          284_733_707_589_872_850,
          164_435_740_288_387_503,
          259_572_741_793_888_962,
          215_793_509_705_812_255,
          228_241_955_430_903_492,
          221_708_554_683_218_499,
          212_006_596_549_813_798,
          139_215_019_150_089_363,
          23_964_000_621_384_961,
          55_201_052_708_218_217,
          112_969_240_468_397_636,
          22_130_735_059_088_892,
          244_278_597_799_509_466,
          220_175_845_070_832_114,
          43_243_288_828
        ],
        10_185_424_423_732_253,
        58,
        1024
      )

    {aSL, aSR} = :lists.split(16 - p, aS)
    {alg, {aSL, :lists.reverse(aSR)}}
  end

  defp exs1024_jump(_, aS, _, _, _, 0) do
    aS
  end

  defp exs1024_jump(s, aS, [h | t], _, 0, tN) do
    exs1024_jump(s, aS, t, h, 58, tN)
  end

  defp exs1024_jump({l, rL}, aS, jL, j, n, tN) do
    {_, nS} = exs1024_next({l, rL})

    case j &&& 1 <<< (1 - 1) do
      1 ->
        aS2 =
          :lists.zipwith(
            fn x, y ->
              x ^^^ y
            end,
            aS,
            l ++ :lists.reverse(rL)
          )

        exs1024_jump(nS, aS2, jL, j >>> 1, n - 1, tN - 1)

      0 ->
        exs1024_jump(nS, aS, jL, j >>> 1, n - 1, tN - 1)
    end
  end

  def exro928_seed(l) when is_list(l) do
    {seed58_nz(16, l), []}
  end

  def exro928_seed(x) when is_integer(x) do
    {seed58(16, x &&& 1 <<< (64 - 1)), []}
  end

  def exro928_seed({a1, a2, a3}) do
    {s0, x0} = seed58(a1 &&& 1 <<< (64 - 1))
    {s1, x1} = seed58(a2 &&& (1 <<< (64 - 1)) ^^^ x0)
    {s2, x2} = seed58(a3 &&& (1 <<< (64 - 1)) ^^^ x1)
    {[[s0, s1, s2] | seed58(13, x2)], []}
  end

  defp exro928ss_next({[[s15, s0] | ss], rs}) do
    sR = exro928_next_state(ss, rs, s15, s0)

    {(
       v_0 = s0 + (s0 &&& 1 <<< (58 - 2 - 1) <<< 2) &&& 1 <<< (58 - 1)
       v_1 = (v_0 &&& 1 <<< (58 - 7 - 1) <<< 7) ||| v_0 >>> (58 - 7)
       v_1 + (v_1 &&& 1 <<< (58 - 3 - 1) <<< 3) &&& 1 <<< (58 - 1)
     ), sR}
  end

  defp exro928ss_next({[s15], rs}) do
    exro928ss_next({[s15 | :lists.reverse(rs)], []})
  end

  def exro928_next({[[s15, s0] | ss], rs}) do
    sR = exro928_next_state(ss, rs, s15, s0)
    {{s15, s0}, sR}
  end

  def exro928_next({[s15], rs}) do
    exro928_next({[s15 | :lists.reverse(rs)], []})
  end

  def exro928_next_state({[[s15, s0] | ss], rs}) do
    exro928_next_state(ss, rs, s15, s0)
  end

  def exro928_next_state({[s15], rs}) do
    [s0 | ss] = :lists.reverse(rs)
    exro928_next_state(ss, [], s15, s0)
  end

  defp exro928_next_state(ss, rs, s15, s0) do
    q = s15 ^^^ s0

    newS15 =
      (s0 &&& 1 <<< (58 - 44 - 1) <<< 44) |||
        (s0 >>> (58 - 44)) ^^^ q ^^^ (q &&& 1 <<< (58 - 9 - 1) <<< 9)

    newS0 = (q &&& 1 <<< (58 - 45 - 1) <<< 45) ||| q >>> (58 - 45)
    {[newS0 | ss], [newS15 | rs]}
  end

  defp exro928ss_uniform({alg, sR}) do
    {v, newSR} = exro928ss_next(sR)
    {(v >>> (58 - 53)) * 1.1102230246251565e-16, {alg, newSR}}
  end

  defp exro928ss_uniform(range, {alg, sR}) do
    {v, newSR} = exro928ss_next(sR)
    maxMinusRange = 1 <<< (58 - range)

    cond do
      0 <= maxMinusRange ->
        cond do
          v < range ->
            {v + 1, {alg, newSR}}

          true ->
            i = rem(v, range)

            cond do
              v - i <= maxMinusRange ->
                {i + 1, {alg, newSR}}

              true ->
                exro928ss_uniform(range, {alg, newSR})
            end
        end

      true ->
        uniform_range(range, alg, newSR, v)
    end
  end

  defp exro928_jump({alg, sR}) do
    {alg, exro928_jump_2pow512(sR)}
  end

  def exro928_jump_2pow512(sR) do
    polyjump(sR, &exro928_next_state/1, [
      290_573_448_171_827_402,
      382_251_779_910_418_577,
      423_857_156_240_780_192,
      317_638_803_078_791_815,
      312_577_798_172_065_765,
      305_801_842_905_235_492,
      450_887_821_400_921_554,
      490_154_825_290_594_607,
      507_224_882_549_817_556,
      305_131_922_350_994_371,
      524_004_876_356_613_068,
      399_286_492_428_034_246,
      556_129_459_533_271_918,
      302_163_523_288_674_092,
      295_571_835_370_094_372,
      487_547_435_355_635_071
    ])
  end

  def exro928_jump_2pow20(sR) do
    polyjump(sR, &exro928_next_state/1, [
      412_473_694_820_566_502,
      432_883_605_991_317_039,
      525_373_508_288_112_196,
      403_915_169_708_599_875,
      319_067_783_491_633_768,
      301_226_760_020_322_060,
      311_627_678_308_842_608,
      376_040_681_981_803_602,
      339_701_046_172_540_810,
      406_476_937_554_306_621,
      319_178_240_279_900_411,
      538_961_455_727_032_748,
      343_829_982_822_907_227,
      562_090_186_051_299_616,
      294_421_712_295_949_406,
      517_056_752_316_592_047
    ])
  end

  defp exrop_seed(l) when is_list(l) do
    [s0, s1] = seed58_nz(2, l)
    [s0 | s1]
  end

  defp exrop_seed(x) when is_integer(x) do
    [s0, s1] = seed58(2, x &&& 1 <<< (64 - 1))
    [s0 | s1]
  end

  defp exrop_seed({a1, a2, a3}) do
    [
      _
      | s1
    ] =
      exrop_next_s(
        a1 * 4_294_967_197 + 1 &&& 1 <<< (58 - 1),
        a2 * 4_294_967_231 + 1 &&& 1 <<< (58 - 1)
      )

    exrop_next_s(
      a3 * 4_294_967_279 + 1 &&& 1 <<< (58 - 1),
      s1
    )
  end

  defp exrop_next_s(s0, s1) do
    s1_a = s1 ^^^ s0

    [
      (s0 &&& 1 <<< (58 - 24 - 1) <<< 24) |||
        (s0 >>> (58 - 24)) ^^^ s1_a ^^^ (s1_a &&& 1 <<< (58 - 2 - 1) <<< 2)
      | (s1_a &&& 1 <<< (58 - 35 - 1) <<< 35) ||| s1_a >>> (58 - 35)
    ]
  end

  defp exrop_next([s0 | s1]) do
    {s0 + s1 &&& 1 <<< (58 - 1),
     (
       s1_a = s1 ^^^ s0

       [
         (s0 &&& 1 <<< (58 - 24 - 1) <<< 24) |||
           (s0 >>> (58 - 24)) ^^^ s1_a ^^^ (s1_a &&& 1 <<< (58 - 2 - 1) <<< 2)
         | (s1_a &&& 1 <<< (58 - 35 - 1) <<< 35) ||| s1_a >>> (58 - 35)
       ]
     )}
  end

  defp exrop_uniform({alg, r}) do
    {v, r1} = exrop_next(r)
    {(v >>> (58 - 53)) * 1.1102230246251565e-16, {alg, r1}}
  end

  defp exrop_uniform(range, {alg, r}) do
    {v, r1} = exrop_next(r)
    maxMinusRange = 1 <<< (58 - range)

    cond do
      0 <= maxMinusRange ->
        cond do
          v < range ->
            {v + 1, {alg, r1}}

          true ->
            i = rem(v, range)

            cond do
              v - i <= maxMinusRange ->
                {i + 1, {alg, r1}}

              true ->
                exrop_uniform(range, {alg, r1})
            end
        end

      true ->
        uniform_range(range, alg, r1, v)
    end
  end

  defp exrop_jump({alg, s}) do
    [
      j
      | js
    ] = [
      1 <<< 58 ||| (49_452_476_321_943_384_982_939_338_509_431_082 &&& 1 <<< (58 - 1)),
      49_452_476_321_943_384_982_939_338_509_431_082 >>> 58
    ]

    {alg, exrop_jump(s, 0, 0, j, js)}
  end

  defp exrop_jump(_S, s0, s1, 0, []) do
    [s0 | s1]
  end

  defp exrop_jump(s, s0, s1, 1, [j | js]) do
    exrop_jump(s, s0, s1, j, js)
  end

  defp exrop_jump([s__0 | s__1] = _S, s0, s1, j, js) do
    case j &&& 1 <<< (1 - 1) do
      1 ->
        newS = exrop_next_s(s__0, s__1)
        exrop_jump(newS, s0 ^^^ s__0, s1 ^^^ s__1, j >>> 1, js)

      0 ->
        newS = exrop_next_s(s__0, s__1)
        exrop_jump(newS, s0, s1, j >>> 1, js)
    end
  end

  defp seed58_nz(n, ss) do
    seed_nz(n, ss, 58, false)
  end

  defp seed64_nz(n, ss) do
    seed_nz(n, ss, 64, false)
  end

  defp seed_nz(_N, [], _M, false) do
    :erlang.error(:zero_seed)
  end

  defp seed_nz(0, [_ | _], _M, _NZ) do
    :erlang.error(:too_many_seed_integers)
  end

  defp seed_nz(0, [], _M, _NZ) do
    []
  end

  defp seed_nz(n, [], m, true) do
    [0 | seed_nz(n - 1, [], m, true)]
  end

  defp seed_nz(n, [s | ss], m, nZ) do
    cond do
      is_integer(s) ->
        r = s &&& 1 <<< (m - 1)
        [r | seed_nz(n - 1, ss, m, nZ or r !== 0)]

      true ->
        :erlang.error(:non_integer_seed)
    end
  end

  def seed58(0, _X) do
    []
  end

  def seed58(n, x) do
    {z, newX} = seed58(x)
    [z | seed58(n - 1, newX)]
  end

  defp seed58(x_0) do
    {z0, x} = splitmix64_next(x_0)

    case z0 &&& 1 <<< (58 - 1) do
      0 ->
        seed58(x)

      z ->
        {z, x}
    end
  end

  defp seed64(0, _X) do
    []
  end

  defp seed64(n, x) do
    {z, newX} = seed64(x)
    [z | seed64(n - 1, newX)]
  end

  defp seed64(x_0) do
    {z, x} = zX = splitmix64_next(x_0)

    cond do
      z === 0 ->
        seed64(x)

      true ->
        zX
    end
  end

  defp splitmix64_next(x_0) do
    x = x_0 + 11_400_714_819_323_198_485 &&& 1 <<< (64 - 1)
    z_0 = (x ^^^ (x >>> 30)) * 13_787_848_793_156_543_929 &&& 1 <<< (64 - 1)
    z_1 = (z_0 ^^^ (z_0 >>> 27)) * 10_723_151_780_598_845_931 &&& 1 <<< (64 - 1)
    {z_1 ^^^ (z_1 >>> 31) &&& 1 <<< (64 - 1), x}
  end

  defp polyjump({ss, rs} = sR, nextState, jumpConst) do
    ts = :lists.duplicate(length(ss) + length(rs), 0)
    polyjump(sR, nextState, jumpConst, ts)
  end

  defp polyjump(_SR, _NextState, [], ts) do
    {ts, []}
  end

  defp polyjump(sR, nextState, [j | js], ts) do
    polyjump(sR, nextState, js, ts, j)
  end

  defp polyjump(sR, nextState, js, ts, 1) do
    polyjump(sR, nextState, js, ts)
  end

  defp polyjump({ss, rs} = sR, nextState, js, ts, j)
       when j !== 0 do
    newSR = nextState.(sR)
    newJ = j >>> 1

    case j &&& 1 <<< (1 - 1) do
      0 ->
        polyjump(newSR, nextState, js, ts, newJ)

      1 ->
        polyjump(newSR, nextState, js, xorzip_sr(ts, ss, rs), newJ)
    end
  end

  defp xorzip_sr([], [], :undefined) do
    []
  end

  defp xorzip_sr(ts, [], rs) do
    xorzip_sr(ts, :lists.reverse(rs), :undefined)
  end

  defp xorzip_sr([t | ts], [s | ss], rs) do
    [t ^^^ s | xorzip_sr(ts, ss, rs)]
  end

  def format_jumpconst58(string) do
    reOpts = [{:newline, :any}, {:capture, :all_but_first, :binary}, :global]
    {:match, matches} = :re.run(string, '0x([a-zA-Z0-9]+)', reOpts)
    format_jumcons58_matches(:lists.reverse(matches), 0)
  end

  defp format_jumcons58_matches([], j) do
    format_jumpconst58_value(j)
  end

  defp format_jumcons58_matches([[bin] | matches], j) do
    newJ = j <<< 64 ||| :erlang.binary_to_integer(bin, 16)
    format_jumcons58_matches(matches, newJ)
  end

  defp format_jumpconst58_value(0) do
    :ok
  end

  defp format_jumpconst58_value(j) do
    :io.format(
      '16#~s,~n',
      [
        :erlang.integer_to_list(
          (j &&& 1 <<< (58 - 1)) ||| 1 <<< 58,
          16
        )
      ]
    )

    format_jumpconst58_value(j >>> 58)
  end

  defp get_52({alg = %{:bits => bits, :next => next}, s0}) do
    {int, s1} = next.(s0)
    {1 <<< (bits - 51 - 1) &&& int, int >>> (bits - 51), {alg, s1}}
  end

  defp get_52({alg = %{:next => next}, s0}) do
    {int, s1} = next.(s0)
    {1 <<< 51 &&& int, int &&& 1 <<< (51 - 1), {alg, s1}}
  end

  defp normal_s(0, sign, x0, state0) do
    {u0, s1} = uniform_s(state0)
    x = -1 / 3.654152885361009 * :math.log(u0)
    {u1, s2} = uniform_s(s1)
    y = -:math.log(u1)

    case y + y > x * x do
      false ->
        normal_s(0, sign, x0, s2)

      true when sign === 0 ->
        {3.654152885361009 + x, s2}

      true ->
        {-3.654152885361009 - x, s2}
    end
  end

  defp normal_s(idx, _Sign, x, state0) do
    fi2 = normal_fi(idx + 1)
    {u0, s1} = uniform_s(state0)

    case (normal_fi(idx) - fi2) * u0 + fi2 < :math.exp(-0.5 * x * x) do
      true ->
        {x, s1}

      false ->
        normal_s(s1)
    end
  end

  defp normal_kiwi(indx) do
    :erlang.element(
      indx,
      {{2_104_047_571_236_786, 1.736725412160263e-15}, {0, 9.558660351455634e-17},
       {1_693_657_211_986_787, 1.2708704834810623e-16},
       {1_919_380_038_271_141, 1.4909740962495474e-16},
       {2_015_384_402_196_343, 1.6658733631586268e-16},
       {2_068_365_869_448_128, 1.8136120810119029e-16},
       {2_101_878_624_052_573, 1.9429720153135588e-16},
       {2_124_958_784_102_998, 2.0589500628482093e-16},
       {2_141_808_670_795_147, 2.1646860576895422e-16},
       {2_154_644_611_568_301, 2.2622940392218116e-16},
       {2_164_744_887_587_275, 2.353271891404589e-16},
       {2_172_897_953_696_594, 2.438723455742877e-16},
       {2_179_616_279_372_365, 2.5194879829274225e-16},
       {2_185_247_251_868_649, 2.5962199772528103e-16},
       {2_190_034_623_107_822, 2.6694407473648285e-16},
       {2_194_154_434_521_197, 2.7395729685142446e-16},
       {2_197_736_978_774_660, 2.8069646002484804e-16},
       {2_200_880_740_891_961, 2.871905890411393e-16},
       {2_203_661_538_010_620, 2.9346417484728883e-16},
       {2_206_138_681_109_102, 2.9953809336782113e-16},
       {2_208_359_231_806_599, 3.054303000719244e-16},
       {2_210_361_007_258_210, 3.111563633892157e-16},
       {2_212_174_742_388_539, 3.1672988018581815e-16},
       {2_213_825_672_704_646, 3.2216280350549905e-16},
       {2_215_334_711_002_614, 3.274657040793975e-16},
       {2_216_719_334_487_595, 3.326479811684171e-16},
       {2_217_994_262_139_172, 3.377180341735323e-16},
       {2_219_171_977_965_032, 3.4268340353119356e-16},
       {2_220_263_139_538_712, 3.475508873172976e-16},
       {2_221_276_900_117_330, 3.523266384600203e-16},
       {2_222_221_164_932_930, 3.5701624633953494e-16},
       {2_223_102_796_829_069, 3.616248057159834e-16},
       {2_223_927_782_546_658, 3.661569752965354e-16},
       {2_224_701_368_170_060, 3.7061702777236077e-16},
       {2_225_428_170_204_312, 3.75008892787478e-16},
       {2_226_112_267_248_242, 3.7933619401549554e-16},
       {2_226_757_276_105_256, 3.836022812967728e-16},
       {2_227_366_415_328_399, 3.8781025861250247e-16},
       {2_227_942_558_554_684, 3.919630085325768e-16},
       {2_228_488_279_492_521, 3.9606321366256378e-16},
       {2_229_005_890_047_222, 4.001133755254669e-16},
       {2_229_497_472_775_193, 4.041158312414333e-16},
       {2_229_964_908_627_060, 4.080727683096045e-16},
       {2_230_409_900_758_597, 4.119862377480744e-16},
       {2_230_833_995_044_585, 4.1585816580828064e-16},
       {2_231_238_597_816_133, 4.1969036444740733e-16},
       {2_231_624_991_250_191, 4.234845407152071e-16},
       {2_231_994_346_765_928, 4.272423051889976e-16},
       {2_232_347_736_722_750, 4.309651795716294e-16},
       {2_232_686_144_665_934, 4.346546035512876e-16},
       {2_233_010_474_325_959, 4.383119410085457e-16},
       {2_233_321_557_544_881, 4.4193848564470665e-16},
       {2_233_620_161_276_071, 4.455354660957914e-16},
       {2_233_906_993_781_271, 4.491040505882875e-16},
       {2_234_182_710_130_335, 4.52645351185714e-16},
       {2_234_447_917_093_496, 4.561604276690038e-16},
       {2_234_703_177_503_020, 4.596502910884941e-16},
       {2_234_949_014_150_181, 4.631159070208165e-16},
       {2_235_185_913_274_316, 4.665581985600875e-16},
       {2_235_414_327_692_884, 4.699780490694195e-16},
       {2_235_634_679_614_920, 4.733763047158324e-16},
       {2_235_847_363_174_595, 4.767537768090853e-16},
       {2_236_052_746_716_837, 4.8011124396270155e-16},
       {2_236_251_174_862_869, 4.834494540935008e-16},
       {2_236_442_970_379_967, 4.867691262742209e-16},
       {2_236_628_435_876_762, 4.900709524522994e-16},
       {2_236_807_855_342_765, 4.933555990465414e-16},
       {2_236_981_495_548_562, 4.966237084322178e-16},
       {2_237_149_607_321_147, 4.998759003240909e-16},
       {2_237_312_426_707_209, 5.031127730659319e-16},
       {2_237_470_176_035_652, 5.0633490483427195e-16},
       {2_237_623_064_889_403, 5.095428547633892e-16},
       {2_237_771_290_995_388, 5.127371639978797e-16},
       {2_237_915_041_040_597, 5.159183566785736e-16},
       {2_238_054_491_421_305, 5.190869408670343e-16},
       {2_238_189_808_931_712, 5.222434094134042e-16},
       {2_238_321_151_397_660, 5.253882407719454e-16},
       {2_238_448_668_260_432, 5.285218997682382e-16},
       {2_238_572_501_115_169, 5.316448383216618e-16},
       {2_238_692_784_207_942, 5.34757496126473e-16},
       {2_238_809_644_895_133, 5.378603012945235e-16},
       {2_238_923_204_068_402, 5.409536709623993e-16},
       {2_239_033_576_548_190, 5.440380118655467e-16},
       {2_239_140_871_448_443, 5.471137208817361e-16},
       {2_239_245_192_514_958, 5.501811855460336e-16},
       {2_239_346_638_439_541, 5.532407845392784e-16},
       {2_239_445_303_151_952, 5.56292888151909e-16},
       {2_239_541_276_091_442, 5.593378587248462e-16},
       {2_239_634_642_459_498, 5.623760510690043e-16},
       {2_239_725_483_455_293, 5.65407812864896e-16},
       {2_239_813_876_495_186, 5.684334850436814e-16},
       {2_239_899_895_417_494, 5.714534021509204e-16},
       {2_239_983_610_673_676, 5.744678926941961e-16},
       {2_240_065_089_506_935, 5.774772794756965e-16},
       {2_240_144_396_119_183, 5.804818799107686e-16},
       {2_240_221_591_827_230, 5.834820063333892e-16},
       {2_240_296_735_208_969, 5.864779662894365e-16},
       {2_240_369_882_240_293, 5.894700628185872e-16},
       {2_240_441_086_423_386, 5.924585947256134e-16},
       {2_240_510_398_907_004, 5.95443856841806e-16},
       {2_240_577_868_599_305, 5.984261402772028e-16},
       {2_240_643_542_273_726, 6.014057326642664e-16},
       {2_240_707_464_668_391, 6.043829183936125e-16},
       {2_240_769_678_579_486, 6.073579788423606e-16},
       {2_240_830_224_948_980, 6.103311925956439e-16},
       {2_240_889_142_947_082, 6.133028356617911e-16},
       {2_240_946_470_049_769, 6.162731816816596e-16},
       {2_241_002_242_111_691, 6.192425021325847e-16},
       {2_241_056_493_434_746, 6.222110665273788e-16},
       {2_241_109_256_832_602, 6.251791426088e-16},
       {2_241_160_563_691_400, 6.281469965398895e-16},
       {2_241_210_444_026_879, 6.311148930905604e-16},
       {2_241_258_926_538_122, 6.34083095820806e-16},
       {2_241_306_038_658_137, 6.370518672608815e-16},
       {2_241_351_806_601_435, 6.400214690888025e-16},
       {2_241_396_255_408_788, 6.429921623054896e-16},
       {2_241_439_408_989_313, 6.459642074078832e-16},
       {2_241_481_290_160_038, 6.489378645603397e-16},
       {2_241_521_920_683_062, 6.519133937646159e-16},
       {2_241_561_321_300_462, 6.548910550287415e-16},
       {2_241_599_511_767_028, 6.578711085350741e-16},
       {2_241_636_510_880_960, 6.608538148078259e-16},
       {2_241_672_336_512_612, 6.638394348803506e-16},
       {2_241_707_005_631_362, 6.668282304624746e-16},
       {2_241_740_534_330_713, 6.698204641081558e-16},
       {2_241_772_937_851_689, 6.728163993837531e-16},
       {2_241_804_230_604_585, 6.758163010371901e-16},
       {2_241_834_426_189_161, 6.78820435168298e-16},
       {2_241_863_537_413_311, 6.818290694006254e-16},
       {2_241_891_576_310_281, 6.848424730550038e-16},
       {2_241_918_554_154_466, 6.878609173251664e-16},
       {2_241_944_481_475_843, 6.908846754557169e-16},
       {2_241_969_368_073_071, 6.939140229227569e-16},
       {2_241_993_223_025_298, 6.969492376174829e-16},
       {2_242_016_054_702_685, 6.999906000330764e-16},
       {2_242_037_870_775_710, 7.030383934552151e-16},
       {2_242_058_678_223_225, 7.060929041565482e-16},
       {2_242_078_483_339_331, 7.091544215954873e-16},
       {2_242_097_291_739_040, 7.122232386196779e-16},
       {2_242_115_108_362_774, 7.152996516745303e-16},
       {2_242_131_937_479_672, 7.183839610172063e-16},
       {2_242_147_782_689_725, 7.214764709364707e-16},
       {2_242_162_646_924_736, 7.245774899788387e-16},
       {2_242_176_532_448_092, 7.276873311814693e-16},
       {2_242_189_440_853_337, 7.308063123122743e-16},
       {2_242_201_373_061_537, 7.339347561177405e-16},
       {2_242_212_329_317_416, 7.370729905789831e-16},
       {2_242_222_309_184_237, 7.4022134917658e-16},
       {2_242_231_311_537_397, 7.433801711647648e-16},
       {2_242_239_334_556_717, 7.465498018555889e-16},
       {2_242_246_375_717_369, 7.497305929136979e-16},
       {2_242_252_431_779_415, 7.529229026624058e-16},
       {2_242_257_498_775_893, 7.561270964017922e-16},
       {2_242_261_571_999_416, 7.5934354673958895e-16},
       {2_242_264_645_987_196, 7.625726339356756e-16},
       {2_242_266_714_504_453, 7.658147462610487e-16},
       {2_242_267_770_526_109, 7.690702803721919e-16},
       {2_242_267_806_216_711, 7.723396417018299e-16},
       {2_242_266_812_908_462, 7.756232448671174e-16},
       {2_242_264_781_077_289, 7.789215140963852e-16},
       {2_242_261_700_316_818, 7.822348836756411e-16},
       {2_242_257_559_310_145, 7.855637984161084e-16},
       {2_242_252_345_799_276, 7.889087141441755e-16},
       {2_242_246_046_552_082, 7.922700982152271e-16},
       {2_242_238_647_326_615, 7.956484300529366e-16},
       {2_242_230_132_832_625, 7.99044201715713e-16},
       {2_242_220_486_690_076, 8.024579184921259e-16},
       {2_242_209_691_384_458, 8.058900995272657e-16},
       {2_242_197_728_218_684, 8.093412784821501e-16},
       {2_242_184_577_261_310, 8.128120042284501e-16},
       {2_242_170_217_290_819, 8.163028415809877e-16},
       {2_242_154_625_735_679, 8.198143720706533e-16},
       {2_242_137_778_609_839, 8.23347194760605e-16},
       {2_242_119_650_443_327, 8.26901927108847e-16},
       {2_242_100_214_207_556, 8.304792058805374e-16},
       {2_242_079_441_234_906, 8.340796881136629e-16},
       {2_242_057_301_132_135, 8.377040521420222e-16},
       {2_242_033_761_687_079, 8.413529986798028e-16},
       {2_242_008_788_768_107, 8.450272519724097e-16},
       {2_241_982_346_215_682, 8.487275610186155e-16},
       {2_241_954_395_725_356, 8.524547008695596e-16},
       {2_241_924_896_721_443, 8.562094740106233e-16},
       {2_241_893_806_220_517, 8.599927118327665e-16},
       {2_241_861_078_683_830, 8.638052762005259e-16},
       {2_241_826_665_857_598, 8.676480611245582e-16},
       {2_241_790_516_600_041, 8.715219945473698e-16},
       {2_241_752_576_693_881, 8.754280402517175e-16},
       {2_241_712_788_642_916, 8.793671999021043e-16},
       {2_241_671_091_451_078, 8.833405152308408e-16},
       {2_241_627_420_382_235, 8.873490703813135e-16},
       {2_241_581_706_698_773, 8.913939944224086e-16},
       {2_241_533_877_376_767, 8.954764640495068e-16},
       {2_241_483_854_795_281, 8.9959770648911e-16},
       {2_241_431_556_397_035, 9.037590026260118e-16},
       {2_241_376_894_317_345, 9.079616903740068e-16},
       {2_241_319_774_977_817, 9.122071683134846e-16},
       {2_241_260_098_640_860, 9.164968996219135e-16},
       {2_241_197_758_920_538, 9.208324163262308e-16},
       {2_241_132_642_244_704, 9.252153239095693e-16},
       {2_241_064_627_262_652, 9.296473063086417e-16},
       {2_240_993_584_191_742, 9.341301313425265e-16},
       {2_240_919_374_095_536, 9.38665656618666e-16},
       {2_240_841_848_084_890, 9.432558359676707e-16},
       {2_240_760_846_432_232, 9.479027264651738e-16},
       {2_240_676_197_587_784, 9.526084961066279e-16},
       {2_240_587_717_084_782, 9.57375432209745e-16},
       {2_240_495_206_318_753, 9.622059506294838e-16},
       {2_240_398_451_183_567, 9.671026058823054e-16},
       {2_240_297_220_544_165, 9.720681022901626e-16},
       {2_240_191_264_522_612, 9.771053062707209e-16},
       {2_240_080_312_570_155, 9.822172599190541e-16},
       {2_239_964_071_293_331, 9.874071960480671e-16},
       {2_239_842_221_996_530, 9.926785548807976e-16},
       {2_239_714_417_896_699, 9.980350026183645e-16},
       {2_239_580_280_957_725, 1.003480452143618e-15},
       {2_239_439_398_282_193, 1.0090190861637457e-15},
       {2_239_291_317_986_196, 1.0146553831467086e-15},
       {2_239_135_544_468_203, 1.0203941464683124e-15},
       {2_238_971_532_964_979, 1.0262405372613567e-15},
       {2_238_798_683_265_269, 1.0322001115486456e-15},
       {2_238_616_332_424_351, 1.03827886235154e-15},
       {2_238_423_746_288_095, 1.044483267600047e-15},
       {2_238_220_109_591_890, 1.0508203448355195e-15},
       {2_238_004_514_345_216, 1.057297713900989e-15},
       {2_237_775_946_143_212, 1.06392366906768e-15},
       {2_237_533_267_957_822, 1.0707072623632994e-15},
       {2_237_275_200_846_753, 1.0776584002668106e-15},
       {2_237_000_300_869_952, 1.0847879564403425e-15},
       {2_236_706_931_309_099, 1.0921079038149563e-15},
       {2_236_393_229_029_147, 1.0996314701785628e-15},
       {2_236_057_063_479_501, 1.1073733224935752e-15},
       {2_235_695_986_373_246, 1.1153497865853155e-15},
       {2_235_307_169_458_859, 1.1235791107110833e-15},
       {2_234_887_326_941_578, 1.1320817840164846e-15},
       {2_234_432_617_919_447, 1.140880924258278e-15},
       {2_233_938_522_519_765, 1.1500027537839792e-15},
       {2_233_399_683_022_677, 1.159477189144919e-15},
       {2_232_809_697_779_198, 1.169338578691096e-15},
       {2_232_160_850_599_817, 1.17962663529558e-15},
       {2_231_443_750_584_641, 1.190387629928289e-15},
       {2_230_646_845_562_170, 1.2016759392543819e-15},
       {2_229_755_753_817_986, 1.2135560818666897e-15},
       {2_228_752_329_126_533, 1.2261054417450561e-15},
       {2_227_613_325_162_504, 1.2394179789163251e-15},
       {2_226_308_442_121_174, 1.2536093926602567e-15},
       {2_224_797_391_720_399, 1.268824481425501e-15},
       {2_223_025_347_823_832, 1.2852479319096109e-15},
       {2_220_915_633_329_809, 1.3031206634689985e-15},
       {2_218_357_446_087_030, 1.3227655770195326e-15},
       {2_215_184_158_448_668, 1.3446300925011171e-15},
       {2_211_132_412_537_369, 1.3693606835128518e-15},
       {2_205_758_503_851_065, 1.397943667277524e-15},
       {2_198_248_265_654_987, 1.4319989869661328e-15},
       {2_186_916_352_102_141, 1.4744848603597596e-15},
       {2_167_562_552_481_814, 1.5317872741611144e-15},
       {2_125_549_880_839_716, 1.6227698675312968e-15}}
    )
  end

  defp normal_fi(indx) do
    :erlang.element(
      indx,
      {1.0, 0.9771017012676708, 0.959879091800106, 0.9451989534422991, 0.9320600759592299,
       0.9199915050393465, 0.9087264400521303, 0.898095921898343, 0.8879846607558328,
       0.8783096558089168, 0.8690086880368565, 0.8600336211963311, 0.8513462584586775,
       0.8429156531122037, 0.834716292986883, 0.8267268339462209, 0.8189291916037019,
       0.8113078743126557, 0.8038494831709638, 0.7965423304229584, 0.789376143566024,
       0.782341832654802, 0.7754313049811866, 0.7686373157984857, 0.7619533468367948,
       0.7553735065070957, 0.7488924472191564, 0.7425052963401506, 0.7362075981268621,
       0.7299952645614757, 0.7238645334686297, 0.7178119326307215, 0.711834248878248,
       0.7059285013327538, 0.7000919181365112, 0.6943219161261163, 0.6886160830046714,
       0.6829721616449943, 0.6773880362187731, 0.6718617198970817, 0.6663913439087498,
       0.6609751477766628, 0.6556114705796969, 0.6502987431108164, 0.645035480820822,
       0.6398202774530561, 0.6346517992876233, 0.6295287799248362, 0.6244500155470261,
       0.619414360605834, 0.6144207238889134, 0.6094680649257731, 0.6045553906974673,
       0.5996817526191248, 0.5948462437679869, 0.5900479963328255, 0.5852861792633709,
       0.5805599961007903, 0.5758686829723532, 0.5712115067352527, 0.5665877632561639,
       0.5619967758145239, 0.5574378936187655, 0.5529104904258318, 0.5484139632552654,
       0.5439477311900258, 0.5395112342569516, 0.5351039323804572, 0.5307253044036615,
       0.526374847171684, 0.5220520746723214, 0.5177565172297559, 0.5134877207473265,
       0.5092452459957476, 0.5050286679434679, 0.5008375751261483, 0.4966715690524893,
       0.49253026364386815, 0.4884132847054576, 0.4843202694266829, 0.4802508659090464,
       0.4762047327195055, 0.47218153846772976, 0.4681809614056932, 0.4642026890481739,
       0.4602464178128425, 0.4563118526787161, 0.45239870686184824, 0.44850670150720273,
       0.4446355653957391, 0.44078503466580377, 0.43695485254798533, 0.4331447691126521,
       0.42935454102944126, 0.4255839313380218, 0.42183270922949573, 0.41810064983784795,
       0.4143875340408909, 0.410693148270188, 0.40701728432947315, 0.4033597392211143,
       0.399720314980197, 0.39609881851583223, 0.3924950614593154, 0.38890886001878855,
       0.38534003484007706, 0.38178841087339344, 0.37825381724561896, 0.37473608713789086,
       0.3712350576682392, 0.36775056977903225, 0.3642824681290037, 0.36083060098964775,
       0.3573948201457802, 0.35397498080007656, 0.3505709414814059, 0.3471825639567935,
       0.34380971314685055, 0.34045225704452164, 0.3371100666370059, 0.33378301583071823,
       0.3304709813791634, 0.3271738428136013, 0.32389148237639104, 0.3206237849569053,
       0.3173706380299135, 0.31413193159633707, 0.31090755812628634, 0.3076974125042919,
       0.3045013919766498, 0.3013193961008029, 0.2981513266966853, 0.29499708779996164,
       0.291856585617095, 0.2887297284821827, 0.2856164268155016, 0.2825165930837074,
       0.2794301417616377, 0.2763569892956681, 0.2732970540685769, 0.2702502563658752,
       0.26721651834356114, 0.2641957639972608, 0.2611879191327208, 0.2581929113376189,
       0.2552106699546617, 0.2522411260559419, 0.24928421241852824, 0.24633986350126363,
       0.24340801542275012, 0.2404886059405004, 0.23758157443123795, 0.2346868618723299,
       0.23180441082433859, 0.22893416541468023, 0.2260760713223802, 0.22323007576391746,
       0.22039612748015194, 0.21757417672433113, 0.21476417525117358, 0.21196607630703015,
       0.209179834621125, 0.20640540639788071, 0.20364274931033485, 0.20089182249465656,
       0.1981525865457751, 0.19542500351413428, 0.19270903690358912, 0.19000465167046496,
       0.18731181422380025, 0.18463049242679927, 0.18196065559952254, 0.17930227452284767,
       0.176655321443735, 0.17401977008183875, 0.17139559563750595, 0.1687827748012115,
       0.16618128576448205, 0.1635911082323657, 0.16101222343751107, 0.1584446141559243,
       0.1558882647244792, 0.15334316106026283, 0.15080929068184568, 0.14828664273257453,
       0.14577520800599403, 0.1432749789735134, 0.1407859498144447, 0.1383081164485507,
       0.13584147657125373, 0.13338602969166913, 0.1309417771736443, 0.12850872227999952,
       0.12608687022018586, 0.12367622820159654, 0.12127680548479021, 0.11888861344290998,
       0.1165116656256108, 0.11414597782783835, 0.111791568163838, 0.10944845714681163,
       0.10711666777468364, 0.1047962256224869, 0.10248715894193508, 0.10018949876880981,
       0.09790327903886228, 0.09562853671300882, 0.09336531191269086, 0.09111364806637363,
       0.08887359206827579, 0.08664519445055796, 0.08442850957035337, 0.08222359581320286,
       0.08003051581466306, 0.07784933670209604, 0.07568013035892707, 0.07352297371398127,
       0.07137794905889037, 0.06924514439700677, 0.0671246538277885, 0.06501657797124284,
       0.06292102443775811, 0.060838108349539864, 0.05876795292093376, 0.0567106901062029,
       0.054666461324888914, 0.052635418276792176, 0.05061772386094776, 0.04861355321586852,
       0.04662309490193037, 0.04464655225129444, 0.04268414491647443, 0.04073611065594093,
       0.03880270740452611, 0.036884215688567284, 0.034980941461716084, 0.03309321945857852,
       0.031221417191920245, 0.029365939758133314, 0.027527235669603082, 0.025705804008548896,
       0.023902203305795882, 0.022117062707308864, 0.020351096230044517, 0.018605121275724643,
       0.016880083152543166, 0.015177088307935325, 0.01349745060173988, 0.011842757857907888,
       0.010214971439701471, 0.008616582769398732, 0.007050875471373227, 0.005522403299250997,
       0.0040379725933630305, 0.0026090727461021627, 0.0012602859304985975}
    )
  end

  def bc64(v) do
    bc(v, 1 <<< (64 - 1), 64)
  end

  defp bc(v, b, n) when b <= v do
    n
  end

  defp bc(v, b, n) do
    bc(v, b >>> 1, n - 1)
  end

  def make_float(s, e, m) do
    <<f::float>> = <<s::size(1), e::size(11), m::size(52)>>
    f
  end

  def float2str(n) do
    <<s::size(1), e::size(11), m::size(52)>> = <<:erlang.float(n)::float>>

    :lists.flatten(
      :io_lib.format(
        '~c~c.~13.16.0bE~b',
        [
          case s do
            1 ->
              ?-

            0 ->
              ?+
          end,
          case e do
            0 ->
              ?0

            _ ->
              ?1
          end,
          m,
          e - 1023
        ]
      )
    )
  end
end
