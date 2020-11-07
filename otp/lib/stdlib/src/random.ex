defmodule :m_random do
  use Bitwise

  def seed0() do
    {3172, 9814, 20125}
  end

  def seed() do
    case seed_put(seed0()) do
      :undefined ->
        seed0()

      {_, _, _} = tuple ->
        tuple
    end
  end

  def seed(int) when is_integer(int) do
    a1 = int >>> 16 &&& 268_435_455
    a2 = int &&& 16_777_215
    a3 = int >>> 36 ||| a2 >>> 16
    seed(a1, a2, a3)
  end

  def seed({a1, a2, a3}) do
    seed(a1, a2, a3)
  end

  def seed(a1, a2, a3) do
    seed_put(
      {rem(abs(a1), 30269 - 1) + 1, rem(abs(a2), 30307 - 1) + 1, rem(abs(a3), 30323 - 1) + 1}
    )
  end

  defp seed_put(seed) do
    :erlang.put(:random_seed, seed)
  end

  def uniform() do
    {a1, a2, a3} =
      case :erlang.get(:random_seed) do
        :undefined ->
          seed0()

        tuple ->
          tuple
      end

    b1 = rem(a1 * 171, 30269)
    b2 = rem(a2 * 172, 30307)
    b3 = rem(a3 * 170, 30323)
    :erlang.put(:random_seed, {b1, b2, b3})
    r = b1 / 30269 + b2 / 30307 + b3 / 30323
    r - trunc(r)
  end

  def uniform(n) when is_integer(n) and n >= 1 do
    trunc(uniform() * n) + 1
  end

  def uniform_s({a1, a2, a3}) do
    b1 = rem(a1 * 171, 30269)
    b2 = rem(a2 * 172, 30307)
    b3 = rem(a3 * 170, 30323)
    r = b1 / 30269 + b2 / 30307 + b3 / 30323
    {r - trunc(r), {b1, b2, b3}}
  end

  def uniform_s(n, state0) when is_integer(n) and n >= 1 do
    {f, state1} = uniform_s(state0)
    {trunc(f * n) + 1, state1}
  end
end
