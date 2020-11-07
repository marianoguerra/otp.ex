defmodule :m_reltool_fgraph do
  use Bitwise
  require Record
  Record.defrecord(:r_fg_e, :fg_e, l: 10.0, k: 10.0)

  Record.defrecord(:r_fg_v, :fg_v,
    p: {0.0, 0.0},
    v: {0.0, 0.0},
    q: 5.0,
    m: 1.0,
    type: :dynamic,
    color: :default,
    resides: :undefined,
    selected: false
  )

  def new() do
    []
  end

  def is_defined(key, _Fg) do
    case :erlang.get(key) do
      :undefined ->
        false

      _ ->
        true
    end
  end

  def get(k, _Fg) do
    case :erlang.get(k) do
      {_, v} ->
        v

      _ ->
        :undefined
    end
  end

  def add(key, value, fg) do
    :erlang.put(key, {key, value})
    [key | fg]
  end

  def set(key, value, fg) do
    :erlang.put(key, {key, value})
    fg
  end

  def size(fg) do
    length(fg)
  end

  def del(key, fg) do
    :erlang.erase(key)
    :lists.delete(key, fg)
  end

  def foreach(fun, fg) do
    :lists.foreach(
      fn key ->
        fun.(:erlang.get(key))
      end,
      fg
    )

    fg
  end

  def map(fun, fg) do
    :lists.foreach(
      fn key ->
        :erlang.put(key, fun.(:erlang.get(key)))
      end,
      fg
    )

    fg
  end

  def foldl(fun, i, fg) do
    :lists.foldl(
      fn key, out ->
        fun.(:erlang.get(key), out)
      end,
      i,
      fg
    )
  end

  def mapfoldl(fun, i, fg) do
    acc =
      :lists.foldl(
        fn key, out ->
          {value, acc} = fun.(:erlang.get(key), out)
          :erlang.put(key, value)
          acc
        end,
        i,
        fg
      )

    {fg, acc}
  end

  def step(vs, es) do
    step(vs, es, {0, 0})
  end

  def step(vs, es, pa) do
    :reltool_fgraph.map(
      fn
        node = {_, r_fg_v(type: :static)} ->
          node

        {key, value = r_fg_v(p: {px, py}, v: {vx, vy}, type: :dynamic)}
        when is_float(px) and is_float(py) and
               is_float(vx) and is_float(vy) ->
          f0 = {0.0, 0.0}
          f1 = coulomb_repulsion(key, value, vs, f0)
          f2 = hooke_attraction(key, value, vs, es, f1)
          f3 = point_attraction(key, value, pa, f2)
          {fx, fy} = f3
          vx1 = (vx + 0.25 * fx) * 0.75
          vy1 = (vy + 0.25 * fy) * 0.75
          px1 = px + 0.25 * vx1
          py1 = py + 0.25 * vy1
          {key, r_fg_v(value, p: {px1, py1}, v: {vx1, vy1})}

        node ->
          node
      end,
      vs
    )
  end

  defp point_attraction(_, r_fg_v(p: p0), pa, {fx, fy})
       when is_float(fx) and is_float(fy) do
    k = 20
    l = 150
    {r, {cx, cy}} = composition(p0, pa)
    f = -k * 0.005 * (r - l)
    {fx + cx * f, fy + cy * f}
  end

  defp coulomb_repulsion(k0, r_fg_v(p: p0, q: q0), vs, {fx0, fy0})
       when is_float(fx0) and is_float(fy0) do
    :reltool_fgraph.foldl(
      fn
        {k1, _}, f when k1 == k0 ->
          f

        {_, r_fg_v(p: p1, q: q1)}, {fx, fy} ->
          {r, {cx, cy}} = composition(p0, p1)
          f = 1.0e3 * (q1 * q0) / (r * r + 0.0001)
          {fx + cx * f, fy + cy * f}

        _, f ->
          f
      end,
      {fx0, fy0},
      vs
    )
  end

  defp hooke_attraction(key0, r_fg_v(p: p0), vs, es, {fx0, fy0})
       when is_float(fx0) and is_float(fy0) do
    :reltool_fgraph.foldl(
      fn
        {{key1, key1}, _}, f ->
          f

        {{key1, key2}, r_fg_e(l: l, k: k)}, {fx, fy}
        when key1 === key0 ->
          r_fg_v(p: p1) = :reltool_fgraph.get(key2, vs)
          {r, {cx, cy}} = composition(p0, p1)
          f = -k * 0.005 * (r - l)
          {fx + cx * f, fy + cy * f}

        {{key2, key1}, r_fg_e(l: l, k: k)}, {fx, fy}
        when key1 === key0 ->
          r_fg_v(p: p1) = :reltool_fgraph.get(key2, vs)
          {r, {cx, cy}} = composition(p0, p1)
          f = -k * 0.005 * (r - l)
          {fx + cx * f, fy + cy * f}

        _, f ->
          f
      end,
      {fx0, fy0},
      es
    )
  end

  defp composition({px1, py1}, {px0, py0})
       when is_float(px1) and
              is_float(py1) and is_float(px0) and
              is_float(py0) do
    dx = px1 - px0
    dy = py1 - py0
    r = :math.sqrt(dx * dx + dy * dy + 0.001)
    {r, {dx / r, dy / r}}
  end
end
