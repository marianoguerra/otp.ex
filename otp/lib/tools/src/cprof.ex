defmodule :m_cprof do
  use Bitwise

  def start() do
    tr({:_, :_, :_}, true) + tr(:on_load, true)
  end

  def start({_, _, _} = mFA) do
    tr(mFA, true)
  end

  def start({funcSpec}) do
    tr(funcSpec, true)
  end

  def start(m) do
    tr({m, :_, :_}, true)
  end

  def start(m, f) do
    tr({m, f, :_}, true)
  end

  def start(m, f, a) do
    tr({m, f, a}, true)
  end

  def stop() do
    tr({:_, :_, :_}, false) + tr(:on_load, false)
  end

  def stop({_, _, _} = mFA) do
    tr(mFA, false)
  end

  def stop({funcSpec}) do
    tr(funcSpec, false)
  end

  def stop(m) do
    tr({m, :_, :_}, false)
  end

  def stop(m, f) do
    tr({m, f, :_}, false)
  end

  def stop(m, f, a) do
    tr({m, f, a}, false)
  end

  def restart() do
    tr({:_, :_, :_}, :restart)
  end

  def restart({_, _, _} = mFA) do
    tr(mFA, :restart)
  end

  def restart({funcSpec}) do
    tr(funcSpec, :restart)
  end

  def restart(m) do
    tr({m, :_, :_}, :restart)
  end

  def restart(m, f) do
    tr({m, f, :_}, :restart)
  end

  def restart(m, f, a) do
    tr({m, f, a}, :restart)
  end

  def pause() do
    tr({:_, :_, :_}, :pause) + tr(:on_load, false)
  end

  def pause({_, _, _} = mFA) do
    tr(mFA, :pause)
  end

  def pause({funcSpec}) do
    tr(funcSpec, :pause)
  end

  def pause(m) do
    tr({m, :_, :_}, :pause)
  end

  def pause(m, f) do
    tr({m, f, :_}, :pause)
  end

  def pause(m, f, a) do
    tr({m, f, a}, :pause)
  end

  def analyse() do
    analyse(1)
  end

  def analyse(limit) when is_integer(limit) do
    l0 =
      for mod <- :code.all_loaded() do
        analyse(:erlang.element(1, mod), limit)
      end

    l1 =
      for {m, c, lm} <- l0, c > 0, m !== :cprof do
        {c, m, lm}
      end

    n =
      :lists.foldl(
        fn {c, _, _}, q ->
          q + c
        end,
        0,
        l1
      )

    l =
      for {c, m, lm} <- :lists.reverse(:lists.sort(l1)) do
        {m, c, lm}
      end

    {n, l}
  end

  def analyse(m) when is_atom(m) do
    analyse(m, 1)
  end

  def analyse(m, limit)
      when is_atom(m) and
             is_integer(limit) do
    l0 =
      for {f, a} <- m.module_info(:functions) do
        mFA = {m, f, a}
        {_, c} = :erlang.trace_info(mFA, :call_count)
        [c | mFA]
      end

    l1 =
      for [c | _] = x <- l0, is_integer(c) do
        x
      end

    n =
      :lists.foldl(
        fn [c | _], q ->
          q + c
        end,
        0,
        l1
      )

    l2 =
      for [c | _] = x <- l1, c >= limit do
        x
      end

    l =
      for [c | mFA] <- :lists.reverse(:lists.sort(l2)) do
        {mFA, c}
      end

    {m, n, l}
  end

  def analyze() do
    analyse()
  end

  def analyze(x) do
    analyse(x)
  end

  def analyze(x, y) do
    analyse(x, y)
  end

  defp tr(funcSpec, state) do
    :erlang.trace_pattern(funcSpec, state, [:call_count])
  end
end
