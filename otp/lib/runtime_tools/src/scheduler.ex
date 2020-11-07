defmodule :m_scheduler do
  use Bitwise

  def sample() do
    sample(:scheduler_wall_time)
  end

  def sample_all() do
    sample(:scheduler_wall_time_all)
  end

  defp sample(stats) do
    case :erlang.statistics(stats) do
      :undefined ->
        :erlang.system_flag(:scheduler_wall_time, true)
        sample(stats)

      list ->
        sorted = :lists.sort(list)

        tagged =
          :lists.map(
            fn {i, a, t} ->
              {sched_tag(i), i, a, t}
            end,
            sorted
          )

        {stats, tagged}
    end
  end

  def utilization(seconds)
      when is_integer(seconds) and
             seconds > 0 do
    oldFlag =
      :erlang.system_flag(
        :scheduler_wall_time,
        true
      )

    t0 = sample()

    receive do
    after
      seconds * 1000 ->
        :ok
    end

    t1 = sample()

    case oldFlag do
      false ->
        :erlang.system_flag(:scheduler_wall_time, oldFlag)

      true ->
        :ok
    end

    utilization(t0, t1)
  end

  def utilization({stats, _} = t0)
      when stats === :scheduler_wall_time or
             stats === :scheduler_wall_time_all do
    utilization(t0, sample(stats))
  end

  def utilization({stats, ts0}, {stats, ts1}) do
    diffs =
      :lists.map(
        fn {{tag, i, a0, t0}, {tag, i, a1, t1}} ->
          {tag, i, a1 - a0, t1 - t0}
        end,
        :lists.zip(ts0, ts1)
      )

    {lst0, {a, t, n}} =
      :lists.foldl(
        fn {tag, i, adiff, tdiff}, {lst, acc} ->
          r = safe_div(adiff, tdiff)
          {[{tag, i, r, percent(r)} | lst], acc(tag, adiff, tdiff, acc)}
        end,
        {[], {0, 0, 0}},
        diffs
      )

    total = safe_div(a, t)
    lst1 = :lists.reverse(lst0)

    lst2 =
      case :erlang.system_info(:logical_processors_available) do
        :unknown ->
          lst1

        lPA ->
          weighted = total * (n / lPA)
          [{:weighted, weighted, percent(weighted)} | lst1]
      end

    [{:total, total, percent(total)} | lst2]
  end

  def utilization(
        {:scheduler_wall_time, _} = t0,
        {:scheduler_wall_time_all, ts1}
      ) do
    utilization(t0, {:scheduler_wall_time, remove_io(ts1)})
  end

  def utilization(
        {:scheduler_wall_time_all, ts0},
        {:scheduler_wall_time, _} = t1
      ) do
    utilization({:scheduler_wall_time, remove_io(ts0)}, t1)
  end

  defp acc(:io, _, _, acc) do
    acc
  end

  defp acc(tag, adiff, tdiff, {asum, tsum, n})
       when tag === :normal or tag === :cpu do
    {adiff + asum, tdiff + tsum, n + 1}
  end

  defp remove_io(ts) do
    :lists.filter(
      fn
        {:io, _, _, _} ->
          false

        _ ->
          true
      end,
      ts
    )
  end

  defp safe_div(a, b) do
    cond do
      b == 0.0 ->
        0.0

      true ->
        a / b
    end
  end

  defp sched_tag(nr) do
    normal = :erlang.system_info(:schedulers)
    cpu = normal + :erlang.system_info(:dirty_cpu_schedulers)

    case nr do
      _ when nr <= normal ->
        :normal

      _ when nr <= cpu ->
        :cpu

      _ ->
        :io
    end
  end

  defp percent(f) do
    :erlang.float_to_list(f * 100, [{:decimals, 1}]) ++ [?%]
  end
end
