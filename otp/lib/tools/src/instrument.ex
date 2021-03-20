defmodule :m_instrument do
  use Bitwise

  def allocations() do
    allocations(%{})
  end

  def allocations(options) do
    ref = make_ref()

    defaults = %{
      scheduler_ids: :lists.seq(0, :erlang.system_info(:schedulers)),
      allocator_types: :erlang.system_info(:alloc_util_allocators),
      histogram_start: 128,
      histogram_width: 18
    }

    {histStart, msgCount} =
      dispatch_gather(
        :maps.merge(
          defaults,
          options
        ),
        ref,
        &:erts_internal.gather_alloc_histograms/1
      )

    alloc_hist_receive(histStart, msgCount, ref)
  end

  defp alloc_hist_receive(_HistStart, 0, _Ref) do
    {:error, :not_enabled}
  end

  defp alloc_hist_receive(histStart, msgCount, ref) when msgCount > 0 do
    {unscanned, histograms} = alloc_hist_receive_1(msgCount, ref, 0, %{})
    {:ok, {histStart, unscanned, histograms}}
  end

  defp alloc_hist_receive_1(0, _Ref, unscanned, result) do
    {unscanned, result}
  end

  defp alloc_hist_receive_1(msgCount, ref, unscanned0, result0) do
    receive do
      {^ref, unscanned, tags} ->
        result = :lists.foldl(&alloc_hist_fold_result/2, result0, tags)
        alloc_hist_receive_1(msgCount - 1, ref, unscanned0 + unscanned, result)
    end
  end

  defp alloc_hist_fold_result({id, type, blockHist}, result0) do
    idAllocs0 = :maps.get(id, result0, %{})

    mergedHists =
      case :maps.find(type, idAllocs0) do
        {:ok, prevHist} ->
          alloc_hist_merge_hist(tuple_size(blockHist), blockHist, prevHist)

        :error ->
          blockHist
      end

    idAllocs = Map.put(idAllocs0, type, mergedHists)
    Map.put(result0, id, idAllocs)
  end

  defp alloc_hist_merge_hist(0, a, _B) do
    a
  end

  defp alloc_hist_merge_hist(index, a, b) do
    merged =
      :erlang.setelement(
        index,
        a,
        :erlang.element(
          index,
          a
        ) +
          :erlang.element(
            index,
            b
          )
      )

    alloc_hist_merge_hist(index - 1, merged, b)
  end

  def carriers() do
    carriers(%{})
  end

  def carriers(options) do
    ref = make_ref()

    defaults = %{
      scheduler_ids: :lists.seq(0, :erlang.system_info(:schedulers)),
      allocator_types: :erlang.system_info(:alloc_util_allocators),
      histogram_start: 512,
      histogram_width: 14
    }

    {histStart, msgCount} =
      dispatch_gather(
        :maps.merge(
          defaults,
          options
        ),
        ref,
        &:erts_internal.gather_carrier_info/1
      )

    carrier_info_receive(histStart, msgCount, ref)
  end

  defp carrier_info_receive(_HistStart, 0, _Ref) do
    {:error, :not_enabled}
  end

  defp carrier_info_receive(histStart, msgCount, ref) do
    {:ok, {histStart, carrier_info_receive_1(msgCount, ref, [])}}
  end

  defp carrier_info_receive_1(0, _Ref, result) do
    :lists.flatten(result)
  end

  defp carrier_info_receive_1(msgCount, ref, result0) do
    receive do
      {^ref, carriers} ->
        carrier_info_receive_1(msgCount - 1, ref, [carriers, result0])
    end
  end

  defp dispatch_gather(
         %{
           allocator_types: allocatorTypes,
           scheduler_ids: schedulerIds,
           histogram_start: histStart,
           histogram_width: histWidth
         },
         ref,
         gather
       )
       when is_list(allocatorTypes) and
              is_list(schedulerIds) and histStart >= 1 and
              histStart <= 1 <<< 28 and histWidth >= 1 and
              histWidth <= 32 do
    msgCount =
      :lists.sum(
        for schedId <- schedulerIds,
            allocatorType <- allocatorTypes do
          gather.({allocatorType, schedId, histWidth, histStart, ref})
        end
      )

    {histStart, msgCount}
  end

  defp dispatch_gather(_, _, _) do
    :erlang.error(:badarg)
  end
end
